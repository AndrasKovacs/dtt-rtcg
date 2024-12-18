"use strict";
var __getOwnPropNames = Object.getOwnPropertyNames;
var __commonJS = (cb, mod) => function __require() {
  return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
};

// node_modules/readline-sync/lib/readline-sync.js
var require_readline_sync = __commonJS({
  "node_modules/readline-sync/lib/readline-sync.js"(exports2) {
    "use strict";
    var IS_WIN = process.platform === "win32";
    var ALGORITHM_CIPHER = "aes-256-cbc";
    var ALGORITHM_HASH = "sha256";
    var DEFAULT_ERR_MSG = "The current environment doesn't support interactive reading from TTY.";
    var fs = require("fs");
    var TTY = process.binding("tty_wrap").TTY;
    var childProc = require("child_process");
    var pathUtil = require("path");
    var defaultOptions = {
      /* eslint-disable key-spacing */
      prompt: "> ",
      hideEchoBack: false,
      mask: "*",
      limit: [],
      limitMessage: "Input another, please.$<( [)limit(])>",
      defaultInput: "",
      trueValue: [],
      falseValue: [],
      caseSensitive: false,
      keepWhitespace: false,
      encoding: "utf8",
      bufferSize: 1024,
      print: void 0,
      history: true,
      cd: false,
      phContent: void 0,
      preCheck: void 0
      /* eslint-enable key-spacing */
    };
    var fdR = "none";
    var isRawMode = false;
    var salt = 0;
    var lastInput = "";
    var inputHistory = [];
    var _DBG_useExt = false;
    var _DBG_checkOptions = false;
    var _DBG_checkMethod = false;
    var fdW;
    var ttyR;
    var extHostPath;
    var extHostArgs;
    var tempdir;
    var rawInput;
    function getHostArgs(options) {
      function encodeArg(arg) {
        return arg.replace(/[^\w\u0080-\uFFFF]/g, function(chr) {
          return "#" + chr.charCodeAt(0) + ";";
        });
      }
      return extHostArgs.concat(function(conf) {
        var args = [];
        Object.keys(conf).forEach(function(optionName) {
          if (conf[optionName] === "boolean") {
            if (options[optionName]) {
              args.push("--" + optionName);
            }
          } else if (conf[optionName] === "string") {
            if (options[optionName]) {
              args.push("--" + optionName, encodeArg(options[optionName]));
            }
          }
        });
        return args;
      }({
        /* eslint-disable key-spacing */
        display: "string",
        displayOnly: "boolean",
        keyIn: "boolean",
        hideEchoBack: "boolean",
        mask: "string",
        limit: "string",
        caseSensitive: "boolean"
        /* eslint-enable key-spacing */
      }));
    }
    function _execFileSync(options, execOptions) {
      function getTempfile(name) {
        var suffix = "", filepath, fd;
        tempdir = tempdir || require("os").tmpdir();
        while (true) {
          filepath = pathUtil.join(tempdir, name + suffix);
          try {
            fd = fs.openSync(filepath, "wx");
          } catch (e) {
            if (e.code === "EEXIST") {
              suffix++;
              continue;
            } else {
              throw e;
            }
          }
          fs.closeSync(fd);
          break;
        }
        return filepath;
      }
      var res = {}, pathStdout = getTempfile("readline-sync.stdout"), pathStderr = getTempfile("readline-sync.stderr"), pathExit = getTempfile("readline-sync.exit"), pathDone = getTempfile("readline-sync.done"), crypto = require("crypto"), hostArgs, shellPath, shellArgs, exitCode, extMessage, shasum, decipher, password;
      shasum = crypto.createHash(ALGORITHM_HASH);
      shasum.update("" + process.pid + salt++ + Math.random());
      password = shasum.digest("hex");
      decipher = crypto.createDecipher(ALGORITHM_CIPHER, password);
      hostArgs = getHostArgs(options);
      if (IS_WIN) {
        shellPath = process.env.ComSpec || "cmd.exe";
        process.env.Q = '"';
        shellArgs = [
          "/V:ON",
          "/S",
          "/C",
          "(%Q%" + shellPath + "%Q% /V:ON /S /C %Q%%Q%" + extHostPath + "%Q%" + hostArgs.map(function(arg) {
            return " %Q%" + arg + "%Q%";
          }).join("") + " & (echo !ERRORLEVEL!)>%Q%" + pathExit + "%Q%%Q%) 2>%Q%" + pathStderr + "%Q% |%Q%" + process.execPath + "%Q% %Q%" + __dirname + "\\encrypt.js%Q% %Q%" + ALGORITHM_CIPHER + "%Q% %Q%" + password + "%Q% >%Q%" + pathStdout + "%Q% & (echo 1)>%Q%" + pathDone + "%Q%"
        ];
      } else {
        shellPath = "/bin/sh";
        shellArgs = [
          "-c",
          // Use `()`, not `{}` for `-c` (text param)
          '("' + extHostPath + '"' + /* ESLint bug? */
          // eslint-disable-line no-path-concat
          hostArgs.map(function(arg) {
            return " '" + arg.replace(/'/g, "'\\''") + "'";
          }).join("") + '; echo $?>"' + pathExit + '") 2>"' + pathStderr + '" |"' + process.execPath + '" "' + __dirname + '/encrypt.js" "' + ALGORITHM_CIPHER + '" "' + password + '" >"' + pathStdout + '"; echo 1 >"' + pathDone + '"'
        ];
      }
      if (_DBG_checkMethod) {
        _DBG_checkMethod("_execFileSync", hostArgs);
      }
      try {
        childProc.spawn(shellPath, shellArgs, execOptions);
      } catch (e) {
        res.error = new Error(e.message);
        res.error.method = "_execFileSync - spawn";
        res.error.program = shellPath;
        res.error.args = shellArgs;
      }
      while (fs.readFileSync(pathDone, { encoding: options.encoding }).trim() !== "1") {
      }
      if ((exitCode = fs.readFileSync(pathExit, { encoding: options.encoding }).trim()) === "0") {
        res.input = decipher.update(
          fs.readFileSync(pathStdout, { encoding: "binary" }),
          "hex",
          options.encoding
        ) + decipher.final(options.encoding);
      } else {
        extMessage = fs.readFileSync(pathStderr, { encoding: options.encoding }).trim();
        res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? "\n" + extMessage : ""));
        res.error.method = "_execFileSync";
        res.error.program = shellPath;
        res.error.args = shellArgs;
        res.error.extMessage = extMessage;
        res.error.exitCode = +exitCode;
      }
      fs.unlinkSync(pathStdout);
      fs.unlinkSync(pathStderr);
      fs.unlinkSync(pathExit);
      fs.unlinkSync(pathDone);
      return res;
    }
    function readlineExt(options) {
      var res = {}, execOptions = { env: process.env, encoding: options.encoding }, hostArgs, extMessage;
      if (!extHostPath) {
        if (IS_WIN) {
          if (process.env.PSModulePath) {
            extHostPath = "powershell.exe";
            extHostArgs = [
              "-ExecutionPolicy",
              "Bypass",
              "-File",
              __dirname + "\\read.ps1"
            ];
          } else {
            extHostPath = "cscript.exe";
            extHostArgs = ["//nologo", __dirname + "\\read.cs.js"];
          }
        } else {
          extHostPath = "/bin/sh";
          extHostArgs = [__dirname + "/read.sh"];
        }
      }
      if (IS_WIN && !process.env.PSModulePath) {
        execOptions.stdio = [process.stdin];
      }
      if (childProc.execFileSync) {
        hostArgs = getHostArgs(options);
        if (_DBG_checkMethod) {
          _DBG_checkMethod("execFileSync", hostArgs);
        }
        try {
          res.input = childProc.execFileSync(extHostPath, hostArgs, execOptions);
        } catch (e) {
          extMessage = e.stderr ? (e.stderr + "").trim() : "";
          res.error = new Error(DEFAULT_ERR_MSG + (extMessage ? "\n" + extMessage : ""));
          res.error.method = "execFileSync";
          res.error.program = extHostPath;
          res.error.args = hostArgs;
          res.error.extMessage = extMessage;
          res.error.exitCode = e.status;
          res.error.code = e.code;
          res.error.signal = e.signal;
        }
      } else {
        res = _execFileSync(options, execOptions);
      }
      if (!res.error) {
        res.input = res.input.replace(/^\s*'|'\s*$/g, "");
        options.display = "";
      }
      return res;
    }
    function _readlineSync(options) {
      var input = "", displaySave = options.display, silent = !options.display && options.keyIn && options.hideEchoBack && !options.mask;
      function tryExt() {
        var res = readlineExt(options);
        if (res.error) {
          throw res.error;
        }
        return res.input;
      }
      if (_DBG_checkOptions) {
        _DBG_checkOptions(options);
      }
      (function() {
        var fsB, constants, verNum;
        function getFsB() {
          if (!fsB) {
            fsB = process.binding("fs");
            constants = process.binding("constants");
            constants = constants && constants.fs && typeof constants.fs.O_RDWR === "number" ? constants.fs : constants;
          }
          return fsB;
        }
        if (typeof fdR !== "string") {
          return;
        }
        fdR = null;
        if (IS_WIN) {
          verNum = function(ver) {
            var nums = ver.replace(/^\D+/, "").split(".");
            var verNum2 = 0;
            if (nums[0] = +nums[0]) {
              verNum2 += nums[0] * 1e4;
            }
            if (nums[1] = +nums[1]) {
              verNum2 += nums[1] * 100;
            }
            if (nums[2] = +nums[2]) {
              verNum2 += nums[2];
            }
            return verNum2;
          }(process.version);
          if (!(verNum >= 20302 && verNum < 40204 || verNum >= 5e4 && verNum < 50100 || verNum >= 50600 && verNum < 60200) && process.stdin.isTTY) {
            process.stdin.pause();
            fdR = process.stdin.fd;
            ttyR = process.stdin._handle;
          } else {
            try {
              fdR = getFsB().open("CONIN$", constants.O_RDWR, parseInt("0666", 8));
              ttyR = new TTY(fdR, true);
            } catch (e) {
            }
          }
          if (process.stdout.isTTY) {
            fdW = process.stdout.fd;
          } else {
            try {
              fdW = fs.openSync("\\\\.\\CON", "w");
            } catch (e) {
            }
            if (typeof fdW !== "number") {
              try {
                fdW = getFsB().open("CONOUT$", constants.O_RDWR, parseInt("0666", 8));
              } catch (e) {
              }
            }
          }
        } else {
          if (process.stdin.isTTY) {
            process.stdin.pause();
            try {
              fdR = fs.openSync("/dev/tty", "r");
              ttyR = process.stdin._handle;
            } catch (e) {
            }
          } else {
            try {
              fdR = fs.openSync("/dev/tty", "r");
              ttyR = new TTY(fdR, false);
            } catch (e) {
            }
          }
          if (process.stdout.isTTY) {
            fdW = process.stdout.fd;
          } else {
            try {
              fdW = fs.openSync("/dev/tty", "w");
            } catch (e) {
            }
          }
        }
      })();
      (function() {
        var isCooked = !options.hideEchoBack && !options.keyIn, atEol, limit, buffer, reqSize, readSize, chunk, line;
        rawInput = "";
        function setRawMode(mode) {
          if (mode === isRawMode) {
            return true;
          }
          if (ttyR.setRawMode(mode) !== 0) {
            return false;
          }
          isRawMode = mode;
          return true;
        }
        if (_DBG_useExt || !ttyR || typeof fdW !== "number" && (options.display || !isCooked)) {
          input = tryExt();
          return;
        }
        if (options.display) {
          fs.writeSync(fdW, options.display);
          options.display = "";
        }
        if (options.displayOnly) {
          return;
        }
        if (!setRawMode(!isCooked)) {
          input = tryExt();
          return;
        }
        reqSize = options.keyIn ? 1 : options.bufferSize;
        buffer = Buffer.allocUnsafe && Buffer.alloc ? Buffer.alloc(reqSize) : new Buffer(reqSize);
        if (options.keyIn && options.limit) {
          limit = new RegExp(
            "[^" + options.limit + "]",
            "g" + (options.caseSensitive ? "" : "i")
          );
        }
        while (true) {
          readSize = 0;
          try {
            readSize = fs.readSync(fdR, buffer, 0, reqSize);
          } catch (e) {
            if (e.code !== "EOF") {
              setRawMode(false);
              input += tryExt();
              return;
            }
          }
          if (readSize > 0) {
            chunk = buffer.toString(options.encoding, 0, readSize);
            rawInput += chunk;
          } else {
            chunk = "\n";
            rawInput += String.fromCharCode(0);
          }
          if (chunk && typeof (line = (chunk.match(/^(.*?)[\r\n]/) || [])[1]) === "string") {
            chunk = line;
            atEol = true;
          }
          if (chunk) {
            chunk = chunk.replace(/[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]/g, "");
          }
          if (chunk && limit) {
            chunk = chunk.replace(limit, "");
          }
          if (chunk) {
            if (!isCooked) {
              if (!options.hideEchoBack) {
                fs.writeSync(fdW, chunk);
              } else if (options.mask) {
                fs.writeSync(fdW, new Array(chunk.length + 1).join(options.mask));
              }
            }
            input += chunk;
          }
          if (!options.keyIn && atEol || options.keyIn && input.length >= reqSize) {
            break;
          }
        }
        if (!isCooked && !silent) {
          fs.writeSync(fdW, "\n");
        }
        setRawMode(false);
      })();
      if (options.print && !silent) {
        options.print(
          displaySave + (options.displayOnly ? "" : (options.hideEchoBack ? new Array(input.length + 1).join(options.mask) : input) + "\n"),
          options.encoding
        );
      }
      return options.displayOnly ? "" : lastInput = options.keepWhitespace || options.keyIn ? input : input.trim();
    }
    function flattenArray(array, validator) {
      var flatArray = [];
      function _flattenArray(array2) {
        if (array2 == null) {
          return;
        }
        if (Array.isArray(array2)) {
          array2.forEach(_flattenArray);
        } else if (!validator || validator(array2)) {
          flatArray.push(array2);
        }
      }
      _flattenArray(array);
      return flatArray;
    }
    function escapePattern(pattern) {
      return pattern.replace(
        /[\x00-\x7f]/g,
        // eslint-disable-line no-control-regex
        function(s) {
          return "\\x" + ("00" + s.charCodeAt().toString(16)).substr(-2);
        }
      );
    }
    function margeOptions() {
      var optionsList = Array.prototype.slice.call(arguments), optionNames, fromDefault;
      if (optionsList.length && typeof optionsList[0] === "boolean") {
        fromDefault = optionsList.shift();
        if (fromDefault) {
          optionNames = Object.keys(defaultOptions);
          optionsList.unshift(defaultOptions);
        }
      }
      return optionsList.reduce(function(options, optionsPart) {
        if (optionsPart == null) {
          return options;
        }
        if (optionsPart.hasOwnProperty("noEchoBack") && !optionsPart.hasOwnProperty("hideEchoBack")) {
          optionsPart.hideEchoBack = optionsPart.noEchoBack;
          delete optionsPart.noEchoBack;
        }
        if (optionsPart.hasOwnProperty("noTrim") && !optionsPart.hasOwnProperty("keepWhitespace")) {
          optionsPart.keepWhitespace = optionsPart.noTrim;
          delete optionsPart.noTrim;
        }
        if (!fromDefault) {
          optionNames = Object.keys(optionsPart);
        }
        optionNames.forEach(function(optionName) {
          var value;
          if (!optionsPart.hasOwnProperty(optionName)) {
            return;
          }
          value = optionsPart[optionName];
          switch (optionName) {
            //                    _readlineSync <- *    * -> defaultOptions
            // ================ string
            case "mask":
            // *    *
            case "limitMessage":
            //      *
            case "defaultInput":
            //      *
            case "encoding":
              value = value != null ? value + "" : "";
              if (value && optionName !== "limitMessage") {
                value = value.replace(/[\r\n]/g, "");
              }
              options[optionName] = value;
              break;
            // ================ number(int)
            case "bufferSize":
              if (!isNaN(value = parseInt(value, 10)) && typeof value === "number") {
                options[optionName] = value;
              }
              break;
            // ================ boolean
            case "displayOnly":
            // *
            case "keyIn":
            // *
            case "hideEchoBack":
            // *    *
            case "caseSensitive":
            // *    *
            case "keepWhitespace":
            // *    *
            case "history":
            //      *
            case "cd":
              options[optionName] = !!value;
              break;
            // ================ array
            case "limit":
            // *    *     to string for readlineExt
            case "trueValue":
            //      *
            case "falseValue":
              options[optionName] = flattenArray(value, function(value2) {
                var type = typeof value2;
                return type === "string" || type === "number" || type === "function" || value2 instanceof RegExp;
              }).map(function(value2) {
                return typeof value2 === "string" ? value2.replace(/[\r\n]/g, "") : value2;
              });
              break;
            // ================ function
            case "print":
            // *    *
            case "phContent":
            //      *
            case "preCheck":
              options[optionName] = typeof value === "function" ? value : void 0;
              break;
            // ================ other
            case "prompt":
            //      *
            case "display":
              options[optionName] = value != null ? value : "";
              break;
          }
        });
        return options;
      }, {});
    }
    function isMatched(res, comps, caseSensitive) {
      return comps.some(function(comp) {
        var type = typeof comp;
        return type === "string" ? caseSensitive ? res === comp : res.toLowerCase() === comp.toLowerCase() : type === "number" ? parseFloat(res) === comp : type === "function" ? comp(res) : comp instanceof RegExp ? comp.test(res) : false;
      });
    }
    function replaceHomePath(path, expand) {
      var homePath = pathUtil.normalize(
        IS_WIN ? (process.env.HOMEDRIVE || "") + (process.env.HOMEPATH || "") : process.env.HOME || ""
      ).replace(/[/\\]+$/, "");
      path = pathUtil.normalize(path);
      return expand ? path.replace(/^~(?=\/|\\|$)/, homePath) : path.replace(new RegExp("^" + escapePattern(homePath) + "(?=\\/|\\\\|$)", IS_WIN ? "i" : ""), "~");
    }
    function replacePlaceholder(text, generator) {
      var PTN_INNER = "(?:\\(([\\s\\S]*?)\\))?(\\w+|.-.)(?:\\(([\\s\\S]*?)\\))?", rePlaceholder = new RegExp("(\\$)?(\\$<" + PTN_INNER + ">)", "g"), rePlaceholderCompat = new RegExp("(\\$)?(\\$\\{" + PTN_INNER + "\\})", "g");
      function getPlaceholderText(s, escape, placeholder, pre, param, post) {
        var text2;
        return escape || typeof (text2 = generator(param)) !== "string" ? placeholder : text2 ? (pre || "") + text2 + (post || "") : "";
      }
      return text.replace(rePlaceholder, getPlaceholderText).replace(rePlaceholderCompat, getPlaceholderText);
    }
    function array2charlist(array, caseSensitive, collectSymbols) {
      var group = [], groupClass = -1, charCode = 0, symbols = "", values, suppressed;
      function addGroup(groups, group2) {
        if (group2.length > 3) {
          groups.push(group2[0] + "..." + group2[group2.length - 1]);
          suppressed = true;
        } else if (group2.length) {
          groups = groups.concat(group2);
        }
        return groups;
      }
      values = array.reduce(function(chars, value) {
        return chars.concat((value + "").split(""));
      }, []).reduce(function(groups, curChar) {
        var curGroupClass, curCharCode;
        if (!caseSensitive) {
          curChar = curChar.toLowerCase();
        }
        curGroupClass = /^\d$/.test(curChar) ? 1 : /^[A-Z]$/.test(curChar) ? 2 : /^[a-z]$/.test(curChar) ? 3 : 0;
        if (collectSymbols && curGroupClass === 0) {
          symbols += curChar;
        } else {
          curCharCode = curChar.charCodeAt(0);
          if (curGroupClass && curGroupClass === groupClass && curCharCode === charCode + 1) {
            group.push(curChar);
          } else {
            groups = addGroup(groups, group);
            group = [curChar];
            groupClass = curGroupClass;
          }
          charCode = curCharCode;
        }
        return groups;
      }, []);
      values = addGroup(values, group);
      if (symbols) {
        values.push(symbols);
        suppressed = true;
      }
      return { values, suppressed };
    }
    function joinChunks(chunks, suppressed) {
      return chunks.join(chunks.length > 2 ? ", " : suppressed ? " / " : "/");
    }
    function getPhContent(param, options) {
      var resCharlist = {}, text, values, arg;
      if (options.phContent) {
        text = options.phContent(param, options);
      }
      if (typeof text !== "string") {
        switch (param) {
          case "hideEchoBack":
          case "mask":
          case "defaultInput":
          case "caseSensitive":
          case "keepWhitespace":
          case "encoding":
          case "bufferSize":
          case "history":
          case "cd":
            text = !options.hasOwnProperty(param) ? "" : typeof options[param] === "boolean" ? options[param] ? "on" : "off" : options[param] + "";
            break;
          // case 'prompt':
          // case 'query':
          // case 'display':
          //   text = options.hasOwnProperty('displaySrc') ? options.displaySrc + '' : '';
          //   break;
          case "limit":
          case "trueValue":
          case "falseValue":
            values = options[options.hasOwnProperty(param + "Src") ? param + "Src" : param];
            if (options.keyIn) {
              resCharlist = array2charlist(values, options.caseSensitive);
              values = resCharlist.values;
            } else {
              values = values.filter(function(value) {
                var type = typeof value;
                return type === "string" || type === "number";
              });
            }
            text = joinChunks(values, resCharlist.suppressed);
            break;
          case "limitCount":
          case "limitCountNotZero":
            text = options[options.hasOwnProperty("limitSrc") ? "limitSrc" : "limit"].length;
            text = text || param !== "limitCountNotZero" ? text + "" : "";
            break;
          case "lastInput":
            text = lastInput;
            break;
          case "cwd":
          case "CWD":
          case "cwdHome":
            text = process.cwd();
            if (param === "CWD") {
              text = pathUtil.basename(text);
            } else if (param === "cwdHome") {
              text = replaceHomePath(text);
            }
            break;
          case "date":
          case "time":
          case "localeDate":
          case "localeTime":
            text = (/* @__PURE__ */ new Date())["to" + param.replace(/^./, function(str) {
              return str.toUpperCase();
            }) + "String"]();
            break;
          default:
            if (typeof (arg = (param.match(/^history_m(\d+)$/) || [])[1]) === "string") {
              text = inputHistory[inputHistory.length - arg] || "";
            }
        }
      }
      return text;
    }
    function getPhCharlist(param) {
      var matches = /^(.)-(.)$/.exec(param), text = "", from, to, code, step;
      if (!matches) {
        return null;
      }
      from = matches[1].charCodeAt(0);
      to = matches[2].charCodeAt(0);
      step = from < to ? 1 : -1;
      for (code = from; code !== to + step; code += step) {
        text += String.fromCharCode(code);
      }
      return text;
    }
    function parseCl(cl) {
      var reToken = new RegExp(/(\s*)(?:("|')(.*?)(?:\2|$)|(\S+))/g), taken = "", args = [], matches, part;
      cl = cl.trim();
      while (matches = reToken.exec(cl)) {
        part = matches[3] || matches[4] || "";
        if (matches[1]) {
          args.push(taken);
          taken = "";
        }
        taken += part;
      }
      if (taken) {
        args.push(taken);
      }
      return args;
    }
    function toBool(res, options) {
      return options.trueValue.length && isMatched(res, options.trueValue, options.caseSensitive) ? true : options.falseValue.length && isMatched(res, options.falseValue, options.caseSensitive) ? false : res;
    }
    function getValidLine(options) {
      var res, forceNext, limitMessage, matches, histInput, args, resCheck;
      function _getPhContent(param) {
        return getPhContent(param, options);
      }
      function addDisplay(text) {
        options.display += (/[^\r\n]$/.test(options.display) ? "\n" : "") + text;
      }
      options.limitSrc = options.limit;
      options.displaySrc = options.display;
      options.limit = "";
      options.display = replacePlaceholder(options.display + "", _getPhContent);
      while (true) {
        res = _readlineSync(options);
        forceNext = false;
        limitMessage = "";
        if (options.defaultInput && !res) {
          res = options.defaultInput;
        }
        if (options.history) {
          if (matches = /^\s*!(?:!|-1)(:p)?\s*$/.exec(res)) {
            histInput = inputHistory[0] || "";
            if (matches[1]) {
              forceNext = true;
            } else {
              res = histInput;
            }
            addDisplay(histInput + "\n");
            if (!forceNext) {
              options.displayOnly = true;
              _readlineSync(options);
              options.displayOnly = false;
            }
          } else if (res && res !== inputHistory[inputHistory.length - 1]) {
            inputHistory = [res];
          }
        }
        if (!forceNext && options.cd && res) {
          args = parseCl(res);
          switch (args[0].toLowerCase()) {
            case "cd":
              if (args[1]) {
                try {
                  process.chdir(replaceHomePath(args[1], true));
                } catch (e) {
                  addDisplay(e + "");
                }
              }
              forceNext = true;
              break;
            case "pwd":
              addDisplay(process.cwd());
              forceNext = true;
              break;
          }
        }
        if (!forceNext && options.preCheck) {
          resCheck = options.preCheck(res, options);
          res = resCheck.res;
          if (resCheck.forceNext) {
            forceNext = true;
          }
        }
        if (!forceNext) {
          if (!options.limitSrc.length || isMatched(res, options.limitSrc, options.caseSensitive)) {
            break;
          }
          if (options.limitMessage) {
            limitMessage = replacePlaceholder(options.limitMessage, _getPhContent);
          }
        }
        addDisplay((limitMessage ? limitMessage + "\n" : "") + replacePlaceholder(options.displaySrc + "", _getPhContent));
      }
      return toBool(res, options);
    }
    exports2._DBG_set_useExt = function(val) {
      _DBG_useExt = val;
    };
    exports2._DBG_set_checkOptions = function(val) {
      _DBG_checkOptions = val;
    };
    exports2._DBG_set_checkMethod = function(val) {
      _DBG_checkMethod = val;
    };
    exports2._DBG_clearHistory = function() {
      lastInput = "";
      inputHistory = [];
    };
    exports2.setDefaultOptions = function(options) {
      defaultOptions = margeOptions(true, options);
      return margeOptions(true);
    };
    exports2.question = function(query, options) {
      return getValidLine(margeOptions(margeOptions(true, options), {
        display: query
      }));
    };
    exports2.prompt = function(options) {
      var readOptions = margeOptions(true, options);
      readOptions.display = readOptions.prompt;
      return getValidLine(readOptions);
    };
    exports2.keyIn = function(query, options) {
      var readOptions = margeOptions(margeOptions(true, options), {
        display: query,
        keyIn: true,
        keepWhitespace: true
      });
      readOptions.limitSrc = readOptions.limit.filter(function(value) {
        var type = typeof value;
        return type === "string" || type === "number";
      }).map(function(text) {
        return replacePlaceholder(text + "", getPhCharlist);
      });
      readOptions.limit = escapePattern(readOptions.limitSrc.join(""));
      ["trueValue", "falseValue"].forEach(function(optionName) {
        readOptions[optionName] = readOptions[optionName].reduce(function(comps, comp) {
          var type = typeof comp;
          if (type === "string" || type === "number") {
            comps = comps.concat((comp + "").split(""));
          } else {
            comps.push(comp);
          }
          return comps;
        }, []);
      });
      readOptions.display = replacePlaceholder(
        readOptions.display + "",
        function(param) {
          return getPhContent(param, readOptions);
        }
      );
      return toBool(_readlineSync(readOptions), readOptions);
    };
    exports2.questionEMail = function(query, options) {
      if (query == null) {
        query = "Input e-mail address: ";
      }
      return exports2.question(query, margeOptions({
        // -------- default
        hideEchoBack: false,
        // http://www.w3.org/TR/html5/forms.html#valid-e-mail-address
        limit: /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/,
        limitMessage: "Input valid e-mail address, please.",
        trueValue: null,
        falseValue: null
      }, options, {
        // -------- forced
        keepWhitespace: false,
        cd: false
      }));
    };
    exports2.questionNewPassword = function(query, options) {
      var resCharlist, min, max, readOptions = margeOptions({
        // -------- default
        hideEchoBack: true,
        mask: "*",
        limitMessage: "It can include: $<charlist>\nAnd the length must be: $<length>",
        trueValue: null,
        falseValue: null,
        caseSensitive: true
      }, options, {
        // -------- forced
        history: false,
        cd: false,
        // limit (by charlist etc.),
        phContent: function(param) {
          return param === "charlist" ? resCharlist.text : param === "length" ? min + "..." + max : null;
        }
      }), charlist, confirmMessage, unmatchMessage, limit, limitMessage, res1, res2;
      options = options || {};
      charlist = replacePlaceholder(
        options.charlist ? options.charlist + "" : "$<!-~>",
        getPhCharlist
      );
      if (isNaN(min = parseInt(options.min, 10)) || typeof min !== "number") {
        min = 12;
      }
      if (isNaN(max = parseInt(options.max, 10)) || typeof max !== "number") {
        max = 24;
      }
      limit = new RegExp("^[" + escapePattern(charlist) + "]{" + min + "," + max + "}$");
      resCharlist = array2charlist([charlist], readOptions.caseSensitive, true);
      resCharlist.text = joinChunks(resCharlist.values, resCharlist.suppressed);
      confirmMessage = options.confirmMessage != null ? options.confirmMessage : "Reinput a same one to confirm it: ";
      unmatchMessage = options.unmatchMessage != null ? options.unmatchMessage : "It differs from first one. Hit only the Enter key if you want to retry from first one.";
      if (query == null) {
        query = "Input new password: ";
      }
      limitMessage = readOptions.limitMessage;
      while (!res2) {
        readOptions.limit = limit;
        readOptions.limitMessage = limitMessage;
        res1 = exports2.question(query, readOptions);
        readOptions.limit = [res1, ""];
        readOptions.limitMessage = unmatchMessage;
        res2 = exports2.question(confirmMessage, readOptions);
      }
      return res1;
    };
    function _questionNum(query, options, parser) {
      var validValue;
      function getValidValue(value) {
        validValue = parser(value);
        return !isNaN(validValue) && typeof validValue === "number";
      }
      exports2.question(query, margeOptions({
        // -------- default
        limitMessage: "Input valid number, please."
      }, options, {
        // -------- forced
        limit: getValidValue,
        cd: false
        // trueValue, falseValue, caseSensitive, keepWhitespace don't work.
      }));
      return validValue;
    }
    exports2.questionInt = function(query, options) {
      return _questionNum(query, options, function(value) {
        return parseInt(value, 10);
      });
    };
    exports2.questionFloat = function(query, options) {
      return _questionNum(query, options, parseFloat);
    };
    exports2.questionPath = function(query, options) {
      var error = "", validPath, readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "$<error(\n)>Input valid path, please.$<( Min:)min>$<( Max:)max>",
        history: true,
        cd: true
      }, options, {
        // -------- forced
        keepWhitespace: false,
        limit: function(value) {
          var exists, stat, res;
          value = replaceHomePath(value, true);
          error = "";
          function mkdirParents(dirPath) {
            dirPath.split(/\/|\\/).reduce(function(parents, dir) {
              var path = pathUtil.resolve(parents += dir + pathUtil.sep);
              if (!fs.existsSync(path)) {
                fs.mkdirSync(path);
              } else if (!fs.statSync(path).isDirectory()) {
                throw new Error("Non directory already exists: " + path);
              }
              return parents;
            }, "");
          }
          try {
            exists = fs.existsSync(value);
            validPath = exists ? fs.realpathSync(value) : pathUtil.resolve(value);
            if (!options.hasOwnProperty("exists") && !exists || typeof options.exists === "boolean" && options.exists !== exists) {
              error = (exists ? "Already exists" : "No such file or directory") + ": " + validPath;
              return false;
            }
            if (!exists && options.create) {
              if (options.isDirectory) {
                mkdirParents(validPath);
              } else {
                mkdirParents(pathUtil.dirname(validPath));
                fs.closeSync(fs.openSync(validPath, "w"));
              }
              validPath = fs.realpathSync(validPath);
            }
            if (exists && (options.min || options.max || options.isFile || options.isDirectory)) {
              stat = fs.statSync(validPath);
              if (options.isFile && !stat.isFile()) {
                error = "Not file: " + validPath;
                return false;
              } else if (options.isDirectory && !stat.isDirectory()) {
                error = "Not directory: " + validPath;
                return false;
              } else if (options.min && stat.size < +options.min || options.max && stat.size > +options.max) {
                error = "Size " + stat.size + " is out of range: " + validPath;
                return false;
              }
            }
            if (typeof options.validate === "function" && (res = options.validate(validPath)) !== true) {
              if (typeof res === "string") {
                error = res;
              }
              return false;
            }
          } catch (e) {
            error = e + "";
            return false;
          }
          return true;
        },
        // trueValue, falseValue, caseSensitive don't work.
        phContent: function(param) {
          return param === "error" ? error : param !== "min" && param !== "max" ? null : options.hasOwnProperty(param) ? options[param] + "" : "";
        }
      });
      options = options || {};
      if (query == null) {
        query = 'Input path (you can "cd" and "pwd"): ';
      }
      exports2.question(query, readOptions);
      return validPath;
    };
    function getClHandler(commandHandler, options) {
      var clHandler = {}, hIndex = {};
      if (typeof commandHandler === "object") {
        Object.keys(commandHandler).forEach(function(cmd) {
          if (typeof commandHandler[cmd] === "function") {
            hIndex[options.caseSensitive ? cmd : cmd.toLowerCase()] = commandHandler[cmd];
          }
        });
        clHandler.preCheck = function(res) {
          var cmdKey;
          clHandler.args = parseCl(res);
          cmdKey = clHandler.args[0] || "";
          if (!options.caseSensitive) {
            cmdKey = cmdKey.toLowerCase();
          }
          clHandler.hRes = cmdKey !== "_" && hIndex.hasOwnProperty(cmdKey) ? hIndex[cmdKey].apply(res, clHandler.args.slice(1)) : hIndex.hasOwnProperty("_") ? hIndex._.apply(res, clHandler.args) : null;
          return { res, forceNext: false };
        };
        if (!hIndex.hasOwnProperty("_")) {
          clHandler.limit = function() {
            var cmdKey = clHandler.args[0] || "";
            if (!options.caseSensitive) {
              cmdKey = cmdKey.toLowerCase();
            }
            return hIndex.hasOwnProperty(cmdKey);
          };
        }
      } else {
        clHandler.preCheck = function(res) {
          clHandler.args = parseCl(res);
          clHandler.hRes = typeof commandHandler === "function" ? commandHandler.apply(res, clHandler.args) : true;
          return { res, forceNext: false };
        };
      }
      return clHandler;
    }
    exports2.promptCL = function(commandHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "Requested command is not available.",
        caseSensitive: false,
        history: true
      }, options), clHandler = getClHandler(commandHandler, readOptions);
      readOptions.limit = clHandler.limit;
      readOptions.preCheck = clHandler.preCheck;
      exports2.prompt(readOptions);
      return clHandler.args;
    };
    exports2.promptLoop = function(inputHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        trueValue: null,
        falseValue: null,
        caseSensitive: false,
        history: true
      }, options);
      while (true) {
        if (inputHandler(exports2.prompt(readOptions))) {
          break;
        }
      }
    };
    exports2.promptCLLoop = function(commandHandler, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false,
        limitMessage: "Requested command is not available.",
        caseSensitive: false,
        history: true
      }, options), clHandler = getClHandler(commandHandler, readOptions);
      readOptions.limit = clHandler.limit;
      readOptions.preCheck = clHandler.preCheck;
      while (true) {
        exports2.prompt(readOptions);
        if (clHandler.hRes) {
          break;
        }
      }
    };
    exports2.promptSimShell = function(options) {
      return exports2.prompt(margeOptions({
        // -------- default
        hideEchoBack: false,
        history: true
      }, options, {
        // -------- forced
        prompt: function() {
          return IS_WIN ? "$<cwd>>" : (
            // 'user@host:cwd$ '
            (process.env.USER || "") + (process.env.HOSTNAME ? "@" + process.env.HOSTNAME.replace(/\..*$/, "") : "") + ":$<cwdHome>$ "
          );
        }()
      }));
    };
    function _keyInYN(query, options, limit) {
      var res;
      if (query == null) {
        query = "Are you sure? ";
      }
      if ((!options || options.guide !== false) && (query += "")) {
        query = query.replace(/\s*:?\s*$/, "") + " [y/n]: ";
      }
      res = exports2.keyIn(query, margeOptions(options, {
        // -------- forced
        hideEchoBack: false,
        limit,
        trueValue: "y",
        falseValue: "n",
        caseSensitive: false
        // mask doesn't work.
      }));
      return typeof res === "boolean" ? res : "";
    }
    exports2.keyInYN = function(query, options) {
      return _keyInYN(query, options);
    };
    exports2.keyInYNStrict = function(query, options) {
      return _keyInYN(query, options, "yn");
    };
    exports2.keyInPause = function(query, options) {
      if (query == null) {
        query = "Continue...";
      }
      if ((!options || options.guide !== false) && (query += "")) {
        query = query.replace(/\s+$/, "") + " (Hit any key)";
      }
      exports2.keyIn(query, margeOptions({
        // -------- default
        limit: null
      }, options, {
        // -------- forced
        hideEchoBack: true,
        mask: ""
      }));
    };
    exports2.keyInSelect = function(items, query, options) {
      var readOptions = margeOptions({
        // -------- default
        hideEchoBack: false
      }, options, {
        // -------- forced
        trueValue: null,
        falseValue: null,
        caseSensitive: false,
        // limit (by items),
        phContent: function(param) {
          return param === "itemsCount" ? items.length + "" : param === "firstItem" ? (items[0] + "").trim() : param === "lastItem" ? (items[items.length - 1] + "").trim() : null;
        }
      }), keylist = "", key2i = {}, charCode = 49, display = "\n";
      if (!Array.isArray(items) || !items.length || items.length > 35) {
        throw "`items` must be Array (max length: 35).";
      }
      items.forEach(function(item, i) {
        var key = String.fromCharCode(charCode);
        keylist += key;
        key2i[key] = i;
        display += "[" + key + "] " + (item + "").trim() + "\n";
        charCode = charCode === 57 ? 97 : charCode + 1;
      });
      if (!options || options.cancel !== false) {
        keylist += "0";
        key2i["0"] = -1;
        display += "[0] " + (options && options.cancel != null && typeof options.cancel !== "boolean" ? (options.cancel + "").trim() : "CANCEL") + "\n";
      }
      readOptions.limit = keylist;
      display += "\n";
      if (query == null) {
        query = "Choose one from list: ";
      }
      if (query += "") {
        if (!options || options.guide !== false) {
          query = query.replace(/\s*:?\s*$/, "") + " [$<limit>]: ";
        }
        display += query;
      }
      return key2i[exports2.keyIn(display, readOptions).toLowerCase()];
    };
    exports2.getRawInput = function() {
      return rawInput;
    };
    function _setOption(optionName, args) {
      var options;
      if (args.length) {
        options = {};
        options[optionName] = args[0];
      }
      return exports2.setDefaultOptions(options)[optionName];
    }
    exports2.setPrint = function() {
      return _setOption("print", arguments);
    };
    exports2.setPrompt = function() {
      return _setOption("prompt", arguments);
    };
    exports2.setEncoding = function() {
      return _setOption("encoding", arguments);
    };
    exports2.setMask = function() {
      return _setOption("mask", arguments);
    };
    exports2.setBufferSize = function() {
      return _setOption("bufferSize", arguments);
    };
  }
});

// rts.js


//@ts-check
'use strict';

const util_ = require('util');
const reader_ = require_readline_sync();
const debug_ = (x) => {
  console.log(util_.inspect(x, false, null))
}

/** @typedef {String} Name */

// Closed and Open values
// ----------------------------------------------------------------------------------------------------

const _Var       = 'Var'
const _Let       = 'Let'
const _Lam       = 'Lam'
const _App       = 'App'
const _Quote     = 'Quote'
const _Splice    = 'Splice'
const _Return    = 'Return'
const _Bind      = 'Bind'
const _Seq       = 'Seq'
const _New       = 'New'
const _Write     = 'Write'
const _Read      = 'Read'
const _Closure   = 'Closure'
const _CSP       = 'CSP'
const _LiftedLam = 'LiftedLam'
const _Body      = 'Body'
const _NatElim   = 'NatElim'
const _ReadNat   = 'ReadNat'
const _PrintNat  = 'PrintNat'
const _Log       = 'Log'
const _Rec       = 'Rec'
const _Suc       = 'Suc'
const _Proj      = 'Proj'
const _NatLit    = 'NatLit'

/**
  @typedef {
  {tag: _Var, name: Name} |
  {tag: _Let, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Lam, _1: Name, _2: (v: Open) => Open} |
  {tag: _App, _1: Open, _2: Open} |
  {tag: _Quote, _1: Open} |
  {tag: _Splice, _1: Open} |
  {tag: _Return, _1: Open} |
  {tag: _Bind, _1: Name, _2: Open, _3: (v: Open) => Open} |
  {tag: _Seq, _1: Open, _2: Open} |
  {tag: _New, _1: Open} |
  {tag: _Write, _1: Open, _2: Open} |
  {tag: _Read, _1: Open} |
  {tag: _CSP, _1: Closed, _2: Name} |

  {tag: _NatElim, _1 : Open, _2: Open, _3: Open} |
  {tag: _ReadNat} |
  {tag: _PrintNat, _1: Open} |
  {tag: _Log, _1: String} |
  {tag: _Rec, _1: Map<String, Open>} |
  {tag: _Suc, _1: Open} |
  {tag: _Proj, _1: Open, _2: Name}
  } Open

  @typedef {
    undefined |
    Object |
    Number |
    Open |
    {_1 : Closed}
    } Closed
*/

/** @type {(x:Name) => Open} */
function Var_    (x)       {return {tag: _Var, name: x}}
/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */
function Let_    (x, t, u) {return {tag: _Let, _1: x, _2: t, _3: u}}
/** @type {(x:Name, t:(v: Open) => Open) => Open} */
function Lam_    (x, t)    {return {tag: _Lam, _1: x, _2: t}}
/** @type {(t:Open, u:Open) => Open} */
function App_    (t, u)    {return {tag: _App, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function Quote_  (t)       {return {tag: _Quote, _1: t}}
/** @type {(t: Open) => Open} */
function Splice_ (t)       {return {tag: _Splice, _1: t}}
/** @type {(t: Open) => Open} */
function Return_ (t)       {return {tag: _Return, _1: t}}
/** @type {(x:Name, t:Open, u:(v: Open) => Open) => Open} */
function Bind_   (x, t, u) {return {tag: _Bind, _1: x, _2: t, _3: u}}
/** @type {(t:Open, u:Open) => Open} */
function Seq_    (t, u)    {return {tag: _Seq, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function New_    (t)       {return {tag: _New, _1: t}}
/** @type {(t:Open, u:Open) => Open} */
function Write_  (t, u)    {return {tag: _Write, _1: t, _2: u}}
/** @type {(t: Open) => Open} */
function Read_   (t)       {return {tag: _Read, _1: t}}
/** @type {(t: Closed, x:Name) => Open} */
function CSP_ (t,x)        {return {tag: _CSP, _1: t, _2:x}}
/** @type {(s: Open, z: Open, n: Open) => Open} */
function NatElim_(s, z, n) {return {tag: _NatElim, _1: s, _2: z, _3: n}}
/** @type {() => Open} */
function ReadNat_() {return {tag: _ReadNat}}
/** @type {(t: Open) => Open} */
function PrintNat_(t) {return {tag: _PrintNat, _1: t}}
/** @type {(s:String) => Open} */
function Log_(s) {return {tag: _Log, _1: s}}
/** @type {(ts: Map<String, Open>) => Open} */
function Rec_(ts) {return {tag: _Rec, _1: ts}}
/** @type {(n:Open) => Open} */
function Suc_(n) {return {tag: _Suc, _1: n}}
/** @type {(t:Open, x: Name) => Open} */
function Proj_(t, x) {return {tag: _Proj, _1: t, _2: x}}

/** @type {Open} */
const CSP_undefined_ = CSP_(undefined, 'undefined')


// Non-shadowing first-order term representation of code
// ----------------------------------------------------------------------------------------------------

/**
   @typedef {
   {tag: _Var, _1: Name} |
   {tag: _CSP, _1: Number, _2: Name} |
   {tag: _Let, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Lam, _1: Name, _2: Tm} |
   {tag: _LiftedLam, _1: Name, _2: Name, _3: Array<Name>} |
   {tag: _App, _1: Tm, _2: Tm} |
   {tag: _Quote, _1: Tm} |
   {tag: _Splice, _1: Tm} |
   {tag: _Return, _1: Tm} |
   {tag: _Bind, _1: Name, _2: Tm, _3: Tm} |
   {tag: _Seq, _1: Tm, _2: Tm} |
   {tag: _New, _1: Tm} |
   {tag: _Write, _1: Tm, _2: Tm} |
   {tag: _Read, _1: Tm} |
   {tag: _Log, _1: String} |
   {tag: _ReadNat} |
   {tag: _PrintNat, _1: Tm} |
   {tag: _Rec, _1: Map<Name, Tm>} |
   {tag: _Proj, _1: Tm, _2: Name} |
   {tag: _Suc, _1: Tm} |
   {tag: _NatElim, _1: Tm, _2 : Tm, _3 : Tm} |
   {tag: _NatLit, _1: Number}
   } Tm

  // Top-level terms are only used in closure conversion in closed codegen
   @typedef {
   {tag: _Let, _1: Name, _2: Tm, _3: Top} |
   {tag: _Bind, _1 : Name, _2: Tm, _3: Top} |
   {tag: _Seq, _1: Tm, _2: Top} |
   {tag: _Body, _1: Tm} |
   {tag: _Closure, _1: Name, _2: Array<Name>, _3: Name, _4: Tm, _5: Top}
   } Top
*/

/** @type {(x: Name) => Tm} */
function TVar_       (x) {return {tag: _Var, _1: x } }
/** @type {(x:Number, y:Name) => Tm} */
function TCSP_       (i, x) {return {tag: _CSP, _1: i, _2:x} }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TLet_       (x,t,u) {return {tag: _Let, _1:x , _2:t , _3:u } }
/** @type {(x:Name, t:Tm) => Tm} */
function TLam_       (x,t) {return {tag: _Lam, _1:x , _2: t } }
/** @type {(f:Name, x:Name, args:Array<Name>) => Tm} */
function TLiftedLam_ (f, x, args) {return {tag: _LiftedLam, _1: f , _2:x, _3: args} }
/** @type {(t:Tm, u:Tm) => Tm} */
function TApp_       (t,u) {return {tag: _App, _1: t , _2: u}}
/** @type {(t: Tm) => Tm} */
function TQuote_     (t) {return {tag: _Quote, _1: t } }
/** @type {(t: Tm) => Tm} */
function TSplice_    (t) {return {tag: _Splice, _1: t } }
/** @type {(t: Tm) => Tm} */
function TReturn_    (t) {return {tag: _Return, _1: t } }
/** @type {(x:Name, t:Tm, u:Tm) => Tm} */
function TBind_      (x, t, u) {return {tag: _Bind, _1: x , _2: t , _3: u } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TSeq_       (t, u) {return {tag: _Seq, _1: t , _2: u } }
/** @type {(t: Tm) => Tm} */
function TNew_       (t) {return {tag: _New, _1: t } }
/** @type {(t:Tm, u:Tm) => Tm} */
function TWrite_     (t, u) {return {tag: _Write, _1: t , _2: u } }
/** @type {(t: Tm) => Tm} */
function TRead_      (t) {return {tag: _Read, _1: t }}
/** @type {(t: Number) => Tm} */
function TNatLit_      (t) {return {tag: _NatLit, _1: t }}
/** @type {() => Tm} */
function TReadNat_() {return {tag: _ReadNat}}
/** @type {(t:Tm) => Tm} */
function TPrintNat_(t) {return {tag: _PrintNat, _1: t}}
/** @type {(ts:Map<Name, Tm>) => Tm} */
function TRec_(ts) {return {tag: _Rec, _1: ts}}
/** @type {(t:Tm, x: Name) => Tm} */
function TProj_(t, x) {return {tag: _Proj, _1: t, _2:x}}
/** @type {(t:Tm) => Tm} */
function TSuc_(t) {return {tag: _Suc, _1 : t}}
/** @type {(s:Tm, z:Tm, n:Tm) => Tm} */
function TNatElim_(s,z,n) {return {tag: _NatElim, _1 : s, _2: z, _3 : n}}
/** @type {(x:String) => Tm} */
function TLog_(x) {return {tag: _Log, _1 : x}}


/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopLet_     (x, t, u) { return {tag: _Let, _1: x , _2: t, _3: u } }
/** @type {(x:Name, t:Tm, u:Top) => Top} */
function TopBind_    (x, t, u) { return {tag: _Bind, _1: x , _2: t, _3: u } }
/** @type {(t:Tm, u:Top) => Top} */
function TopSeq_     (t, u) { return {tag: _Seq, _1: t , _2: u } }
/** @type {(t: Tm) => Top} */
function TopBody_    (t) { return {tag: _Body, _1: t} }
/** @type {(x:Name, env: Array<Name>, arg:Name, body:Tm, t:Top) => Top} */
function TopClosure_ (x, env, arg, body, t) { return {tag: _Closure, _1: x , _2: env, _3: arg , _4: body , _5: t}}

// SHARED TOP STATE
// ----------------------------------------------------------------------------------------------------

/** @type{Set<Name>} */
const boundVarSet_ = new Set();

/** @type {(x: String) => String} */
function freshenName_(x){
  let res = x
  while (boundVarSet_.has(res)){
    res = res + boundVarSet_.size
  }
  return res
}

//----------------------------------------------------------------------------------------------------

/** @type{(ls : undefined|Array<String>, code:String) => void} */
function displayCode_(loc, code){
  if (loc) {
    console.log('CODE GENERATED AT:')
    for (const l of loc){
      console.log(l)
    }
  } else {
    console.log('CODE GENERATED:')
  }
  console.log('CODE:')
  console.log(code)
  console.log('')
}

// CODE GENERATION CALLED FROM CLOSED EVALUATION
// ----------------------------------------------------------------------------------------------------

/** @type{(x: String, y: Array<Closed>, loc: undefined|Array<String>) => Closed} */
function evalCodeGenClosed_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}

/** @type {(t:Open, loc: undefined|Array<String>) => Closed} */
function codegenClosed_(t, loc) {

  // CLOSURE CONVERSION
  // ----------------------------------------------------------------------------------------------------
  /** @type {(t:Open) => {_1 : Top, _2 : Array<Closed>}} */
  function closureConvert(top){

    /** @typedef {{name : Name, env: Array<Name>, arg: Name, body: Tm}} Closure */

    /** @type {undefined|Number} */
    let stage = undefined

    /** @type{(s : Number, act: () => Tm) => Tm} */
    const inStage = (s, act) => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type {Set<Name>} */
    let freeVars = new Set()

    /** @type {Set<Name>} */
    let topVars = new Set()

    /** @type {Array<Closure>} */
    let closures = new Array()

    /** @type {Array<Closed>} */
    let cspArray = new Array()

    /** @type {Name} */
    let currentTopName = ''

    /** @type{(t:Open) => Top} */
    function goTop(top){

      /** @type {(cs: Array<Closure>, t: Top) => Top} */
      function addClosures(cs, t){
        let res = t
        for (const cl of cs.reverse()){
          res = TopClosure_(cl.name, cl.env, cl.arg, cl.body, res)
        }
        return res
      }

      /** @type {(name: Name) => void} */
      function resetAtTop(name){
        currentTopName = name
        freeVars.clear()
        closures = new Array()
      }

      switch (top.tag) {
        case _Let : {
          const x = freshenName_(top._1)
          const t = top._2
          const u = top._3
          resetAtTop(x)
          const t2 = go(t)
          const newClosures = closures
          boundVarSet_.add(x)
          topVars.add(x)
          const u2 = goTop(u(Var_(x)))
          return addClosures(newClosures, TopLet_(x, t2, u2))
        }
        case _Bind : {
          const x = freshenName_(top._1)
          const t = top._2
          const u = top._3
          resetAtTop(x)
          const t2 = go(t)
          const newClosures = closures
          boundVarSet_.add(x)
          topVars.add(x)
          const u2 = goTop(u(Var_(x)))
          return addClosures(newClosures, TopBind_(x, t2, u2))
        }

        case _Seq : {
          const t = top._1
          const u = top._2
          resetAtTop('$cl')
          const t2 = go(t)
          const newClosures = closures
          const u2 = goTop(u)
          return addClosures(newClosures, TopSeq_(t2, u2))
        }

        default: {
          resetAtTop('$cl')
          const t2 = go(top)
          return addClosures(closures, TopBody_(t2))
        }
      } // switch
    } // goTop

    /** @type {(x: Name, act: (x: Name) => Tm) => Tm} */
    function fresh(x, act){
      const x2 = freshenName_(x)
      boundVarSet_.add(x2)
      const res = act(x2)
      boundVarSet_.delete(x)
      return res
    }

    /** @type {(x : Name, act : (x: Name) => Tm) => Tm} */
    const bind = (x, act) =>
      fresh(x, (x) => {
        const a = act(x)
        freeVars.delete(x)
        return a
    })

    /** @type {(t: Open) => Tm} */
    function go(top){
      switch (top.tag){
        case _Var : {
          if (!topVars.has(top.name)) {
            freeVars.add(top.name)
          }
          return TVar_(top.name)
        }
        case _Let : {
          const x = top._1
          const t = top._2
          const u = top._3
          const t2 = go(t)
          return bind(x, (x) => TLet_(x, t2, go(u(Var_(x)))))
        }
        case _Lam : {
          const x = top._1
          const t = top._2
          if (stage === undefined) {
            return fresh(x, (x) => {
              let oldFreeVars = freeVars
              freeVars = new Set()
              const t2 = go(t(Var_(x)))
              freeVars.delete(x)
              const capture = Array.from(freeVars.values())
              const clName  = currentTopName + closures.length + '_'
              closures.push({name: clName, env: capture, arg: x, body: t2})
              freeVars.forEach((x) => oldFreeVars.add(x))
              freeVars = oldFreeVars
              return TLiftedLam_(clName, x, capture)
            })
          } else {
            return bind(x, (x) => TLam_(x, go(t(Var_(x)))))
          }
        }
        case _App : return TApp_(go(top._1), go(top._2))
        case _Quote : {
          if (stage === undefined) {
            return inStage(1, () => TQuote_(go(top._1)))
          } else {
            return inStage(stage + 1, () => TQuote_(go(top._1)))
          }
        }
        case _Splice : {
          if (stage && stage > 0){
            return inStage(stage - 1, () => TSplice_(go(top._1)))
          } else {
            return TSplice_(go(top._1))
          }
        }
        case _Bind : {
          const t2 = go(top._2)
          return bind(top._1, (x) => TBind_(x, t2, go(top._3(Var_(x)))))
        }
        case _Return : return TReturn_(go(top._1))
        case _Seq    : return TSeq_(go(top._1), go(top._2))
        case _New    : return TNew_(go(top._1))
        case _Write  : return TWrite_(go(top._1), go(top._2))
        case _Read   : return TRead_(go(top._1))

        case _CSP : {
          if (typeof top._1 === 'number'){  // inline closed numerals into source code
            return TNatLit_(top._1)
          } else {
            const id = cspArray.length
            cspArray.push(top._1)
            return TCSP_(id, top._2)
          }
        }
        case _Suc     : return TSuc_(go(top._1))
        case _NatElim : return TNatElim_(go(top._1), go(top._2), go(top._3))

        case _Rec : {
          const res = new Map()
          top._1.forEach((t, x) => {res.set(x, go(t))})
          return TRec_(res)
        }
        case _Proj     : return TProj_(go(top._1), top._2)
        case _PrintNat : return TPrintNat_(go(top._1))
        case _ReadNat  : return TReadNat_()
        case _Log      : return TLog_(top._1)
      } // switch
    } // go

    const res = goTop(top)
    return {_1: res, _2: cspArray}
  } // closureConvert

  // CODE EMISSION
  // ----------------------------------------------------------------------------------------------------

  /** @type{(t:Top) => String} */
  function emitCode(t){

    // code builder is simply an array of strings
    /** @type {Array<String>} */
    const builder = new Array()

    /** @type {() => String} */
    const build = () => {return builder.join('')}

    /** @type {Number} */
    let indentation = 0

    /** @type {Boolean} */
    let isTail = true

    // only used in open emission
    /** @type{Number} */
    let stage = 0

    /** @type{Set<Name>} */
    const closedVars = new Set()

    /** @type {(s:String) => void} */
    const put = (s) => {builder.push(s)}

    /** @type {(s:String) => () => void} */
    const str = (s) => () => put(s)

    /** @type {(s:String) => () => void} */
    const strLit = (s) => () => put("`" + s + "`")

    /** @type {() => void} */
    const newl = () => {put('\n' + ' '.repeat(indentation))}

    /** @type{(s : Number, act: () => void) => (() => void)} */
    const inStage = (s, act) => () => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const tail = (act) => () => {
      const backup = isTail
      isTail = true
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const nonTail = (act) => () => {
      const backup = isTail
      isTail = false
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const indent = (act) => () => {
      const backup = indentation
      indentation += 2
      const res = act()
      indentation = backup
      return res
    }

    /** @type{ () => void } */
    function semi(){put(';')}

    /** @type{ (act: () => void) => (() => void) } */
    const par = (act) => () => {put('('); act(); put(')')}

    /** @type{ (x: Name, closed: Boolean, act: () => void) => () => void} */
    const bind = (x, closed, act) => () => {
      if (closed) {closedVars.add(x)};
      const res = act();
      if (closed) {closedVars.delete(x)}
      return res;
    }

    /** @type{ (x: Name, closed: Boolean, t: () => void, u: () => void) => (() => void) } */
    const jLet = (x, closed, t, u) => () => {
      if (isTail){
        put('const ' + x + ' = '); indent(nonTail(t))(); semi(); newl(); tail(bind(x, closed, u))()
      } else {
        put('((' + x + ') => ');
        par(nonTail(bind(x, closed, u)))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{ (t: () => void, u: () => void) => (() => void) } */
    const jSeq = (t, u) => () => {
      if (isTail){
        indent(nonTail(t))(); semi(); newl(); tail(u)()
      } else {
        put('((_) => '); par(nonTail(u))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{(xs: Array<() => void>) => (() => void)} */
    const jTuple = (xs) => () => {
      if (xs.length === 0){
        put('()')
      } else {
        put('('); xs[0](); xs.slice(1, xs.length).forEach((act) => {put(', '); act()}); put(')')
      }
    }

    /** @type((t : () => void) => () => void)} */
    const jReturn = (t) => () => {
      if (isTail) { put('return '); nonTail(t)()
      } else      { t() }
    }

    /** @type{(xs : Array<Name>, closed: Boolean, t: () => void) => () => void} */
    const jLam = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => {');
        tail(() => {
          if (closed) {xs.forEach((x) => closedVars.add(x))}
          t();
          if (closed) {xs.forEach((x) => closedVars.delete(x))}
        })();
        put('}')
      })()
    }

    /** @type{(xs: Array<Name>, closed:Boolean, t: () => void) => () => void} */
    const jLamExp = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => ')
        nonTail(() => {
          if (closed) {xs.forEach((x) => closedVars.add(x))};
          t();
          if (closed) {xs.forEach((x) => closedVars.delete(x))};
          })()
      })()
    }

    /** @type{(t: () => void, u: () => void) => () => void} */
    const cApp = (t, u) => () => {
      jReturn(() => {par(t)(); put('._1'); par(u)()})()
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const cRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put(arr[i][0]);
          put(': ');
          return nonTail(() => ceval(arr[i][1]))();
        } else {
          put(arr[i][0]);
          put(': ');
          nonTail(() => ceval(arr[i][1]))();
          put(', ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(t : () => void, args: Array<() => void>) => () => void} */
    const jApp = (t, args) => () => {
      jReturn(() => {t(); jTuple(args)() })()
    }

    /** @type{(t : () => void) => () => void} */
    const cRun = (t) => () => {
      jReturn(() => {t(); put('()') })()
    }

    /** @type{(env: Array<Name>, x: Name, closed:Boolean, t : () => void) => () => void} */
    const jClosure = (env, x, closed, t) => () => {
      if (env.length === 0){
        jLam([x], closed, t)()
      } else {
        jLamExp(env, closed, jLam([x], closed, t))()
      }
    }

    /** @type{(t: () => void, args: Array<() => void>) => () => void} */
    const jAppClosure = (t, args) => () => {
      if (args.length === 0){
        t()
      } else {
        t(); jTuple(args)()
      }
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put('['); strLit(arr[i][0])(); put(', '); nonTail(() => oeval(arr[i][1]))(); put(']');
        } else {
          put('['); strLit(arr[i][0])(); put(', '); nonTail(() => oeval(arr[i][1]))(); put('], ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(x:Name) => Name} */
    const closeVar = (x) => x + 'c'
    /** @type{(x:Name) => Name} */
    const openVar  = (x) => x + 'o'

    //----------------------------------------------------------------------------------------------------

    /** @type {(t:Tm) => void} */
    function exec(top){
      switch (top.tag){
        case _Var       : return cRun(() => put(top._1))()
        case _Let       : return jLet(top._1, true, () => ceval(top._2), () => exec(top._3))()
        case _Lam       : throw new Error('impossible')
        case _LiftedLam : throw new Error('impossible')
        case _App       : return cRun(cApp(() => ceval(top._1), () => ceval(top._2)))()
        case _Quote     : throw new Error('impossible')
        case _Splice    : return jApp(str('codegen'), [() => ceval(top._1)])()
        case _Return    : return jReturn(() => ceval(top._1))()
        case _Bind      : return jLet(top._1, true, () => exec(top._2), () => exec(top._3))()
        case _Seq       : return jSeq(() => exec(top._1), () => exec(top._2))()
        case _New       : return jReturn(() => {put('{_1 : '); ceval(top._1); put('}')})()
        case _Write     : return nonTail(() => {ceval(top._1); put('._1 = '); ceval(top._2)})()
        case _Read      : return jReturn(() => {ceval(top._1); put('._1')})()
        case _CSP       : return jReturn(() => {put('csp_[' + top._1 + ']()/*'); strLit(top._2); put('*/')})()
        case _Log       : return jApp(str('log_'), [])()
        case _ReadNat   : return jApp(str('readNat_'), [])()
        case _PrintNat  : return jApp(str('printNat_'), [() => ceval(top._1)])()
        case _Rec       : throw new Error('impossible')
        case _Proj      : return cRun(() => {ceval(top._1); put('.'); put(top._2)})()
        case _Suc       : throw new Error('impossible')
        case _NatElim   : return cRun(jApp(str('cNatElim_'),[() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)]))()
        case _NatLit    : throw new Error('impossible')
      }
    }

    /** @type {(t:Tm) => void} */
    function ceval(top){
      switch (top.tag){
        case _Var       : return jReturn(str(top._1))()
        case _Let       : return jLet(top._1, true, () => ceval(top._2), () => ceval(top._3))()
        case _Lam       : throw new Error('impossible')
        case _LiftedLam : return jReturn(() => {
                            put('{_1 : ');
                            jAppClosure(str(closeVar(top._1)), top._3.map(str))();
                            put(', _2 : ');
                            jAppClosure(str(openVar(top._1)), top._3.map(oevalVar))();
                            put('}')
                            })()
        case _App       : return cApp(() => ceval(top._1), () => ceval(top._2))()
        case _Quote     : return inStage(1, () => {return oeval(top._1)})()
        case _Splice    : return jApp(str('codegenClosed_'), [() => ceval(top._1)])()
        case _Return    : return jLam([], true, () => exec(top))()
        case _Bind      : return jLam([], true, () => exec(top))()
        case _Seq       : return jLam([], true, () => exec(top))()
        case _New       : return jLam([], true, () => exec(top))()
        case _Write     : return jLam([], true, () => exec(top))()
        case _Read      : return jLam([], true, () => exec(top))()
        case _CSP       : return jReturn(() => {put('csp_[' + top._1 + ']/*'); put(top._2); put('*/')})()
        case _Log       : return jLam([], true, () => exec(top))()
        case _ReadNat   : return jLam([], true, () => exec(top))()
        case _PrintNat  : return jLam([], true, () => exec(top))()
        case _Rec       : return jReturn(() => {put('{'); cRec(top._1)(); put('}')})()
        case _Proj      : return jReturn(() => {ceval(top._1); put('.'); put(top._2) })()
        case _Suc       : return jApp(str('cSuc_'), [() => ceval(top._1)])()
        case _NatElim   : return jApp(str('cNatElim_'), [() => ceval(top._1), () => ceval(top._2), () => ceval(top._3)])()
        case _NatLit    : return jReturn(str(top._1.toString()))()
      }
    }

    /** @type {(x:Name) => () => void} */
    const oevalVar = (x) => () => {
      if (closedVars.has(x)){
        return jApp(str('CSP_'), [str(x), strLit(x)])();
      } else {
        return jReturn(str(x))();
      }
    }

    /** @type {(t:Tm) => void} */
    function oeval(top){
      switch (top.tag){
        case _Var : return oevalVar(top._1)()
        case _CSP : return jApp(str('CSP_'), [str('csp_[' + top._1 + ']'), strLit(top._2)])()

        case _Let : {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
          }
        }
        case _Lam : {
          return jApp(str('Lam_'), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])()
        }
        case _LiftedLam :
          return jApp(str('Lam_'), [strLit(top._2), jAppClosure(str(openVar(top._1)), top._3.map(str))])()
        case _App : {
          if (stage === 0){
            return jApp(str('app_'), [() => oeval(top._1), () => oeval(top._2)])()
          } else {
            return jApp(str('App_'), [() => oeval(top._1), () => oeval(top._2)])()
          }
        }
        case _Quote : return jApp(str('quote_'), [inStage(stage + 1, () => oeval(top._1))])()
        case _Splice : {
          if (stage === 0){
            return jApp(str('codegenOpen_'), [() => oeval(top._1)])()
          } else {
            return inStage(stage - 1, jApp(str('splice_'), [() => oeval(top._1)]))()
          }
        }

        case _Return    : return jApp(str('Return_'), [() => oeval(top._1)])()
        case _Bind      : return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
        case _Seq       : return jApp(str('Seq_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _New       : return jApp(str('New_'), [() => oeval(top._1)])()
        case _Write     : return jApp(str('Write_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _Read      : return jApp(str('Read_'), [() => oeval(top._1)])()
        case _Log       : return jApp(str('Log_'), [strLit(top._1)])()
        case _ReadNat   : return jApp(str('ReadNat_'), [])()
        case _PrintNat  : return jApp(str('PrintNat_'), [() => oeval(top._1)])()
        case _Rec       : return jApp(str('Rec_'), [ () => {put('new Map(['); oRec(top._1)(); put('])')}])()

        case _Proj : {
          if (stage === 0){
            return jApp(str('proj_'), [() => oeval(top._1), strLit(top._2)])()
          } else {
            return jApp(str('Proj_'), [() => oeval(top._1), strLit(top._2)])()
          }
        }
        case _Suc : {
          if (stage === 0){
            return jApp(str('suc_'), [() => oeval(top._1)])()
          } else {
            return jApp(str('Suc_'), [() => oeval(top._1)])()
          }
        }
        case _NatElim : {
          if (stage === 0){
            return jApp(str('natElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          } else {
            return jApp(str('NatElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          }
        }
        case _NatLit : {
          return jApp(str('CSP_'), [str(top._1.toString()), strLit('')])()
        }
      }
    }

    /** @type {(t:Top) => void} */
    function execTop(top){
      tail(() => {
        switch (top.tag){
          case _Let  : return jLet(top._1, true, () => ceval(top._2), () => {execTop(top._3)})()
          case _Bind : return jLet(top._1, true, () => exec(top._2), () => {execTop(top._3)})()
          case _Seq  : return jSeq((() => exec(top._1)), (() => {execTop(top._2)}))()

          case _Closure : {
            const x    = top._1
            const env  = top._2
            const arg  = top._3
            const body = top._4
            const t    = top._5
            return jLet(closeVar(x), true, jClosure(env, arg, true, () => ceval(body)),
                   jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {return oeval(body)})), () =>
                   execTop(t)))()
          }
          case _Body : {
            return exec(top._1)
          }
        }
      })()
    }

    /** @type {(t:Top) => void} */
    function cevalTop(top){
      switch (top.tag){
        case _Let : {
          return jLet(top._1, true, () => ceval(top._2), () => {newl(); cevalTop(top._3)})()
        }
        case _Bind : {
          return jLam([], true, () => execTop(top))()
        }
        case _Seq : {
          return jLam([], true, () => execTop(top))()
        }
        case _Closure : {
          const x    = top._1
          const env  = top._2
          const arg  = top._3
          const body = top._4
          const t    = top._5
          return jLet(closeVar(x), true, jClosure(env, arg, true, () => ceval(body)),
                jLet(openVar(x), true, jClosure(env, arg, false, inStage(0, () => {return oeval(body)})), () =>
                cevalTop(t)))()
        }
        case _Body:
          return ceval(top._1)
      }
    } // cevalTop

    /** @type {(t:Top) => void} */
    function oevalTop(top){
      switch (top.tag){
        case _Let: {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => {newl(); oevalTop(top._3)})()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])()
          }
        }
        case _Bind:
          return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oevalTop(top._3))])()
        case _Seq:
          return jApp(str('Seq_'), [() => oeval(top._1), () => oevalTop(top._2)])()
        case _Closure:
          throw new Error('impossible')
        case _Body:
          return oeval(top._1)
      }
    } // oevalTop

    put('() => {\n');
    cevalTop(t);
    put(';\n}');
    return build();

  } // emitCode

  const {_1: t2, _2: cspArray} = closureConvert(t);
  const source = emitCode(t2);
  return evalCodeGenClosed_(source, cspArray, loc)
} // codegenClosed


// CODE GENERATION CALLED FROM OPEN EVALUATION
// ----------------------------------------------------------------------------------------------------

/** @type{(x: String, y: Array<Closed>, loc: undefined|Array<String>) => Open} */
function evalCodeGenOpen_(src_, csp_, loc_) {
  displayCode_(loc_, src_);
  const res_ = eval(src_)();
  return res_;
}

/** @type {(t:Open, loc: undefined|Array<String>) => Open} */
function codegenOpen_(t, loc){

  // CODE QUOTING
  // ----------------------------------------------------------------------------------------------------
  /** @type{(t : Open) => {_1: Tm, _2: Array<Closed>}} */
  function quote(top){

    /** @type {Array<Closed>} */
    const cspArray = new Array();

    /** @type{(x: Name, act : (x:Name) => Tm) => Tm} */
    function bind(x, act){
      const x2 = freshenName_(x);
      boundVarSet_.add(x2);
      const res = act(x2);
      boundVarSet_.delete(x);
      return res;
    }

    /** @type{(t : Open) => Tm} */
    function go(top){
      switch (top.tag){
        case _Var : return TVar_(top.name)
        case _Let : {
          const x = top._1
          const t = top._2
          const u = top._3
          const t2 = go(t)
          return bind(x, (x) => TLet_(x, t2, go(u(Var_(x)))))
        }
        case _Lam    : return bind(top._1, (x) => TLam_(x, go(top._2(Var_(x)))))
        case _App    : return TApp_(go(top._1), go(top._2))
        case _Quote  : return TQuote_(go(top._1))
        case _Splice : return TSplice_(go(top._1))
        case _Bind : {
          const t2 = go(top._2)
          return bind(top._1, (x) => TBind_(x, t2, go(top._3(Var_(x)))))
        }
        case _Return : return TReturn_(go(top._1))
        case _Seq    : return TSeq_(go(top._1), go(top._2))
        case _New    : return TNew_(go(top._1))
        case _Write  : return TWrite_(go(top._1), go(top._2))
        case _Read   : return TRead_(go(top._1))

        case _CSP : {
          if (typeof top._1 === 'number'){  // inline closed numerals into source code
            return TNatLit_(top._1)
          } else {
            const id = cspArray.length
            cspArray.push(top._1)
            return TCSP_(id, top._2)
          }
        }
        case _Suc     : return TSuc_(go(top._1))
        case _NatElim : return TNatElim_(go(top._1), go(top._2), go(top._3))

        case _Rec : {
          const res = new Map()
          top._1.forEach((t, x) => {res.set(x, go(t))})
          return TRec_(res)
        }
        case _Proj     : return TProj_(go(top._1), top._2)
        case _PrintNat : return TPrintNat_(go(top._1))
        case _ReadNat  : return TReadNat_()
        case _Log      : return TLog_(top._1)
      } // switch
    } // go

    return {_1: go(top), _2: cspArray}
  } // quote

  // CODE EMISSION
  //----------------------------------------------------------------------------------------------------

  /** @type{(t:Tm) => String} */
  function emitCode(t){

    // code builder is simply an array of strings
    /** @type {Array<String>} */
    const builder = new Array()

    /** @type {() => String} */
    const build = () => {return builder.join('')}

    /** @type {Set<Name>} */
    const localVars = new Set()

    /** @type {Number} */
    let indentation = 0

    /** @type {Boolean} */
    let isTail = true

    /** @type{Number} */
    let stage = 0

    /** @type {(s:String) => void} */
    const put = (s) => {builder.push(s)}

    /** @type {(s:String) => () => void} */
    const str = (s) => () => put(s)

    /** @type {(s:String) => () => void} */
    const strLit = (s) => () => put("`" + s + "`")

    /** @type {() => void} */
    const newl = () => {put('\n' + ' '.repeat(indentation))}

    /** @type{(s : Number, act: () => void) => (() => void)} */
    const inStage = (s, act) => () => {
      const backup = stage
      stage = s
      const res = act()
      stage = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const tail = (act) => () => {
      const backup = isTail
      isTail = true
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const nonTail = (act) => () => {
      const backup = isTail
      isTail = false
      const res = act()
      isTail = backup
      return res
    }

    /** @type{ (act: () => void) => (() => void) } */
    const indent = (act) => () => {
      const backup = indentation
      indentation += 2
      const res = act()
      indentation = backup
      return res
    }

    /** @type{ () => void } */
    function semi(){put(';')}

    /** @type{ (act: () => void) => (() => void) } */
    const par = (act) => () => {put('('); act(); put(')')}

    /** @type{ (x: Name, act: () => void) => () => void} */
    const bind = (x, act) => () => {
      if (closed) {localVars.add(x)};
      const res = act();
      if (closed) {localVars.delete(x)}
      return res;
    }

    /** @type{ (x: Name, closed: Boolean, t: () => void, u: () => void) => (() => void) } */
    const jLet = (x, closed, t, u) => () => {
      if (isTail){
        put('const ' + x + ' = '); indent(nonTail(t))(); semi(); newl(); tail(bind(x, u))()
      } else {
        put('((' + x + ') => ');
        par(nonTail(bind(x, u)))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{ (t: () => void, u: () => void) => (() => void) } */
    const jSeq = (t, u) => () => {
      if (isTail){
        indent(nonTail(t))(); semi(); newl(); tail(u)()
      } else {
        put('((_) => '); par(nonTail(u))(); put(')('); nonTail(t)(); put(')')
      }
    }

    /** @type{(xs: Array<() => void>) => (() => void)} */
    const jTuple = (xs) => () => {
      if (xs.length === 0){
        put('()')
      } else {
        put('('); xs[0](); xs.slice(1, xs.length).forEach((act) => {put(', '); act()}); put(')')
      }
    }

    /** @type((t : () => void) => () => void)} */
    const jReturn = (t) => () => {
      if (isTail) { put('return '); nonTail(t)()
      } else      { t() }
    }

    /** @type{(xs : Array<Name>, closed: Boolean, t: () => void) => () => void} */
    const jLam = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => {');
        xs.forEach((x) => localVars.add(x))
        tail(t)();
        xs.forEach((x) => localVars.delete(x))
        put('}')
      })()
    }

    /** @type{(xs: Array<Name>, closed:Boolean, t: () => void) => () => void} */
    const jLamExp = (xs, closed, t) => () => {
      jReturn(() => {
        jTuple(xs.map(str))();
        put(' => ')
        nonTail(t)();
      })()
    }

    /** @type{(t: () => void, u: () => void) => () => void} */
    const cApp = (t, u) => () => {
      jReturn(() => {par(t)(); put('._1'); par(u)()})()
    }

    /** @type{(t : () => void, args: Array<() => void>) => () => void} */
    const jApp = (t, args) => () => {
      jReturn(() => {t(); jTuple(args)() })()
    }

    /** @type{(t : () => void) => () => void} */
    const cRun = (t) => () => {
      jReturn(() => {t(); put('()') })()
    }

    /** @type{(env: Array<Name>, x: Name, closed:Boolean, t : () => void) => () => void} */
    const jClosure = (env, x, closed, t) => () => {
      if (env.length === 0){
        jLam([x], closed, t)()
      } else {
        jLamExp(env, closed, jLam([x], closed, t))()
      }
    }

    /** @type{(t: () => void, args: Array<() => void>) => () => void} */
    const jAppClosure = (t, args) => () => {
      if (args.length === 0){
        t()
      } else {
        t(); jTuple(args)()
      }
    }

    /** @type{(ts : Map<Name, Tm>) => () => void} */
    const oRec = (ts) => () => {
      const arr = Array.from(ts);
      /** @type{(i:Number) => void} */
      function go(i) {
        if (i == arr.length){
          return;
        } else if (arr.length !== 0 && i == arr.length - 1){
          put('['); put(arr[i][0]); put(', '); nonTail(() => oeval(arr[i][1]))(); put(']');
        } else {
          put('['); put(arr[i][0]); put(', '); nonTail(() => oeval(arr[i][1]))(); put('], ');
          return go(i+1);
        }
      }
      go(0)
    }

    /** @type{(x:Name) => Name} */
    const openVar  = (x) => x + 'o'

    //----------------------------------------------------------------------------------------------------

    // Local variables are those bound within the currently generate code
    // Non-local vars are the bound vars floating around, bound upstream.

    /** @type {(x:Name) => () => void} */
    const oevalVar = (x) => () => {
      if (localVars.has(x)) {
        return jReturn(str(x))();
      } else {
        return jApp(str('Var_'), [strLit(x)])();
      }
    }

    /** @type {(t:Tm) => void} */
    function oeval(top){
      switch (top.tag){
        case _Var : return oevalVar(top._1)();
        case _CSP : return jApp(str('CSP_'), [str('csp_[' + top._1 + ']'), strLit(top._2)])()

        case _Let : {
          if (stage === 0){
            return jLet(top._1, false, () => oeval(top._2), () => oeval(top._3))()
          } else {
            return jApp(str('Let_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
          }
        }
        case _Lam : {
          return jApp(str('Lam_'), [strLit(top._1), jLam([top._1], false, () => oeval(top._2))])()
        }
        case _LiftedLam :
          throw new Error('impossible')
        case _App : {
          if (stage === 0){
            return jApp(str('app_'), [() => oeval(top._1), () => oeval(top._2)])()
          } else {
            return jApp(str('App_'), [() => oeval(top._1), () => oeval(top._2)])()
          }
        }
        case _Quote : return jApp(str('quote_'), [inStage(stage + 1, () => oeval(top._1))])()
        case _Splice : {
          if (stage === 0){
            return jApp(str('codegenOpen_'), [() => oeval(top._1)])()
          } else {
            return inStage(stage - 1, jApp(str('splice_'), [() => oeval(top._1)]))()
          }
        }

        case _Return    : return jApp(str('Return_'), [() => oeval(top._1)])()
        case _Bind      : return jApp(str('Bind_'), [strLit(top._1), () => oeval(top._2), jLam([top._1], false, () => oeval(top._3))])()
        case _Seq       : return jApp(str('Seq_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _New       : return jApp(str('New_'), [() => oeval(top._1)])()
        case _Write     : return jApp(str('Write_'), [() => oeval(top._1), () => oeval(top._2)])()
        case _Read      : return jApp(str('Read_'), [() => oeval(top._1)])()
        case _Log       : return jApp(str('Log_'), [strLit(top._1)])()
        case _ReadNat   : return jApp(str('ReadNat_'), [])()
        case _PrintNat  : return jApp(str('PrintNat_'), [() => oeval(top._1)])()
        case _Rec       : return jApp(str('Rec_'), [ () => {put('new Map(['); oRec(top._1)(); put('])')}])()

        case _Proj : {
          if (stage === 0){
            return jApp(str('proj_'), [() => oeval(top._1), strLit(top._2)])()
          } else {
            return jApp(str('Proj_'), [() => oeval(top._1), strLit(top._2)])()
          }
        }
        case _Suc : {
          if (stage === 0){
            return jApp(str('suc_'), [() => oeval(top._1)])()
          } else {
            return jApp(str('Suc_'), [() => oeval(top._1)])()
          }
        }
        case _NatElim : {
          if (stage === 0){
            return jApp(str('natElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          } else {
            return jApp(str('NatElim_'), [() => oeval(top._1), () => oeval(top._2), () => oeval(top._3)])()
          }
        }
        case _NatLit : {
          const s = top._1.toString();
          return jApp(str('CSP_'), [str(s), strLit(s)])()
        }
      } // switch
    } // oeval

    put('() => {\n');
    oeval(t);
    put(';\n}');
    return build();

  } // emitCode

  switch (t.tag){
    case _CSP : {
      const res = codegenClosed_(t._1, loc);
      return CSP_(res, '');
    }
    case _Quote : {
      const {_1: t2, _2: cspArray} = quote(t._1);
      const source = emitCode(t2);
      return evalCodeGenOpen_(source, cspArray, loc);
    }
    default : {
      return Splice_(t)
    }
  }
} // codegenOpen_


// CLOSED COMPUTATIONS
//----------------------------------------------------------------------------------------------------

/** @type{(t:Closed) => Closed} */
function cSuc_(t){
  if (typeof t === 'number'){
    return t + 1
  } else {
    throw new Error('impossible')
  }
}

/** @type{(s : Closed, z: Closed, n:Closed) => Closed} */
function cNatElim_(s, z, n) {
  if (n === 0) {
    return z;
  } else if (n > 0) {
    const m = n - 1;
    return s._1(m)._1(cNatElim_(s, z, m))
  } else {
    throw new Error('impossible')
  }
}

// OPEN COMPUTATIONS
//----------------------------------------------------------------------------------------------------

/** @type {(t:Open, u:Open) => Open} */
function app_(t, u) {
    if (t.tag === _CSP) {
        // t must be a closed closure
        const v1 = /** @type{{_1: (v:Closed) => Closed, _2: (v:Open) => Open}} */ (t._1)
        if (u.tag === _CSP) {
            return CSP_(v1._1(u._1), '')
        } else {
            return v1._2(u)
        }
    } else if (t.tag === _Lam) {
        return t._2(u)
    } else {
        return App_(t, u)
    }
}

// Splice in stage 1+, no code generation
/** @type {(t:Open) => Open} */
function splice_(t) {
  if (t.tag == _CSP){
    // t must be a quoted open value
    return /** @type{Open} */ (t._1)
  } else if (t.tag === _Quote) {
    return t._1
  } else {
    return Splice_(t)
  }
}

// Quote in open evaluation
/** @type{(t:Open) => Open} */
function quote_(t){
  if (t.tag === _Splice){
    return t._1
  } else {
    return Quote_(t)
  }
}

/** @type{(t:Open, x:Name) => Open} */
function proj_(t, x){
  if (t.tag === _CSP){
    // must be a record
    return CSP_((t._1)[x], '');
  } else if (t.tag === _Rec) {
    return /** @type{Open} */ (t._1.get(x))
  } else {
    return Proj_(t, x);
  }
}

/** @type{(s : Open, z:Open, n:Open) => Open} */
function natElim_(s, z, n){
  /** @type{(n : Number) => Open} */
  function go(n){
    if (n === 0){
      return z
    } else if (n > 0) {
      const m = n - 1
      return app_(app_(s, CSP_(m, '')), go(m))
    } else {
      throw new Error('impossible')
    }
  }
  if (n.tag === _CSP){
    return go(n._1)
  } else if (n.tag == _Suc){
    return app_(app_(s, n._1), natElim_(s, z, n._1))
  } else {
    return NatElim_(s, z, n);
  }
}

/** @type{(t:Open) => Open} */
function suc_(t){
  if (t.tag === _CSP) {
    return CSP_(t._1 + 1, '')
  } else {
    return Suc_(t)
  }
}

// IO
//----------------------------------------------------------------------------------------------------

function log_(s){
  console.log(s)
  return {}
}

function readNat_(){
  const str = reader_.prompt()
  const num = parseFloat(str)
  const n   = Math.round(num)
  if (!(num === n)){
    throw new Error('Non-integral number')
  } else if (num < 0) {
    throw new Error('negative number')
  } else {
    return n
  }
}

/** @type{(t:Closed) => {}} */
function printNat_(n){
  console.log(n)
  return {}
}


// BEGIN CODE
// ----------------------------------------------------------------------------------------------------
