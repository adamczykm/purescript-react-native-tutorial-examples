# Purescript ReactNative tutorial examples

This repository contains some introductory examples on how to use Purescript's ReactNative bindings to walk through official tutorial, plus some additional simple ones.
It's compiled into a single mobile app where you can run each example.

# Running it on Android

Start your emulator or phone. Then:

```
npm install
npm run build:android
```

If you have your Android Studio prepared and device connected you can try with:

```
react-native run-android
```

If not, take a look at the official guideline at:
https://facebook.github.io/react-native/docs/getting-started.html


You may also have to start the react-native dev server with (in the repo dir):

```
react-native start
```

also you may need to proxy the dev server with:

```
adb reverse tcp:8081 tcp:8081
```

# Running on iOS

```bash
yarn install
yarn start
yarn build:ios
```

Some notes:
- If you have some issues to fetch data with Simulator set `Allow Arbitrary Loads` to `YES` in `info.plist` (via http://stackoverflow.com/a/38219454/2032698)
- If you might have some issues upgrading to React Native `0.40.0` (such as `'React/RCTEventEmitter.h' file not found` or similar) clean your XCode project and build core `React` in XCode as described by @LoopIndigo in  [issue #12042](https://github.com/facebook/react-native/issues/12042#issuecomment-275025960)

# Issues 

Should you have any issues setting up the project, consult: https://facebook.github.io/react-native/docs/getting-started.html
Sometimes you can resolve some of them by killing your app and reopening it on running device.

# Changing and reloading the code

All you have to do to apply changes to the app is calling build command (compiling purescript and bundling it into index.js) and reloading the simulator.

# Credits

This repository contents are based on the work of [`doolse`](https://github.com/doolse)

# License & copyrights

See LICENSE file.
