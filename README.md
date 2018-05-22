# Purescript ReactNative tutorial examples

[TODO]

# Running it on Android

Start your emulator or phone

```
npm install
npm run build:android
react-native run-android
```

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

# Credits

This repository contents are based on the work of [`doolse`](https://github.com/doolse)

# License & copyrights

See LICENSE file.
