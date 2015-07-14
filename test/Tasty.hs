{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC

import Utils

hunitTestHashComposition f val = HU.testCase "Composition test" $ assertEqual "No desc" f val

invariantHashTests :: TestTree
invariantHashTests = testGroup "Test that composition not changing parameter" [
        hunitTestHashComposition (hashToId . idToHash $ 100) 100,
        hunitTestHashComposition (idToHash . hashToId $ "baa") "baa",
        hunitTestHashComposition (idToHash . hashToId $ "longword") "longword"
    ]

hunitTestToHash f val = HU.testCase "idToHash test" $ assertEqual "Wrong string" f val

expectedHashGeneretionTests :: TestTree
expectedHashGeneretionTests = testGroup "Test that idToHash works as expected" [
        hunitTestToHash (idToHash 1) "b",
        hunitTestToHash (idToHash 2) "c",
        hunitTestToHash (idToHash 25) "z",
        hunitTestToHash (idToHash 26) "ba",
        hunitTestToHash (idToHash 27) "bb"
    ]

hunitTestPrefixHttp str res = HU.testCase "prefixHttp test" 
                            $ assertEqual "Wrong string" (prefixHttp str) res

prefixHttpTests :: TestTree
prefixHttpTests = testGroup "Test prefixHttp" [
        hunitTestPrefixHttp "url.test" "http://url.test",
        hunitTestPrefixHttp "https://" "https://",
        hunitTestPrefixHttp "http://test" "http://test"
    ]

allTests :: TestTree
allTests = testGroup "Tasty tests" [
        expectedHashGeneretionTests,
        invariantHashTests,
        prefixHttpTests
    ]

main :: IO ()
main = defaultMain allTests
