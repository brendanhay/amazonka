{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.AWS.S3
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.S3.Encryption.Envelope
    ( envelopeTests
    ) where

import           Control.Monad.Trans.Resource
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Data.Padding
import           Crypto.Error
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Fold
import           Network.AWS.Prelude
import           Network.AWS.S3.Encryption.Body
import           Network.AWS.S3.Encryption.Envelope
import           Network.AWS.S3.Encryption.Types
import           Test.AWS.Prelude
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.QuickCheck.Unicode as Unicode
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (Arbitrary, Gen, Property, testProperty, arbitrary)


envelopeTests :: [TestTree]
envelopeTests =
    [ testGroup "block align chunks"
        [ testCase "empty" testBlockAlignChunksEmpty
        , testCase "one empty chunk" testBlockAlignChunksOneEmptyChunk
        , testCase "collapse empty chunks" testBlockAlignChunksCollapsEmptyChunks
        , testCase "one small chunk" testBlockAlignChunksOneSmallChunk
        , testCase "misaligned chunks" testBlockAlignChunksMisalignedChunks
        , testCase "already aligned chunks" testBlockAlignChunksAlreadyAlignedChunks
        , testCase "leftover" testBlockAlignChunksLeftover
        ]
    , testGroup "encrypt/decrypt"
        [ testCase "empty input" testEncryptEmptyInput
        , testCase "tiny input" testEncryptTinyInput
        , testCase "multiple of block size" testEncryptMultipleOfBlockSize
        , testCase "leftover" testEncryptLeftover
        , testProperty "random" testEncryptRandom
        ]
    ]

testBlockAlignChunksEmpty :: Assertion
testBlockAlignChunksEmpty = do
    res <- runConduit (return () =$= blockAlignChunks 4 $$ CL.consume)
    res @?= []


testBlockAlignChunksOneEmptyChunk :: Assertion
testBlockAlignChunksOneEmptyChunk = do
    res <- runConduit (CL.sourceList [""] =$= blockAlignChunks 4 $$ CL.consume)
    res @?= [""]


testBlockAlignChunksCollapsEmptyChunks :: Assertion
testBlockAlignChunksCollapsEmptyChunks = do
    res <- runConduit (CL.sourceList ["", "", ""] =$= blockAlignChunks 4 $$ CL.consume)
    res @?= [""]


testBlockAlignChunksOneSmallChunk :: Assertion
testBlockAlignChunksOneSmallChunk = do
    res <- runConduit (CL.sourceList ["abc"] =$= blockAlignChunks 4 $$ CL.consume)
    res @?= ["abc"]


testBlockAlignChunksMisalignedChunks :: Assertion
testBlockAlignChunksMisalignedChunks = do
    let input = ["a", "aabbbc", "cc", "dddeeefff"]
    res <- runConduit (CL.sourceList input =$= blockAlignChunks 3 $$ CL.consume)
    res @?= ["aaabbb", "ccc", "dddeeefff"]


testBlockAlignChunksAlreadyAlignedChunks :: Assertion
testBlockAlignChunksAlreadyAlignedChunks = do
    let input = ["aaa", "bbbcccddd", "eee"]
    res <- runConduit (CL.sourceList input =$= blockAlignChunks 3 $$ CL.consume)
    res @?= ["aaa", "bbbcccddd", "eee"]


testBlockAlignChunksLeftover :: Assertion
testBlockAlignChunksLeftover = do
    let input = ["aaabbbcccd"]
    res <- runConduit (CL.sourceList input =$= blockAlignChunks 3 $$ CL.consume)
    res @?= ["aaabbbccc", "d"]


testEncryptEmptyInput :: Assertion
testEncryptEmptyInput = do
    testEncryptDecrypt [""]
    testEncryptDecrypt ["", ""]


testEncryptTinyInput :: Assertion
testEncryptTinyInput = do
    testEncryptDecrypt ["a"]
    testEncryptDecrypt ["a", "b", "c"]


testEncryptMultipleOfBlockSize :: Assertion
testEncryptMultipleOfBlockSize = do
    let b = BS.pack $ replicate aesBlockSize 170
    testEncryptDecrypt [b]
    testEncryptDecrypt [b,b,b]
    testEncryptDecrypt [b, b<>b<>b, b<>b]


testEncryptLeftover :: Assertion
testEncryptLeftover = do
    let b = BS.pack $ replicate aesBlockSize 170
    testEncryptDecrypt [b, "a"]
    testEncryptDecrypt [b, b, b, "a"]
    testEncryptDecrypt [b, b<>b<>b, b<>b, "abc"]


instance Arbitrary ByteString where
    arbitrary = BS8.pack <$> Unicode.string


testEncryptRandom :: Property
testEncryptRandom = QC.monadicIO $ QC.forAllM arbitrary $ \ b ->
    QC.run $ testEncryptDecrypt b


testEncryptDecrypt :: [ByteString] -> Assertion
testEncryptDecrypt bs = do
    let e = mkTestAESV2Envelope
        rqb = ChunkedBody defaultChunkSize (Fold.sum (fromIntegral . BS.length <$> bs)) (CL.sourceList bs)
        (Chunked encBody) = bodyEncrypt e (Chunked rqb)

    (encBody ^. chunkedLength) `mod` fromIntegral aesBlockSize  @?= 0

    encRes <- runResourceT (_chunkedBody encBody $$ CL.consume)

    let rsb = bodyDecrypt e $ RsBody $ (newResumableSource . CL.sourceList) encRes

    (_, decRes) <- runResourceT (_streamBody rsb $$++ CL.consume)

    mconcat bs @?= mconcat decRes


mkTestAESV2Envelope :: Envelope
mkTestAESV2Envelope =
    let (CryptoPassed aes) = cipherInit ("0123456789abcdef0123456789abcdef" :: ByteString) :: CryptoFailable AES256

        e = V2Envelope { _v2Key           = "don't care"
                       , _v2IV            = nullIV
                       , _v2CEKAlgorithm  = AES_CBC_PKCS5Padding
                       , _v2WrapAlgorithm = KMSWrap
                       , _v2Description   = Description mempty
                       }
     in V2 aes e
