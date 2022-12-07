{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Amazonka.S3.Encryption.Envelope
  ( envelopeTests,
  )
where

import Amazonka.Core
import Amazonka.S3.Encryption.Envelope
import Amazonka.S3.Encryption.Types
import Control.Monad.Trans.Resource
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as Fold
import Test.Amazonka.Prelude
import Test.QuickCheck.Instances.ByteString ()
import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, arbitrary, testProperty)
import Prelude hiding (length)

envelopeTests :: [TestTree]
envelopeTests =
  [ testGroup
      "encrypt/decrypt"
      [ testCase "empty input" testEncryptEmptyInput,
        testCase "tiny input" testEncryptTinyInput,
        testCase "multiple of block size" testEncryptMultipleOfBlockSize,
        testCase "leftover" testEncryptLeftover,
        testProperty "random" testEncryptRandom
      ]
  ]

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
  testEncryptDecrypt [b, b, b]
  testEncryptDecrypt [b, b <> b <> b, b <> b]

testEncryptLeftover :: Assertion
testEncryptLeftover = do
  let b = BS.pack $ replicate aesBlockSize 170
  testEncryptDecrypt [b, "a"]
  testEncryptDecrypt [b, b, b, "a"]
  testEncryptDecrypt [b, b <> b <> b, b <> b, "abc"]

testEncryptRandom :: Property
testEncryptRandom = QC.monadicIO $
  QC.forAllM arbitrary $ \b ->
    QC.run $ testEncryptDecrypt b

testEncryptDecrypt :: [ByteString] -> Assertion
testEncryptDecrypt bs = do
  let e = mkTestAESV2Envelope
      rqb = ChunkedBody defaultChunkSize (Fold.sum (fromIntegral . BS.length <$> bs)) (CL.sourceList bs)
      (Chunked ChunkedBody {body, length}) = bodyEncrypt e (Chunked rqb)

  length `mod` fromIntegral aesBlockSize @?= 0

  encRes <- runResourceT . runConduit $ body .| CL.consume

  let ResponseBody {body = rsb} =
        bodyDecrypt e $ ResponseBody $ CL.sourceList encRes

  decRes <- runResourceT . runConduit $ rsb .| CL.consume

  mconcat bs @?= mconcat decRes

mkTestAESV2Envelope :: Envelope
mkTestAESV2Envelope =
  let (CryptoPassed aes) = cipherInit ("0123456789abcdef0123456789abcdef" :: ByteString) :: CryptoFailable AES256

      e =
        V2Envelope
          { _v2Key = "don't care",
            _v2IV = nullIV,
            _v2CEKAlgorithm = AES_CBC_PKCS5Padding,
            _v2WrapAlgorithm = KMSWrap,
            _v2Description = Description mempty
          }
   in V2 aes e
