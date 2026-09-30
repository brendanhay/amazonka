{-# LANGUAGE TypeFamilies #-}

module Test.Amazonka.S3.Encryption.Encrypt (encryptTests) where

import Amazonka.Core hiding (error)
import Amazonka.S3.Encryption.Encrypt
import Amazonka.S3.Encryption.Types
import Control.Exception (ErrorCall (..), evaluate, try)
import Test.Amazonka.S3.Encryption.Envelope (mkTestAESV2Envelope)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Prelude

encryptTests :: TestTree
encryptTests =
  testGroup
    "encrypt"
    [testCase "preserves response evaluation policy" testResponseEvaluation]

data EvaluationProbe = EvaluationProbe

newtype EvaluationResponse = EvaluationResponse Int

instance AWSRequest EvaluationProbe where
  type AWSResponse EvaluationProbe = EvaluationResponse

  evaluateResponse _ (EvaluationResponse value) = value `seq` ()

  request = error "request is not used by this test"

  response = error "response is not used by this test"

testResponseEvaluation :: IO ()
testResponseEvaluation = do
  let encryptedRequest = Encrypted EvaluationProbe [] Discard mkTestAESV2Envelope
      evaluationResponse = EvaluationResponse (error "encrypted response was forced")
  result <-
    try (evaluate (evaluateResponse encryptedRequest evaluationResponse)) ::
      IO (Either ErrorCall ())
  case result of
    Left (ErrorCall exceptionMessage) ->
      assertEqual "unexpected evaluation exception" "encrypted response was forced" exceptionMessage
    Right () -> assertFailure "expected the wrapped response policy to be used"
