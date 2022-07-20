{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Textract
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Textract where

import Amazonka.Textract
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Textract.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAnalyzeDocument $
--             newAnalyzeDocument
--
--         , requestAnalyzeExpense $
--             newAnalyzeExpense
--
--         , requestDetectDocumentText $
--             newDetectDocumentText
--
--         , requestGetDocumentAnalysis $
--             newGetDocumentAnalysis
--
--         , requestGetDocumentTextDetection $
--             newGetDocumentTextDetection
--
--         , requestStartDocumentAnalysis $
--             newStartDocumentAnalysis
--
--         , requestStartDocumentTextDetection $
--             newStartDocumentTextDetection
--
--           ]

--     , testGroup "response"
--         [ responseAnalyzeDocument $
--             newAnalyzeDocumentResponse
--
--         , responseAnalyzeExpense $
--             newAnalyzeExpenseResponse
--
--         , responseDetectDocumentText $
--             newDetectDocumentTextResponse
--
--         , responseGetDocumentAnalysis $
--             newGetDocumentAnalysisResponse
--
--         , responseGetDocumentTextDetection $
--             newGetDocumentTextDetectionResponse
--
--         , responseStartDocumentAnalysis $
--             newStartDocumentAnalysisResponse
--
--         , responseStartDocumentTextDetection $
--             newStartDocumentTextDetectionResponse
--
--           ]
--     ]

-- Requests

requestAnalyzeDocument :: AnalyzeDocument -> TestTree
requestAnalyzeDocument =
  req
    "AnalyzeDocument"
    "fixture/AnalyzeDocument.yaml"

requestAnalyzeExpense :: AnalyzeExpense -> TestTree
requestAnalyzeExpense =
  req
    "AnalyzeExpense"
    "fixture/AnalyzeExpense.yaml"

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText =
  req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

requestGetDocumentAnalysis :: GetDocumentAnalysis -> TestTree
requestGetDocumentAnalysis =
  req
    "GetDocumentAnalysis"
    "fixture/GetDocumentAnalysis.yaml"

requestGetDocumentTextDetection :: GetDocumentTextDetection -> TestTree
requestGetDocumentTextDetection =
  req
    "GetDocumentTextDetection"
    "fixture/GetDocumentTextDetection.yaml"

requestStartDocumentAnalysis :: StartDocumentAnalysis -> TestTree
requestStartDocumentAnalysis =
  req
    "StartDocumentAnalysis"
    "fixture/StartDocumentAnalysis.yaml"

requestStartDocumentTextDetection :: StartDocumentTextDetection -> TestTree
requestStartDocumentTextDetection =
  req
    "StartDocumentTextDetection"
    "fixture/StartDocumentTextDetection.yaml"

-- Responses

responseAnalyzeDocument :: AnalyzeDocumentResponse -> TestTree
responseAnalyzeDocument =
  res
    "AnalyzeDocumentResponse"
    "fixture/AnalyzeDocumentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AnalyzeDocument)

responseAnalyzeExpense :: AnalyzeExpenseResponse -> TestTree
responseAnalyzeExpense =
  res
    "AnalyzeExpenseResponse"
    "fixture/AnalyzeExpenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AnalyzeExpense)

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText =
  res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectDocumentText)

responseGetDocumentAnalysis :: GetDocumentAnalysisResponse -> TestTree
responseGetDocumentAnalysis =
  res
    "GetDocumentAnalysisResponse"
    "fixture/GetDocumentAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentAnalysis)

responseGetDocumentTextDetection :: GetDocumentTextDetectionResponse -> TestTree
responseGetDocumentTextDetection =
  res
    "GetDocumentTextDetectionResponse"
    "fixture/GetDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDocumentTextDetection)

responseStartDocumentAnalysis :: StartDocumentAnalysisResponse -> TestTree
responseStartDocumentAnalysis =
  res
    "StartDocumentAnalysisResponse"
    "fixture/StartDocumentAnalysisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentAnalysis)

responseStartDocumentTextDetection :: StartDocumentTextDetectionResponse -> TestTree
responseStartDocumentTextDetection =
  res
    "StartDocumentTextDetectionResponse"
    "fixture/StartDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDocumentTextDetection)
