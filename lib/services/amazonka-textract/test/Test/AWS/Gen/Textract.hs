{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Textract
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Textract where

import Data.Proxy
import Network.AWS.Textract
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Textract.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDetectDocumentText $
--             newDetectDocumentText
--
--         , requestStartDocumentAnalysis $
--             newStartDocumentAnalysis
--
--         , requestAnalyzeDocument $
--             newAnalyzeDocument
--
--         , requestGetDocumentTextDetection $
--             newGetDocumentTextDetection
--
--         , requestAnalyzeExpense $
--             newAnalyzeExpense
--
--         , requestStartDocumentTextDetection $
--             newStartDocumentTextDetection
--
--         , requestGetDocumentAnalysis $
--             newGetDocumentAnalysis
--
--           ]

--     , testGroup "response"
--         [ responseDetectDocumentText $
--             newDetectDocumentTextResponse
--
--         , responseStartDocumentAnalysis $
--             newStartDocumentAnalysisResponse
--
--         , responseAnalyzeDocument $
--             newAnalyzeDocumentResponse
--
--         , responseGetDocumentTextDetection $
--             newGetDocumentTextDetectionResponse
--
--         , responseAnalyzeExpense $
--             newAnalyzeExpenseResponse
--
--         , responseStartDocumentTextDetection $
--             newStartDocumentTextDetectionResponse
--
--         , responseGetDocumentAnalysis $
--             newGetDocumentAnalysisResponse
--
--           ]
--     ]

-- Requests

requestDetectDocumentText :: DetectDocumentText -> TestTree
requestDetectDocumentText =
  req
    "DetectDocumentText"
    "fixture/DetectDocumentText.yaml"

requestStartDocumentAnalysis :: StartDocumentAnalysis -> TestTree
requestStartDocumentAnalysis =
  req
    "StartDocumentAnalysis"
    "fixture/StartDocumentAnalysis.yaml"

requestAnalyzeDocument :: AnalyzeDocument -> TestTree
requestAnalyzeDocument =
  req
    "AnalyzeDocument"
    "fixture/AnalyzeDocument.yaml"

requestGetDocumentTextDetection :: GetDocumentTextDetection -> TestTree
requestGetDocumentTextDetection =
  req
    "GetDocumentTextDetection"
    "fixture/GetDocumentTextDetection.yaml"

requestAnalyzeExpense :: AnalyzeExpense -> TestTree
requestAnalyzeExpense =
  req
    "AnalyzeExpense"
    "fixture/AnalyzeExpense.yaml"

requestStartDocumentTextDetection :: StartDocumentTextDetection -> TestTree
requestStartDocumentTextDetection =
  req
    "StartDocumentTextDetection"
    "fixture/StartDocumentTextDetection.yaml"

requestGetDocumentAnalysis :: GetDocumentAnalysis -> TestTree
requestGetDocumentAnalysis =
  req
    "GetDocumentAnalysis"
    "fixture/GetDocumentAnalysis.yaml"

-- Responses

responseDetectDocumentText :: DetectDocumentTextResponse -> TestTree
responseDetectDocumentText =
  res
    "DetectDocumentTextResponse"
    "fixture/DetectDocumentTextResponse.proto"
    defaultService
    (Proxy :: Proxy DetectDocumentText)

responseStartDocumentAnalysis :: StartDocumentAnalysisResponse -> TestTree
responseStartDocumentAnalysis =
  res
    "StartDocumentAnalysisResponse"
    "fixture/StartDocumentAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy StartDocumentAnalysis)

responseAnalyzeDocument :: AnalyzeDocumentResponse -> TestTree
responseAnalyzeDocument =
  res
    "AnalyzeDocumentResponse"
    "fixture/AnalyzeDocumentResponse.proto"
    defaultService
    (Proxy :: Proxy AnalyzeDocument)

responseGetDocumentTextDetection :: GetDocumentTextDetectionResponse -> TestTree
responseGetDocumentTextDetection =
  res
    "GetDocumentTextDetectionResponse"
    "fixture/GetDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentTextDetection)

responseAnalyzeExpense :: AnalyzeExpenseResponse -> TestTree
responseAnalyzeExpense =
  res
    "AnalyzeExpenseResponse"
    "fixture/AnalyzeExpenseResponse.proto"
    defaultService
    (Proxy :: Proxy AnalyzeExpense)

responseStartDocumentTextDetection :: StartDocumentTextDetectionResponse -> TestTree
responseStartDocumentTextDetection =
  res
    "StartDocumentTextDetectionResponse"
    "fixture/StartDocumentTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartDocumentTextDetection)

responseGetDocumentAnalysis :: GetDocumentAnalysisResponse -> TestTree
responseGetDocumentAnalysis =
  res
    "GetDocumentAnalysisResponse"
    "fixture/GetDocumentAnalysisResponse.proto"
    defaultService
    (Proxy :: Proxy GetDocumentAnalysis)
