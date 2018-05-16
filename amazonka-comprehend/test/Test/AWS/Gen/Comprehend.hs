{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Comprehend
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Comprehend where

import Data.Proxy
import Network.AWS.Comprehend
import Test.AWS.Comprehend.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchDetectSentiment $
--             batchDetectSentiment
--
--         , requestStartTopicsDetectionJob $
--             startTopicsDetectionJob
--
--         , requestBatchDetectKeyPhrases $
--             batchDetectKeyPhrases
--
--         , requestDetectSentiment $
--             detectSentiment
--
--         , requestBatchDetectEntities $
--             batchDetectEntities
--
--         , requestDetectDominantLanguage $
--             detectDominantLanguage
--
--         , requestDescribeTopicsDetectionJob $
--             describeTopicsDetectionJob
--
--         , requestDetectEntities $
--             detectEntities
--
--         , requestListTopicsDetectionJobs $
--             listTopicsDetectionJobs
--
--         , requestBatchDetectDominantLanguage $
--             batchDetectDominantLanguage
--
--         , requestDetectKeyPhrases $
--             detectKeyPhrases
--
--           ]

--     , testGroup "response"
--         [ responseBatchDetectSentiment $
--             batchDetectSentimentResponse
--
--         , responseStartTopicsDetectionJob $
--             startTopicsDetectionJobResponse
--
--         , responseBatchDetectKeyPhrases $
--             batchDetectKeyPhrasesResponse
--
--         , responseDetectSentiment $
--             detectSentimentResponse
--
--         , responseBatchDetectEntities $
--             batchDetectEntitiesResponse
--
--         , responseDetectDominantLanguage $
--             detectDominantLanguageResponse
--
--         , responseDescribeTopicsDetectionJob $
--             describeTopicsDetectionJobResponse
--
--         , responseDetectEntities $
--             detectEntitiesResponse
--
--         , responseListTopicsDetectionJobs $
--             listTopicsDetectionJobsResponse
--
--         , responseBatchDetectDominantLanguage $
--             batchDetectDominantLanguageResponse
--
--         , responseDetectKeyPhrases $
--             detectKeyPhrasesResponse
--
--           ]
--     ]

-- Requests

requestBatchDetectSentiment :: BatchDetectSentiment -> TestTree
requestBatchDetectSentiment = req
    "BatchDetectSentiment"
    "fixture/BatchDetectSentiment.yaml"

requestStartTopicsDetectionJob :: StartTopicsDetectionJob -> TestTree
requestStartTopicsDetectionJob = req
    "StartTopicsDetectionJob"
    "fixture/StartTopicsDetectionJob.yaml"

requestBatchDetectKeyPhrases :: BatchDetectKeyPhrases -> TestTree
requestBatchDetectKeyPhrases = req
    "BatchDetectKeyPhrases"
    "fixture/BatchDetectKeyPhrases.yaml"

requestDetectSentiment :: DetectSentiment -> TestTree
requestDetectSentiment = req
    "DetectSentiment"
    "fixture/DetectSentiment.yaml"

requestBatchDetectEntities :: BatchDetectEntities -> TestTree
requestBatchDetectEntities = req
    "BatchDetectEntities"
    "fixture/BatchDetectEntities.yaml"

requestDetectDominantLanguage :: DetectDominantLanguage -> TestTree
requestDetectDominantLanguage = req
    "DetectDominantLanguage"
    "fixture/DetectDominantLanguage.yaml"

requestDescribeTopicsDetectionJob :: DescribeTopicsDetectionJob -> TestTree
requestDescribeTopicsDetectionJob = req
    "DescribeTopicsDetectionJob"
    "fixture/DescribeTopicsDetectionJob.yaml"

requestDetectEntities :: DetectEntities -> TestTree
requestDetectEntities = req
    "DetectEntities"
    "fixture/DetectEntities.yaml"

requestListTopicsDetectionJobs :: ListTopicsDetectionJobs -> TestTree
requestListTopicsDetectionJobs = req
    "ListTopicsDetectionJobs"
    "fixture/ListTopicsDetectionJobs.yaml"

requestBatchDetectDominantLanguage :: BatchDetectDominantLanguage -> TestTree
requestBatchDetectDominantLanguage = req
    "BatchDetectDominantLanguage"
    "fixture/BatchDetectDominantLanguage.yaml"

requestDetectKeyPhrases :: DetectKeyPhrases -> TestTree
requestDetectKeyPhrases = req
    "DetectKeyPhrases"
    "fixture/DetectKeyPhrases.yaml"

-- Responses

responseBatchDetectSentiment :: BatchDetectSentimentResponse -> TestTree
responseBatchDetectSentiment = res
    "BatchDetectSentimentResponse"
    "fixture/BatchDetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectSentiment)

responseStartTopicsDetectionJob :: StartTopicsDetectionJobResponse -> TestTree
responseStartTopicsDetectionJob = res
    "StartTopicsDetectionJobResponse"
    "fixture/StartTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy StartTopicsDetectionJob)

responseBatchDetectKeyPhrases :: BatchDetectKeyPhrasesResponse -> TestTree
responseBatchDetectKeyPhrases = res
    "BatchDetectKeyPhrasesResponse"
    "fixture/BatchDetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectKeyPhrases)

responseDetectSentiment :: DetectSentimentResponse -> TestTree
responseDetectSentiment = res
    "DetectSentimentResponse"
    "fixture/DetectSentimentResponse.proto"
    comprehend
    (Proxy :: Proxy DetectSentiment)

responseBatchDetectEntities :: BatchDetectEntitiesResponse -> TestTree
responseBatchDetectEntities = res
    "BatchDetectEntitiesResponse"
    "fixture/BatchDetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectEntities)

responseDetectDominantLanguage :: DetectDominantLanguageResponse -> TestTree
responseDetectDominantLanguage = res
    "DetectDominantLanguageResponse"
    "fixture/DetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy DetectDominantLanguage)

responseDescribeTopicsDetectionJob :: DescribeTopicsDetectionJobResponse -> TestTree
responseDescribeTopicsDetectionJob = res
    "DescribeTopicsDetectionJobResponse"
    "fixture/DescribeTopicsDetectionJobResponse.proto"
    comprehend
    (Proxy :: Proxy DescribeTopicsDetectionJob)

responseDetectEntities :: DetectEntitiesResponse -> TestTree
responseDetectEntities = res
    "DetectEntitiesResponse"
    "fixture/DetectEntitiesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectEntities)

responseListTopicsDetectionJobs :: ListTopicsDetectionJobsResponse -> TestTree
responseListTopicsDetectionJobs = res
    "ListTopicsDetectionJobsResponse"
    "fixture/ListTopicsDetectionJobsResponse.proto"
    comprehend
    (Proxy :: Proxy ListTopicsDetectionJobs)

responseBatchDetectDominantLanguage :: BatchDetectDominantLanguageResponse -> TestTree
responseBatchDetectDominantLanguage = res
    "BatchDetectDominantLanguageResponse"
    "fixture/BatchDetectDominantLanguageResponse.proto"
    comprehend
    (Proxy :: Proxy BatchDetectDominantLanguage)

responseDetectKeyPhrases :: DetectKeyPhrasesResponse -> TestTree
responseDetectKeyPhrases = res
    "DetectKeyPhrasesResponse"
    "fixture/DetectKeyPhrasesResponse.proto"
    comprehend
    (Proxy :: Proxy DetectKeyPhrases)
