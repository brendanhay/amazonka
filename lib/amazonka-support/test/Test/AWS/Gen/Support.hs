{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Support where

import Data.Proxy
import Network.AWS.Support
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Support.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRefreshTrustedAdvisorCheck $
--             mkRefreshTrustedAdvisorCheck
--
--         , requestDescribeCases $
--             mkDescribeCases
--
--         , requestDescribeTrustedAdvisorCheckRefreshStatuses $
--             mkDescribeTrustedAdvisorCheckRefreshStatuses
--
--         , requestDescribeTrustedAdvisorCheckSummaries $
--             mkDescribeTrustedAdvisorCheckSummaries
--
--         , requestCreateCase $
--             mkCreateCase
--
--         , requestResolveCase $
--             mkResolveCase
--
--         , requestDescribeSeverityLevels $
--             mkDescribeSeverityLevels
--
--         , requestDescribeTrustedAdvisorChecks $
--             mkDescribeTrustedAdvisorChecks
--
--         , requestDescribeAttachment $
--             mkDescribeAttachment
--
--         , requestAddAttachmentsToSet $
--             mkAddAttachmentsToSet
--
--         , requestDescribeTrustedAdvisorCheckResult $
--             mkDescribeTrustedAdvisorCheckResult
--
--         , requestDescribeServices $
--             mkDescribeServices
--
--         , requestDescribeCommunications $
--             mkDescribeCommunications
--
--         , requestAddCommunicationToCase $
--             mkAddCommunicationToCase
--
--           ]

--     , testGroup "response"
--         [ responseRefreshTrustedAdvisorCheck $
--             mkRefreshTrustedAdvisorCheckResponse
--
--         , responseDescribeCases $
--             mkDescribeCasesResponse
--
--         , responseDescribeTrustedAdvisorCheckRefreshStatuses $
--             mkDescribeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , responseDescribeTrustedAdvisorCheckSummaries $
--             mkDescribeTrustedAdvisorCheckSummariesResponse
--
--         , responseCreateCase $
--             mkCreateCaseResponse
--
--         , responseResolveCase $
--             mkResolveCaseResponse
--
--         , responseDescribeSeverityLevels $
--             mkDescribeSeverityLevelsResponse
--
--         , responseDescribeTrustedAdvisorChecks $
--             mkDescribeTrustedAdvisorChecksResponse
--
--         , responseDescribeAttachment $
--             mkDescribeAttachmentResponse
--
--         , responseAddAttachmentsToSet $
--             mkAddAttachmentsToSetResponse
--
--         , responseDescribeTrustedAdvisorCheckResult $
--             mkDescribeTrustedAdvisorCheckResultResponse
--
--         , responseDescribeServices $
--             mkDescribeServicesResponse
--
--         , responseDescribeCommunications $
--             mkDescribeCommunicationsResponse
--
--         , responseAddCommunicationToCase $
--             mkAddCommunicationToCaseResponse
--
--           ]
--     ]

-- Requests

requestRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheck -> TestTree
requestRefreshTrustedAdvisorCheck =
  req
    "RefreshTrustedAdvisorCheck"
    "fixture/RefreshTrustedAdvisorCheck.yaml"

requestDescribeCases :: DescribeCases -> TestTree
requestDescribeCases =
  req
    "DescribeCases"
    "fixture/DescribeCases.yaml"

requestDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
requestDescribeTrustedAdvisorCheckRefreshStatuses =
  req
    "DescribeTrustedAdvisorCheckRefreshStatuses"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatuses.yaml"

requestDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
requestDescribeTrustedAdvisorCheckSummaries =
  req
    "DescribeTrustedAdvisorCheckSummaries"
    "fixture/DescribeTrustedAdvisorCheckSummaries.yaml"

requestCreateCase :: CreateCase -> TestTree
requestCreateCase =
  req
    "CreateCase"
    "fixture/CreateCase.yaml"

requestResolveCase :: ResolveCase -> TestTree
requestResolveCase =
  req
    "ResolveCase"
    "fixture/ResolveCase.yaml"

requestDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
requestDescribeSeverityLevels =
  req
    "DescribeSeverityLevels"
    "fixture/DescribeSeverityLevels.yaml"

requestDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
requestDescribeTrustedAdvisorChecks =
  req
    "DescribeTrustedAdvisorChecks"
    "fixture/DescribeTrustedAdvisorChecks.yaml"

requestDescribeAttachment :: DescribeAttachment -> TestTree
requestDescribeAttachment =
  req
    "DescribeAttachment"
    "fixture/DescribeAttachment.yaml"

requestAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
requestAddAttachmentsToSet =
  req
    "AddAttachmentsToSet"
    "fixture/AddAttachmentsToSet.yaml"

requestDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
requestDescribeTrustedAdvisorCheckResult =
  req
    "DescribeTrustedAdvisorCheckResult"
    "fixture/DescribeTrustedAdvisorCheckResult.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDescribeCommunications :: DescribeCommunications -> TestTree
requestDescribeCommunications =
  req
    "DescribeCommunications"
    "fixture/DescribeCommunications.yaml"

requestAddCommunicationToCase :: AddCommunicationToCase -> TestTree
requestAddCommunicationToCase =
  req
    "AddCommunicationToCase"
    "fixture/AddCommunicationToCase.yaml"

-- Responses

responseRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheckResponse -> TestTree
responseRefreshTrustedAdvisorCheck =
  res
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

responseDescribeCases :: DescribeCasesResponse -> TestTree
responseDescribeCases =
  res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCases)

responseDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
responseDescribeTrustedAdvisorCheckRefreshStatuses =
  res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

responseDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
responseDescribeTrustedAdvisorCheckSummaries =
  res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

responseCreateCase :: CreateCaseResponse -> TestTree
responseCreateCase =
  res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCase)

responseResolveCase :: ResolveCaseResponse -> TestTree
responseResolveCase =
  res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResolveCase)

responseDescribeSeverityLevels :: DescribeSeverityLevelsResponse -> TestTree
responseDescribeSeverityLevels =
  res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSeverityLevels)

responseDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecksResponse -> TestTree
responseDescribeTrustedAdvisorChecks =
  res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

responseDescribeAttachment :: DescribeAttachmentResponse -> TestTree
responseDescribeAttachment =
  res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAttachment)

responseAddAttachmentsToSet :: AddAttachmentsToSetResponse -> TestTree
responseAddAttachmentsToSet =
  res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddAttachmentsToSet)

responseDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
responseDescribeTrustedAdvisorCheckResult =
  res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeServices)

responseDescribeCommunications :: DescribeCommunicationsResponse -> TestTree
responseDescribeCommunications =
  res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCommunications)

responseAddCommunicationToCase :: AddCommunicationToCaseResponse -> TestTree
responseAddCommunicationToCase =
  res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddCommunicationToCase)
