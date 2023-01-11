{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Support
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Support where

import Amazonka.Support
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Support.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddAttachmentsToSet $
--             newAddAttachmentsToSet
--
--         , requestAddCommunicationToCase $
--             newAddCommunicationToCase
--
--         , requestCreateCase $
--             newCreateCase
--
--         , requestDescribeAttachment $
--             newDescribeAttachment
--
--         , requestDescribeCases $
--             newDescribeCases
--
--         , requestDescribeCommunications $
--             newDescribeCommunications
--
--         , requestDescribeServices $
--             newDescribeServices
--
--         , requestDescribeSeverityLevels $
--             newDescribeSeverityLevels
--
--         , requestDescribeTrustedAdvisorCheckRefreshStatuses $
--             newDescribeTrustedAdvisorCheckRefreshStatuses
--
--         , requestDescribeTrustedAdvisorCheckResult $
--             newDescribeTrustedAdvisorCheckResult
--
--         , requestDescribeTrustedAdvisorCheckSummaries $
--             newDescribeTrustedAdvisorCheckSummaries
--
--         , requestDescribeTrustedAdvisorChecks $
--             newDescribeTrustedAdvisorChecks
--
--         , requestRefreshTrustedAdvisorCheck $
--             newRefreshTrustedAdvisorCheck
--
--         , requestResolveCase $
--             newResolveCase
--
--           ]

--     , testGroup "response"
--         [ responseAddAttachmentsToSet $
--             newAddAttachmentsToSetResponse
--
--         , responseAddCommunicationToCase $
--             newAddCommunicationToCaseResponse
--
--         , responseCreateCase $
--             newCreateCaseResponse
--
--         , responseDescribeAttachment $
--             newDescribeAttachmentResponse
--
--         , responseDescribeCases $
--             newDescribeCasesResponse
--
--         , responseDescribeCommunications $
--             newDescribeCommunicationsResponse
--
--         , responseDescribeServices $
--             newDescribeServicesResponse
--
--         , responseDescribeSeverityLevels $
--             newDescribeSeverityLevelsResponse
--
--         , responseDescribeTrustedAdvisorCheckRefreshStatuses $
--             newDescribeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , responseDescribeTrustedAdvisorCheckResult $
--             newDescribeTrustedAdvisorCheckResultResponse
--
--         , responseDescribeTrustedAdvisorCheckSummaries $
--             newDescribeTrustedAdvisorCheckSummariesResponse
--
--         , responseDescribeTrustedAdvisorChecks $
--             newDescribeTrustedAdvisorChecksResponse
--
--         , responseRefreshTrustedAdvisorCheck $
--             newRefreshTrustedAdvisorCheckResponse
--
--         , responseResolveCase $
--             newResolveCaseResponse
--
--           ]
--     ]

-- Requests

requestAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
requestAddAttachmentsToSet =
  req
    "AddAttachmentsToSet"
    "fixture/AddAttachmentsToSet.yaml"

requestAddCommunicationToCase :: AddCommunicationToCase -> TestTree
requestAddCommunicationToCase =
  req
    "AddCommunicationToCase"
    "fixture/AddCommunicationToCase.yaml"

requestCreateCase :: CreateCase -> TestTree
requestCreateCase =
  req
    "CreateCase"
    "fixture/CreateCase.yaml"

requestDescribeAttachment :: DescribeAttachment -> TestTree
requestDescribeAttachment =
  req
    "DescribeAttachment"
    "fixture/DescribeAttachment.yaml"

requestDescribeCases :: DescribeCases -> TestTree
requestDescribeCases =
  req
    "DescribeCases"
    "fixture/DescribeCases.yaml"

requestDescribeCommunications :: DescribeCommunications -> TestTree
requestDescribeCommunications =
  req
    "DescribeCommunications"
    "fixture/DescribeCommunications.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices =
  req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
requestDescribeSeverityLevels =
  req
    "DescribeSeverityLevels"
    "fixture/DescribeSeverityLevels.yaml"

requestDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
requestDescribeTrustedAdvisorCheckRefreshStatuses =
  req
    "DescribeTrustedAdvisorCheckRefreshStatuses"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatuses.yaml"

requestDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
requestDescribeTrustedAdvisorCheckResult =
  req
    "DescribeTrustedAdvisorCheckResult"
    "fixture/DescribeTrustedAdvisorCheckResult.yaml"

requestDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
requestDescribeTrustedAdvisorCheckSummaries =
  req
    "DescribeTrustedAdvisorCheckSummaries"
    "fixture/DescribeTrustedAdvisorCheckSummaries.yaml"

requestDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
requestDescribeTrustedAdvisorChecks =
  req
    "DescribeTrustedAdvisorChecks"
    "fixture/DescribeTrustedAdvisorChecks.yaml"

requestRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheck -> TestTree
requestRefreshTrustedAdvisorCheck =
  req
    "RefreshTrustedAdvisorCheck"
    "fixture/RefreshTrustedAdvisorCheck.yaml"

requestResolveCase :: ResolveCase -> TestTree
requestResolveCase =
  req
    "ResolveCase"
    "fixture/ResolveCase.yaml"

-- Responses

responseAddAttachmentsToSet :: AddAttachmentsToSetResponse -> TestTree
responseAddAttachmentsToSet =
  res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddAttachmentsToSet)

responseAddCommunicationToCase :: AddCommunicationToCaseResponse -> TestTree
responseAddCommunicationToCase =
  res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddCommunicationToCase)

responseCreateCase :: CreateCaseResponse -> TestTree
responseCreateCase =
  res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCase)

responseDescribeAttachment :: DescribeAttachmentResponse -> TestTree
responseDescribeAttachment =
  res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAttachment)

responseDescribeCases :: DescribeCasesResponse -> TestTree
responseDescribeCases =
  res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCases)

responseDescribeCommunications :: DescribeCommunicationsResponse -> TestTree
responseDescribeCommunications =
  res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCommunications)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices =
  res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServices)

responseDescribeSeverityLevels :: DescribeSeverityLevelsResponse -> TestTree
responseDescribeSeverityLevels =
  res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSeverityLevels)

responseDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
responseDescribeTrustedAdvisorCheckRefreshStatuses =
  res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

responseDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
responseDescribeTrustedAdvisorCheckResult =
  res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrustedAdvisorCheckResult)

responseDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
responseDescribeTrustedAdvisorCheckSummaries =
  res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrustedAdvisorCheckSummaries)

responseDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecksResponse -> TestTree
responseDescribeTrustedAdvisorChecks =
  res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrustedAdvisorChecks)

responseRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheckResponse -> TestTree
responseRefreshTrustedAdvisorCheck =
  res
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RefreshTrustedAdvisorCheck)

responseResolveCase :: ResolveCaseResponse -> TestTree
responseResolveCase =
  res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveCase)
