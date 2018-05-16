{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             refreshTrustedAdvisorCheck
--
--         , requestDescribeCases $
--             describeCases
--
--         , requestDescribeTrustedAdvisorCheckRefreshStatuses $
--             describeTrustedAdvisorCheckRefreshStatuses
--
--         , requestDescribeTrustedAdvisorCheckSummaries $
--             describeTrustedAdvisorCheckSummaries
--
--         , requestCreateCase $
--             createCase
--
--         , requestResolveCase $
--             resolveCase
--
--         , requestDescribeSeverityLevels $
--             describeSeverityLevels
--
--         , requestDescribeTrustedAdvisorChecks $
--             describeTrustedAdvisorChecks
--
--         , requestDescribeAttachment $
--             describeAttachment
--
--         , requestAddAttachmentsToSet $
--             addAttachmentsToSet
--
--         , requestDescribeTrustedAdvisorCheckResult $
--             describeTrustedAdvisorCheckResult
--
--         , requestDescribeServices $
--             describeServices
--
--         , requestDescribeCommunications $
--             describeCommunications
--
--         , requestAddCommunicationToCase $
--             addCommunicationToCase
--
--           ]

--     , testGroup "response"
--         [ responseRefreshTrustedAdvisorCheck $
--             refreshTrustedAdvisorCheckResponse
--
--         , responseDescribeCases $
--             describeCasesResponse
--
--         , responseDescribeTrustedAdvisorCheckRefreshStatuses $
--             describeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , responseDescribeTrustedAdvisorCheckSummaries $
--             describeTrustedAdvisorCheckSummariesResponse
--
--         , responseCreateCase $
--             createCaseResponse
--
--         , responseResolveCase $
--             resolveCaseResponse
--
--         , responseDescribeSeverityLevels $
--             describeSeverityLevelsResponse
--
--         , responseDescribeTrustedAdvisorChecks $
--             describeTrustedAdvisorChecksResponse
--
--         , responseDescribeAttachment $
--             describeAttachmentResponse
--
--         , responseAddAttachmentsToSet $
--             addAttachmentsToSetResponse
--
--         , responseDescribeTrustedAdvisorCheckResult $
--             describeTrustedAdvisorCheckResultResponse
--
--         , responseDescribeServices $
--             describeServicesResponse
--
--         , responseDescribeCommunications $
--             describeCommunicationsResponse
--
--         , responseAddCommunicationToCase $
--             addCommunicationToCaseResponse
--
--           ]
--     ]

-- Requests

requestRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheck -> TestTree
requestRefreshTrustedAdvisorCheck = req
    "RefreshTrustedAdvisorCheck"
    "fixture/RefreshTrustedAdvisorCheck.yaml"

requestDescribeCases :: DescribeCases -> TestTree
requestDescribeCases = req
    "DescribeCases"
    "fixture/DescribeCases.yaml"

requestDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
requestDescribeTrustedAdvisorCheckRefreshStatuses = req
    "DescribeTrustedAdvisorCheckRefreshStatuses"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatuses.yaml"

requestDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
requestDescribeTrustedAdvisorCheckSummaries = req
    "DescribeTrustedAdvisorCheckSummaries"
    "fixture/DescribeTrustedAdvisorCheckSummaries.yaml"

requestCreateCase :: CreateCase -> TestTree
requestCreateCase = req
    "CreateCase"
    "fixture/CreateCase.yaml"

requestResolveCase :: ResolveCase -> TestTree
requestResolveCase = req
    "ResolveCase"
    "fixture/ResolveCase.yaml"

requestDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
requestDescribeSeverityLevels = req
    "DescribeSeverityLevels"
    "fixture/DescribeSeverityLevels.yaml"

requestDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
requestDescribeTrustedAdvisorChecks = req
    "DescribeTrustedAdvisorChecks"
    "fixture/DescribeTrustedAdvisorChecks.yaml"

requestDescribeAttachment :: DescribeAttachment -> TestTree
requestDescribeAttachment = req
    "DescribeAttachment"
    "fixture/DescribeAttachment.yaml"

requestAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
requestAddAttachmentsToSet = req
    "AddAttachmentsToSet"
    "fixture/AddAttachmentsToSet.yaml"

requestDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
requestDescribeTrustedAdvisorCheckResult = req
    "DescribeTrustedAdvisorCheckResult"
    "fixture/DescribeTrustedAdvisorCheckResult.yaml"

requestDescribeServices :: DescribeServices -> TestTree
requestDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

requestDescribeCommunications :: DescribeCommunications -> TestTree
requestDescribeCommunications = req
    "DescribeCommunications"
    "fixture/DescribeCommunications.yaml"

requestAddCommunicationToCase :: AddCommunicationToCase -> TestTree
requestAddCommunicationToCase = req
    "AddCommunicationToCase"
    "fixture/AddCommunicationToCase.yaml"

-- Responses

responseRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheckResponse -> TestTree
responseRefreshTrustedAdvisorCheck = res
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse.proto"
    support
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

responseDescribeCases :: DescribeCasesResponse -> TestTree
responseDescribeCases = res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse.proto"
    support
    (Proxy :: Proxy DescribeCases)

responseDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
responseDescribeTrustedAdvisorCheckRefreshStatuses = res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

responseDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
responseDescribeTrustedAdvisorCheckSummaries = res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

responseCreateCase :: CreateCaseResponse -> TestTree
responseCreateCase = res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse.proto"
    support
    (Proxy :: Proxy CreateCase)

responseResolveCase :: ResolveCaseResponse -> TestTree
responseResolveCase = res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse.proto"
    support
    (Proxy :: Proxy ResolveCase)

responseDescribeSeverityLevels :: DescribeSeverityLevelsResponse -> TestTree
responseDescribeSeverityLevels = res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse.proto"
    support
    (Proxy :: Proxy DescribeSeverityLevels)

responseDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecksResponse -> TestTree
responseDescribeTrustedAdvisorChecks = res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

responseDescribeAttachment :: DescribeAttachmentResponse -> TestTree
responseDescribeAttachment = res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse.proto"
    support
    (Proxy :: Proxy DescribeAttachment)

responseAddAttachmentsToSet :: AddAttachmentsToSetResponse -> TestTree
responseAddAttachmentsToSet = res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse.proto"
    support
    (Proxy :: Proxy AddAttachmentsToSet)

responseDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
responseDescribeTrustedAdvisorCheckResult = res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

responseDescribeServices :: DescribeServicesResponse -> TestTree
responseDescribeServices = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    support
    (Proxy :: Proxy DescribeServices)

responseDescribeCommunications :: DescribeCommunicationsResponse -> TestTree
responseDescribeCommunications = res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse.proto"
    support
    (Proxy :: Proxy DescribeCommunications)

responseAddCommunicationToCase :: AddCommunicationToCaseResponse -> TestTree
responseAddCommunicationToCase = res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse.proto"
    support
    (Proxy :: Proxy AddCommunicationToCase)
