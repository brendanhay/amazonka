{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Support where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Support
import Test.AWS.Support.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testRefreshTrustedAdvisorCheck $
--             refreshTrustedAdvisorCheck
--
--         , testDescribeCases $
--             describeCases
--
--         , testDescribeTrustedAdvisorCheckRefreshStatuses $
--             describeTrustedAdvisorCheckRefreshStatuses
--
--         , testDescribeTrustedAdvisorCheckSummaries $
--             describeTrustedAdvisorCheckSummaries
--
--         , testCreateCase $
--             createCase
--
--         , testResolveCase $
--             resolveCase
--
--         , testDescribeSeverityLevels $
--             describeSeverityLevels
--
--         , testDescribeTrustedAdvisorChecks $
--             describeTrustedAdvisorChecks
--
--         , testDescribeAttachment $
--             describeAttachment
--
--         , testAddAttachmentsToSet $
--             addAttachmentsToSet
--
--         , testDescribeTrustedAdvisorCheckResult $
--             describeTrustedAdvisorCheckResult
--
--         , testDescribeServices $
--             describeServices
--
--         , testDescribeCommunications $
--             describeCommunications
--
--         , testAddCommunicationToCase $
--             addCommunicationToCase
--
--           ]

--     , testGroup "response"
--         [ testRefreshTrustedAdvisorCheckResponse $
--             refreshTrustedAdvisorCheckResponse
--
--         , testDescribeCasesResponse $
--             describeCasesResponse
--
--         , testDescribeTrustedAdvisorCheckRefreshStatusesResponse $
--             describeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , testDescribeTrustedAdvisorCheckSummariesResponse $
--             describeTrustedAdvisorCheckSummariesResponse
--
--         , testCreateCaseResponse $
--             createCaseResponse
--
--         , testResolveCaseResponse $
--             resolveCaseResponse
--
--         , testDescribeSeverityLevelsResponse $
--             describeSeverityLevelsResponse
--
--         , testDescribeTrustedAdvisorChecksResponse $
--             describeTrustedAdvisorChecksResponse
--
--         , testDescribeAttachmentResponse $
--             describeAttachmentResponse
--
--         , testAddAttachmentsToSetResponse $
--             addAttachmentsToSetResponse
--
--         , testDescribeTrustedAdvisorCheckResultResponse $
--             describeTrustedAdvisorCheckResultResponse
--
--         , testDescribeServicesResponse $
--             describeServicesResponse
--
--         , testDescribeCommunicationsResponse $
--             describeCommunicationsResponse
--
--         , testAddCommunicationToCaseResponse $
--             addCommunicationToCaseResponse
--
--           ]
--     ]

-- Requests

testRefreshTrustedAdvisorCheck :: RefreshTrustedAdvisorCheck -> TestTree
testRefreshTrustedAdvisorCheck = req
    "RefreshTrustedAdvisorCheck"
    "fixture/RefreshTrustedAdvisorCheck.yaml"

testDescribeCases :: DescribeCases -> TestTree
testDescribeCases = req
    "DescribeCases"
    "fixture/DescribeCases.yaml"

testDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatuses = req
    "DescribeTrustedAdvisorCheckRefreshStatuses"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatuses.yaml"

testDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
testDescribeTrustedAdvisorCheckSummaries = req
    "DescribeTrustedAdvisorCheckSummaries"
    "fixture/DescribeTrustedAdvisorCheckSummaries.yaml"

testCreateCase :: CreateCase -> TestTree
testCreateCase = req
    "CreateCase"
    "fixture/CreateCase.yaml"

testResolveCase :: ResolveCase -> TestTree
testResolveCase = req
    "ResolveCase"
    "fixture/ResolveCase.yaml"

testDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
testDescribeSeverityLevels = req
    "DescribeSeverityLevels"
    "fixture/DescribeSeverityLevels.yaml"

testDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
testDescribeTrustedAdvisorChecks = req
    "DescribeTrustedAdvisorChecks"
    "fixture/DescribeTrustedAdvisorChecks.yaml"

testDescribeAttachment :: DescribeAttachment -> TestTree
testDescribeAttachment = req
    "DescribeAttachment"
    "fixture/DescribeAttachment.yaml"

testAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
testAddAttachmentsToSet = req
    "AddAttachmentsToSet"
    "fixture/AddAttachmentsToSet.yaml"

testDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
testDescribeTrustedAdvisorCheckResult = req
    "DescribeTrustedAdvisorCheckResult"
    "fixture/DescribeTrustedAdvisorCheckResult.yaml"

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices.yaml"

testDescribeCommunications :: DescribeCommunications -> TestTree
testDescribeCommunications = req
    "DescribeCommunications"
    "fixture/DescribeCommunications.yaml"

testAddCommunicationToCase :: AddCommunicationToCase -> TestTree
testAddCommunicationToCase = req
    "AddCommunicationToCase"
    "fixture/AddCommunicationToCase.yaml"

-- Responses

testRefreshTrustedAdvisorCheckResponse :: RefreshTrustedAdvisorCheckResponse -> TestTree
testRefreshTrustedAdvisorCheckResponse = res
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse.proto"
    support
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

testDescribeCasesResponse :: DescribeCasesResponse -> TestTree
testDescribeCasesResponse = res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse.proto"
    support
    (Proxy :: Proxy DescribeCases)

testDescribeTrustedAdvisorCheckRefreshStatusesResponse :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatusesResponse = res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

testDescribeTrustedAdvisorCheckSummariesResponse :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
testDescribeTrustedAdvisorCheckSummariesResponse = res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

testCreateCaseResponse :: CreateCaseResponse -> TestTree
testCreateCaseResponse = res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse.proto"
    support
    (Proxy :: Proxy CreateCase)

testResolveCaseResponse :: ResolveCaseResponse -> TestTree
testResolveCaseResponse = res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse.proto"
    support
    (Proxy :: Proxy ResolveCase)

testDescribeSeverityLevelsResponse :: DescribeSeverityLevelsResponse -> TestTree
testDescribeSeverityLevelsResponse = res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse.proto"
    support
    (Proxy :: Proxy DescribeSeverityLevels)

testDescribeTrustedAdvisorChecksResponse :: DescribeTrustedAdvisorChecksResponse -> TestTree
testDescribeTrustedAdvisorChecksResponse = res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

testDescribeAttachmentResponse :: DescribeAttachmentResponse -> TestTree
testDescribeAttachmentResponse = res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse.proto"
    support
    (Proxy :: Proxy DescribeAttachment)

testAddAttachmentsToSetResponse :: AddAttachmentsToSetResponse -> TestTree
testAddAttachmentsToSetResponse = res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse.proto"
    support
    (Proxy :: Proxy AddAttachmentsToSet)

testDescribeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
testDescribeTrustedAdvisorCheckResultResponse = res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse.proto"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse.proto"
    support
    (Proxy :: Proxy DescribeServices)

testDescribeCommunicationsResponse :: DescribeCommunicationsResponse -> TestTree
testDescribeCommunicationsResponse = res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse.proto"
    support
    (Proxy :: Proxy DescribeCommunications)

testAddCommunicationToCaseResponse :: AddCommunicationToCaseResponse -> TestTree
testAddCommunicationToCaseResponse = res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse.proto"
    support
    (Proxy :: Proxy AddCommunicationToCase)
