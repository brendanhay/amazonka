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
--         , testCreateCase $
--             createCase
--
--         , testDescribeTrustedAdvisorCheckSummaries $
--             describeTrustedAdvisorCheckSummaries
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
--         , testCreateCaseResponse $
--             createCaseResponse
--
--         , testDescribeTrustedAdvisorCheckSummariesResponse $
--             describeTrustedAdvisorCheckSummariesResponse
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
    "fixture/RefreshTrustedAdvisorCheck"

testDescribeCases :: DescribeCases -> TestTree
testDescribeCases = req
    "DescribeCases"
    "fixture/DescribeCases"

testDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatuses = req
    "DescribeTrustedAdvisorCheckRefreshStatuses"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatuses"

testCreateCase :: CreateCase -> TestTree
testCreateCase = req
    "CreateCase"
    "fixture/CreateCase"

testDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
testDescribeTrustedAdvisorCheckSummaries = req
    "DescribeTrustedAdvisorCheckSummaries"
    "fixture/DescribeTrustedAdvisorCheckSummaries"

testResolveCase :: ResolveCase -> TestTree
testResolveCase = req
    "ResolveCase"
    "fixture/ResolveCase"

testDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
testDescribeSeverityLevels = req
    "DescribeSeverityLevels"
    "fixture/DescribeSeverityLevels"

testDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
testDescribeTrustedAdvisorChecks = req
    "DescribeTrustedAdvisorChecks"
    "fixture/DescribeTrustedAdvisorChecks"

testDescribeAttachment :: DescribeAttachment -> TestTree
testDescribeAttachment = req
    "DescribeAttachment"
    "fixture/DescribeAttachment"

testAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
testAddAttachmentsToSet = req
    "AddAttachmentsToSet"
    "fixture/AddAttachmentsToSet"

testDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
testDescribeTrustedAdvisorCheckResult = req
    "DescribeTrustedAdvisorCheckResult"
    "fixture/DescribeTrustedAdvisorCheckResult"

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = req
    "DescribeServices"
    "fixture/DescribeServices"

testDescribeCommunications :: DescribeCommunications -> TestTree
testDescribeCommunications = req
    "DescribeCommunications"
    "fixture/DescribeCommunications"

testAddCommunicationToCase :: AddCommunicationToCase -> TestTree
testAddCommunicationToCase = req
    "AddCommunicationToCase"
    "fixture/AddCommunicationToCase"

-- Responses

testRefreshTrustedAdvisorCheckResponse :: RefreshTrustedAdvisorCheckResponse -> TestTree
testRefreshTrustedAdvisorCheckResponse = res
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse"
    support
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

testDescribeCasesResponse :: DescribeCasesResponse -> TestTree
testDescribeCasesResponse = res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse"
    support
    (Proxy :: Proxy DescribeCases)

testDescribeTrustedAdvisorCheckRefreshStatusesResponse :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatusesResponse = res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

testCreateCaseResponse :: CreateCaseResponse -> TestTree
testCreateCaseResponse = res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse"
    support
    (Proxy :: Proxy CreateCase)

testDescribeTrustedAdvisorCheckSummariesResponse :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
testDescribeTrustedAdvisorCheckSummariesResponse = res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

testResolveCaseResponse :: ResolveCaseResponse -> TestTree
testResolveCaseResponse = res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse"
    support
    (Proxy :: Proxy ResolveCase)

testDescribeSeverityLevelsResponse :: DescribeSeverityLevelsResponse -> TestTree
testDescribeSeverityLevelsResponse = res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse"
    support
    (Proxy :: Proxy DescribeSeverityLevels)

testDescribeTrustedAdvisorChecksResponse :: DescribeTrustedAdvisorChecksResponse -> TestTree
testDescribeTrustedAdvisorChecksResponse = res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

testDescribeAttachmentResponse :: DescribeAttachmentResponse -> TestTree
testDescribeAttachmentResponse = res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse"
    support
    (Proxy :: Proxy DescribeAttachment)

testAddAttachmentsToSetResponse :: AddAttachmentsToSetResponse -> TestTree
testAddAttachmentsToSetResponse = res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse"
    support
    (Proxy :: Proxy AddAttachmentsToSet)

testDescribeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
testDescribeTrustedAdvisorCheckResultResponse = res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse"
    support
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    support
    (Proxy :: Proxy DescribeServices)

testDescribeCommunicationsResponse :: DescribeCommunicationsResponse -> TestTree
testDescribeCommunicationsResponse = res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse"
    support
    (Proxy :: Proxy DescribeCommunications)

testAddCommunicationToCaseResponse :: AddCommunicationToCaseResponse -> TestTree
testAddCommunicationToCaseResponse = res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse"
    support
    (Proxy :: Proxy AddCommunicationToCase)
