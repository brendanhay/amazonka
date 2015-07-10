{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

testDescribeCasesResponse :: DescribeCasesResponse -> TestTree
testDescribeCasesResponse = res
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse"
    (Proxy :: Proxy DescribeCases)

testDescribeTrustedAdvisorCheckRefreshStatusesResponse :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatusesResponse = res
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

testCreateCaseResponse :: CreateCaseResponse -> TestTree
testCreateCaseResponse = res
    "CreateCaseResponse"
    "fixture/CreateCaseResponse"
    (Proxy :: Proxy CreateCase)

testDescribeTrustedAdvisorCheckSummariesResponse :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
testDescribeTrustedAdvisorCheckSummariesResponse = res
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

testResolveCaseResponse :: ResolveCaseResponse -> TestTree
testResolveCaseResponse = res
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse"
    (Proxy :: Proxy ResolveCase)

testDescribeSeverityLevelsResponse :: DescribeSeverityLevelsResponse -> TestTree
testDescribeSeverityLevelsResponse = res
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse"
    (Proxy :: Proxy DescribeSeverityLevels)

testDescribeTrustedAdvisorChecksResponse :: DescribeTrustedAdvisorChecksResponse -> TestTree
testDescribeTrustedAdvisorChecksResponse = res
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

testDescribeAttachmentResponse :: DescribeAttachmentResponse -> TestTree
testDescribeAttachmentResponse = res
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse"
    (Proxy :: Proxy DescribeAttachment)

testAddAttachmentsToSetResponse :: AddAttachmentsToSetResponse -> TestTree
testAddAttachmentsToSetResponse = res
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse"
    (Proxy :: Proxy AddAttachmentsToSet)

testDescribeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
testDescribeTrustedAdvisorCheckResultResponse = res
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = res
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

testDescribeCommunicationsResponse :: DescribeCommunicationsResponse -> TestTree
testDescribeCommunicationsResponse = res
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse"
    (Proxy :: Proxy DescribeCommunications)

testAddCommunicationToCaseResponse :: AddCommunicationToCaseResponse -> TestTree
testAddCommunicationToCaseResponse = res
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse"
    (Proxy :: Proxy AddCommunicationToCase)

instance Out AddAttachmentsToSet
instance Out AddAttachmentsToSetResponse
instance Out AddCommunicationToCase
instance Out AddCommunicationToCaseResponse
instance Out Attachment
instance Out AttachmentDetails
instance Out CaseDetails
instance Out Category
instance Out Communication
instance Out CreateCase
instance Out CreateCaseResponse
instance Out DescribeAttachment
instance Out DescribeAttachmentResponse
instance Out DescribeCases
instance Out DescribeCasesResponse
instance Out DescribeCommunications
instance Out DescribeCommunicationsResponse
instance Out DescribeServices
instance Out DescribeServicesResponse
instance Out DescribeSeverityLevels
instance Out DescribeSeverityLevelsResponse
instance Out DescribeTrustedAdvisorCheckRefreshStatuses
instance Out DescribeTrustedAdvisorCheckRefreshStatusesResponse
instance Out DescribeTrustedAdvisorCheckResult
instance Out DescribeTrustedAdvisorCheckResultResponse
instance Out DescribeTrustedAdvisorCheckSummaries
instance Out DescribeTrustedAdvisorCheckSummariesResponse
instance Out DescribeTrustedAdvisorChecks
instance Out DescribeTrustedAdvisorChecksResponse
instance Out RecentCaseCommunications
instance Out RefreshTrustedAdvisorCheck
instance Out RefreshTrustedAdvisorCheckResponse
instance Out ResolveCase
instance Out ResolveCaseResponse
instance Out SeverityLevel
instance Out SupportService
instance Out TrustedAdvisorCategorySpecificSummary
instance Out TrustedAdvisorCheckDescription
instance Out TrustedAdvisorCheckRefreshStatus
instance Out TrustedAdvisorCheckResult
instance Out TrustedAdvisorCheckSummary
instance Out TrustedAdvisorCostOptimizingSummary
instance Out TrustedAdvisorResourceDetail
instance Out TrustedAdvisorResourcesSummary
