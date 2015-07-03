-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Support where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Support

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
testRefreshTrustedAdvisorCheck = undefined

testDescribeCases :: DescribeCases -> TestTree
testDescribeCases = undefined

testDescribeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatuses = undefined

testCreateCase :: CreateCase -> TestTree
testCreateCase = undefined

testDescribeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries -> TestTree
testDescribeTrustedAdvisorCheckSummaries = undefined

testResolveCase :: ResolveCase -> TestTree
testResolveCase = undefined

testDescribeSeverityLevels :: DescribeSeverityLevels -> TestTree
testDescribeSeverityLevels = undefined

testDescribeTrustedAdvisorChecks :: DescribeTrustedAdvisorChecks -> TestTree
testDescribeTrustedAdvisorChecks = undefined

testDescribeAttachment :: DescribeAttachment -> TestTree
testDescribeAttachment = undefined

testAddAttachmentsToSet :: AddAttachmentsToSet -> TestTree
testAddAttachmentsToSet = undefined

testDescribeTrustedAdvisorCheckResult :: DescribeTrustedAdvisorCheckResult -> TestTree
testDescribeTrustedAdvisorCheckResult = undefined

testDescribeServices :: DescribeServices -> TestTree
testDescribeServices = undefined

testDescribeCommunications :: DescribeCommunications -> TestTree
testDescribeCommunications = undefined

testAddCommunicationToCase :: AddCommunicationToCase -> TestTree
testAddCommunicationToCase = undefined

-- Responses

testRefreshTrustedAdvisorCheckResponse :: RefreshTrustedAdvisorCheckResponse -> TestTree
testRefreshTrustedAdvisorCheckResponse = resp
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse"
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

testDescribeCasesResponse :: DescribeCasesResponse -> TestTree
testDescribeCasesResponse = resp
    "DescribeCasesResponse"
    "fixture/DescribeCasesResponse"
    (Proxy :: Proxy DescribeCases)

testDescribeTrustedAdvisorCheckRefreshStatusesResponse :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
testDescribeTrustedAdvisorCheckRefreshStatusesResponse = resp
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

testCreateCaseResponse :: CreateCaseResponse -> TestTree
testCreateCaseResponse = resp
    "CreateCaseResponse"
    "fixture/CreateCaseResponse"
    (Proxy :: Proxy CreateCase)

testDescribeTrustedAdvisorCheckSummariesResponse :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
testDescribeTrustedAdvisorCheckSummariesResponse = resp
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

testResolveCaseResponse :: ResolveCaseResponse -> TestTree
testResolveCaseResponse = resp
    "ResolveCaseResponse"
    "fixture/ResolveCaseResponse"
    (Proxy :: Proxy ResolveCase)

testDescribeSeverityLevelsResponse :: DescribeSeverityLevelsResponse -> TestTree
testDescribeSeverityLevelsResponse = resp
    "DescribeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse"
    (Proxy :: Proxy DescribeSeverityLevels)

testDescribeTrustedAdvisorChecksResponse :: DescribeTrustedAdvisorChecksResponse -> TestTree
testDescribeTrustedAdvisorChecksResponse = resp
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

testDescribeAttachmentResponse :: DescribeAttachmentResponse -> TestTree
testDescribeAttachmentResponse = resp
    "DescribeAttachmentResponse"
    "fixture/DescribeAttachmentResponse"
    (Proxy :: Proxy DescribeAttachment)

testAddAttachmentsToSetResponse :: AddAttachmentsToSetResponse -> TestTree
testAddAttachmentsToSetResponse = resp
    "AddAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse"
    (Proxy :: Proxy AddAttachmentsToSet)

testDescribeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
testDescribeTrustedAdvisorCheckResultResponse = resp
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

testDescribeServicesResponse :: DescribeServicesResponse -> TestTree
testDescribeServicesResponse = resp
    "DescribeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

testDescribeCommunicationsResponse :: DescribeCommunicationsResponse -> TestTree
testDescribeCommunicationsResponse = resp
    "DescribeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse"
    (Proxy :: Proxy DescribeCommunications)

testAddCommunicationToCaseResponse :: AddCommunicationToCaseResponse -> TestTree
testAddCommunicationToCaseResponse = resp
    "AddCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse"
    (Proxy :: Proxy AddCommunicationToCase)
