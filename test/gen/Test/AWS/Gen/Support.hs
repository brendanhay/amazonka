-- Module      : Test.AWS.Gen.Support
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.Support
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ refreshTrustedAdvisorCheckTest $
--             refreshTrustedAdvisorCheck
--
--         , describeCasesTest $
--             describeCases
--
--         , describeTrustedAdvisorCheckRefreshStatusesTest $
--             describeTrustedAdvisorCheckRefreshStatuses
--
--         , createCaseTest $
--             createCase
--
--         , describeTrustedAdvisorCheckSummariesTest $
--             describeTrustedAdvisorCheckSummaries
--
--         , resolveCaseTest $
--             resolveCase
--
--         , describeSeverityLevelsTest $
--             describeSeverityLevels
--
--         , describeTrustedAdvisorChecksTest $
--             describeTrustedAdvisorChecks
--
--         , describeAttachmentTest $
--             describeAttachment
--
--         , addAttachmentsToSetTest $
--             addAttachmentsToSet
--
--         , describeTrustedAdvisorCheckResultTest $
--             describeTrustedAdvisorCheckResult
--
--         , describeServicesTest $
--             describeServices
--
--         , describeCommunicationsTest $
--             describeCommunications
--
--         , addCommunicationToCaseTest $
--             addCommunicationToCase
--
--           ]

--     , testGroup "response"
--         [ refreshTrustedAdvisorCheckResponseTest $
--             refreshTrustedAdvisorCheckResponse
--
--         , describeCasesResponseTest $
--             describeCasesResponse
--
--         , describeTrustedAdvisorCheckRefreshStatusesResponseTest $
--             describeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , createCaseResponseTest $
--             createCaseResponse
--
--         , describeTrustedAdvisorCheckSummariesResponseTest $
--             describeTrustedAdvisorCheckSummariesResponse
--
--         , resolveCaseResponseTest $
--             resolveCaseResponse
--
--         , describeSeverityLevelsResponseTest $
--             describeSeverityLevelsResponse
--
--         , describeTrustedAdvisorChecksResponseTest $
--             describeTrustedAdvisorChecksResponse
--
--         , describeAttachmentResponseTest $
--             describeAttachmentResponse
--
--         , addAttachmentsToSetResponseTest $
--             addAttachmentsToSetResponse
--
--         , describeTrustedAdvisorCheckResultResponseTest $
--             describeTrustedAdvisorCheckResultResponse
--
--         , describeServicesResponseTest $
--             describeServicesResponse
--
--         , describeCommunicationsResponseTest $
--             describeCommunicationsResponse
--
--         , addCommunicationToCaseResponseTest $
--             addCommunicationToCaseResponse
--
--           ]
--     ]

-- Requests

refreshTrustedAdvisorCheckTest :: RefreshTrustedAdvisorCheck -> TestTree
refreshTrustedAdvisorCheckTest = undefined

describeCasesTest :: DescribeCases -> TestTree
describeCasesTest = undefined

describeTrustedAdvisorCheckRefreshStatusesTest :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
describeTrustedAdvisorCheckRefreshStatusesTest = undefined

createCaseTest :: CreateCase -> TestTree
createCaseTest = undefined

describeTrustedAdvisorCheckSummariesTest :: DescribeTrustedAdvisorCheckSummaries -> TestTree
describeTrustedAdvisorCheckSummariesTest = undefined

resolveCaseTest :: ResolveCase -> TestTree
resolveCaseTest = undefined

describeSeverityLevelsTest :: DescribeSeverityLevels -> TestTree
describeSeverityLevelsTest = undefined

describeTrustedAdvisorChecksTest :: DescribeTrustedAdvisorChecks -> TestTree
describeTrustedAdvisorChecksTest = undefined

describeAttachmentTest :: DescribeAttachment -> TestTree
describeAttachmentTest = undefined

addAttachmentsToSetTest :: AddAttachmentsToSet -> TestTree
addAttachmentsToSetTest = undefined

describeTrustedAdvisorCheckResultTest :: DescribeTrustedAdvisorCheckResult -> TestTree
describeTrustedAdvisorCheckResultTest = undefined

describeServicesTest :: DescribeServices -> TestTree
describeServicesTest = undefined

describeCommunicationsTest :: DescribeCommunications -> TestTree
describeCommunicationsTest = undefined

addCommunicationToCaseTest :: AddCommunicationToCase -> TestTree
addCommunicationToCaseTest = undefined

-- Responses

refreshTrustedAdvisorCheckResponseTest :: RefreshTrustedAdvisorCheckResponse -> TestTree
refreshTrustedAdvisorCheckResponseTest = resp
    "RefreshTrustedAdvisorCheckResponse"
    "fixture/Support/RefreshTrustedAdvisorCheckResponse"
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

describeCasesResponseTest :: DescribeCasesResponse -> TestTree
describeCasesResponseTest = resp
    "DescribeCasesResponse"
    "fixture/Support/DescribeCasesResponse"
    (Proxy :: Proxy DescribeCases)

describeTrustedAdvisorCheckRefreshStatusesResponseTest :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
describeTrustedAdvisorCheckRefreshStatusesResponseTest = resp
    "DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/Support/DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

createCaseResponseTest :: CreateCaseResponse -> TestTree
createCaseResponseTest = resp
    "CreateCaseResponse"
    "fixture/Support/CreateCaseResponse"
    (Proxy :: Proxy CreateCase)

describeTrustedAdvisorCheckSummariesResponseTest :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
describeTrustedAdvisorCheckSummariesResponseTest = resp
    "DescribeTrustedAdvisorCheckSummariesResponse"
    "fixture/Support/DescribeTrustedAdvisorCheckSummariesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

resolveCaseResponseTest :: ResolveCaseResponse -> TestTree
resolveCaseResponseTest = resp
    "ResolveCaseResponse"
    "fixture/Support/ResolveCaseResponse"
    (Proxy :: Proxy ResolveCase)

describeSeverityLevelsResponseTest :: DescribeSeverityLevelsResponse -> TestTree
describeSeverityLevelsResponseTest = resp
    "DescribeSeverityLevelsResponse"
    "fixture/Support/DescribeSeverityLevelsResponse"
    (Proxy :: Proxy DescribeSeverityLevels)

describeTrustedAdvisorChecksResponseTest :: DescribeTrustedAdvisorChecksResponse -> TestTree
describeTrustedAdvisorChecksResponseTest = resp
    "DescribeTrustedAdvisorChecksResponse"
    "fixture/Support/DescribeTrustedAdvisorChecksResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

describeAttachmentResponseTest :: DescribeAttachmentResponse -> TestTree
describeAttachmentResponseTest = resp
    "DescribeAttachmentResponse"
    "fixture/Support/DescribeAttachmentResponse"
    (Proxy :: Proxy DescribeAttachment)

addAttachmentsToSetResponseTest :: AddAttachmentsToSetResponse -> TestTree
addAttachmentsToSetResponseTest = resp
    "AddAttachmentsToSetResponse"
    "fixture/Support/AddAttachmentsToSetResponse"
    (Proxy :: Proxy AddAttachmentsToSet)

describeTrustedAdvisorCheckResultResponseTest :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
describeTrustedAdvisorCheckResultResponseTest = resp
    "DescribeTrustedAdvisorCheckResultResponse"
    "fixture/Support/DescribeTrustedAdvisorCheckResultResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

describeServicesResponseTest :: DescribeServicesResponse -> TestTree
describeServicesResponseTest = resp
    "DescribeServicesResponse"
    "fixture/Support/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

describeCommunicationsResponseTest :: DescribeCommunicationsResponse -> TestTree
describeCommunicationsResponseTest = resp
    "DescribeCommunicationsResponse"
    "fixture/Support/DescribeCommunicationsResponse"
    (Proxy :: Proxy DescribeCommunications)

addCommunicationToCaseResponseTest :: AddCommunicationToCaseResponse -> TestTree
addCommunicationToCaseResponseTest = resp
    "AddCommunicationToCaseResponse"
    "fixture/Support/AddCommunicationToCaseResponse"
    (Proxy :: Proxy AddCommunicationToCase)
