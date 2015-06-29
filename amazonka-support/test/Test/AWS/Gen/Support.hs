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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Support

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addAttachmentsToSetTest $
--             addAttachmentsToSet
--
--         , addCommunicationToCaseTest $
--             addCommunicationToCase
--
--         , createCaseTest $
--             createCase
--
--         , describeAttachmentTest $
--             describeAttachment
--
--         , describeCasesTest $
--             describeCases
--
--         , describeCommunicationsTest $
--             describeCommunications
--
--         , describeServicesTest $
--             describeServices
--
--         , describeSeverityLevelsTest $
--             describeSeverityLevels
--
--         , describeTrustedAdvisorCheckRefreshStatusesTest $
--             describeTrustedAdvisorCheckRefreshStatuses
--
--         , describeTrustedAdvisorCheckResultTest $
--             describeTrustedAdvisorCheckResult
--
--         , describeTrustedAdvisorCheckSummariesTest $
--             describeTrustedAdvisorCheckSummaries
--
--         , describeTrustedAdvisorChecksTest $
--             describeTrustedAdvisorChecks
--
--         , refreshTrustedAdvisorCheckTest $
--             refreshTrustedAdvisorCheck
--
--         , resolveCaseTest $
--             resolveCase
--
--           ]

--     , testGroup "response"
--         [ addAttachmentsToSetResponseTest $
--             addAttachmentsToSetResponse
--
--         , addCommunicationToCaseResponseTest $
--             addCommunicationToCaseResponse
--
--         , createCaseResponseTest $
--             createCaseResponse
--
--         , describeAttachmentResponseTest $
--             describeAttachmentResponse
--
--         , describeCasesResponseTest $
--             describeCasesResponse
--
--         , describeCommunicationsResponseTest $
--             describeCommunicationsResponse
--
--         , describeServicesResponseTest $
--             describeServicesResponse
--
--         , describeSeverityLevelsResponseTest $
--             describeSeverityLevelsResponse
--
--         , describeTrustedAdvisorCheckRefreshStatusesResponseTest $
--             describeTrustedAdvisorCheckRefreshStatusesResponse
--
--         , describeTrustedAdvisorCheckResultResponseTest $
--             describeTrustedAdvisorCheckResultResponse
--
--         , describeTrustedAdvisorCheckSummariesResponseTest $
--             describeTrustedAdvisorCheckSummariesResponse
--
--         , describeTrustedAdvisorChecksResponseTest $
--             describeTrustedAdvisorChecksResponse
--
--         , refreshTrustedAdvisorCheckResponseTest $
--             refreshTrustedAdvisorCheckResponse
--
--         , resolveCaseResponseTest $
--             resolveCaseResponse
--
--           ]
--     ]

-- Requests

addAttachmentsToSetTest :: AddAttachmentsToSet -> TestTree
addAttachmentsToSetTest = undefined

addCommunicationToCaseTest :: AddCommunicationToCase -> TestTree
addCommunicationToCaseTest = undefined

createCaseTest :: CreateCase -> TestTree
createCaseTest = undefined

describeAttachmentTest :: DescribeAttachment -> TestTree
describeAttachmentTest = undefined

describeCasesTest :: DescribeCases -> TestTree
describeCasesTest = undefined

describeCommunicationsTest :: DescribeCommunications -> TestTree
describeCommunicationsTest = undefined

describeServicesTest :: DescribeServices -> TestTree
describeServicesTest = undefined

describeSeverityLevelsTest :: DescribeSeverityLevels -> TestTree
describeSeverityLevelsTest = undefined

describeTrustedAdvisorCheckRefreshStatusesTest :: DescribeTrustedAdvisorCheckRefreshStatuses -> TestTree
describeTrustedAdvisorCheckRefreshStatusesTest = undefined

describeTrustedAdvisorCheckResultTest :: DescribeTrustedAdvisorCheckResult -> TestTree
describeTrustedAdvisorCheckResultTest = undefined

describeTrustedAdvisorCheckSummariesTest :: DescribeTrustedAdvisorCheckSummaries -> TestTree
describeTrustedAdvisorCheckSummariesTest = undefined

describeTrustedAdvisorChecksTest :: DescribeTrustedAdvisorChecks -> TestTree
describeTrustedAdvisorChecksTest = undefined

refreshTrustedAdvisorCheckTest :: RefreshTrustedAdvisorCheck -> TestTree
refreshTrustedAdvisorCheckTest = undefined

resolveCaseTest :: ResolveCase -> TestTree
resolveCaseTest = undefined

-- Responses

addAttachmentsToSetResponseTest :: AddAttachmentsToSetResponse -> TestTree
addAttachmentsToSetResponseTest = resp
    "addAttachmentsToSetResponse"
    "fixture/AddAttachmentsToSetResponse"
    (Proxy :: Proxy AddAttachmentsToSet)

addCommunicationToCaseResponseTest :: AddCommunicationToCaseResponse -> TestTree
addCommunicationToCaseResponseTest = resp
    "addCommunicationToCaseResponse"
    "fixture/AddCommunicationToCaseResponse"
    (Proxy :: Proxy AddCommunicationToCase)

createCaseResponseTest :: CreateCaseResponse -> TestTree
createCaseResponseTest = resp
    "createCaseResponse"
    "fixture/CreateCaseResponse"
    (Proxy :: Proxy CreateCase)

describeAttachmentResponseTest :: DescribeAttachmentResponse -> TestTree
describeAttachmentResponseTest = resp
    "describeAttachmentResponse"
    "fixture/DescribeAttachmentResponse"
    (Proxy :: Proxy DescribeAttachment)

describeCasesResponseTest :: DescribeCasesResponse -> TestTree
describeCasesResponseTest = resp
    "describeCasesResponse"
    "fixture/DescribeCasesResponse"
    (Proxy :: Proxy DescribeCases)

describeCommunicationsResponseTest :: DescribeCommunicationsResponse -> TestTree
describeCommunicationsResponseTest = resp
    "describeCommunicationsResponse"
    "fixture/DescribeCommunicationsResponse"
    (Proxy :: Proxy DescribeCommunications)

describeServicesResponseTest :: DescribeServicesResponse -> TestTree
describeServicesResponseTest = resp
    "describeServicesResponse"
    "fixture/DescribeServicesResponse"
    (Proxy :: Proxy DescribeServices)

describeSeverityLevelsResponseTest :: DescribeSeverityLevelsResponse -> TestTree
describeSeverityLevelsResponseTest = resp
    "describeSeverityLevelsResponse"
    "fixture/DescribeSeverityLevelsResponse"
    (Proxy :: Proxy DescribeSeverityLevels)

describeTrustedAdvisorCheckRefreshStatusesResponseTest :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> TestTree
describeTrustedAdvisorCheckRefreshStatusesResponseTest = resp
    "describeTrustedAdvisorCheckRefreshStatusesResponse"
    "fixture/DescribeTrustedAdvisorCheckRefreshStatusesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckRefreshStatuses)

describeTrustedAdvisorCheckResultResponseTest :: DescribeTrustedAdvisorCheckResultResponse -> TestTree
describeTrustedAdvisorCheckResultResponseTest = resp
    "describeTrustedAdvisorCheckResultResponse"
    "fixture/DescribeTrustedAdvisorCheckResultResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckResult)

describeTrustedAdvisorCheckSummariesResponseTest :: DescribeTrustedAdvisorCheckSummariesResponse -> TestTree
describeTrustedAdvisorCheckSummariesResponseTest = resp
    "describeTrustedAdvisorCheckSummariesResponse"
    "fixture/DescribeTrustedAdvisorCheckSummariesResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorCheckSummaries)

describeTrustedAdvisorChecksResponseTest :: DescribeTrustedAdvisorChecksResponse -> TestTree
describeTrustedAdvisorChecksResponseTest = resp
    "describeTrustedAdvisorChecksResponse"
    "fixture/DescribeTrustedAdvisorChecksResponse"
    (Proxy :: Proxy DescribeTrustedAdvisorChecks)

refreshTrustedAdvisorCheckResponseTest :: RefreshTrustedAdvisorCheckResponse -> TestTree
refreshTrustedAdvisorCheckResponseTest = resp
    "refreshTrustedAdvisorCheckResponse"
    "fixture/RefreshTrustedAdvisorCheckResponse"
    (Proxy :: Proxy RefreshTrustedAdvisorCheck)

resolveCaseResponseTest :: ResolveCaseResponse -> TestTree
resolveCaseResponseTest = resp
    "resolveCaseResponse"
    "fixture/ResolveCaseResponse"
    (Proxy :: Proxy ResolveCase)
