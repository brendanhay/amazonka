-- Module      : Test.AWS.Gen.CloudSearch
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

module Test.AWS.Gen.CloudSearch where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CloudSearch

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ buildSuggestersTest $
--             buildSuggesters
--
--         , createDomainTest $
--             createDomain
--
--         , defineAnalysisSchemeTest $
--             defineAnalysisScheme
--
--         , defineExpressionTest $
--             defineExpression
--
--         , defineIndexFieldTest $
--             defineIndexField
--
--         , defineSuggesterTest $
--             defineSuggester
--
--         , deleteAnalysisSchemeTest $
--             deleteAnalysisScheme
--
--         , deleteDomainTest $
--             deleteDomain
--
--         , deleteExpressionTest $
--             deleteExpression
--
--         , deleteIndexFieldTest $
--             deleteIndexField
--
--         , deleteSuggesterTest $
--             deleteSuggester
--
--         , describeAnalysisSchemesTest $
--             describeAnalysisSchemes
--
--         , describeAvailabilityOptionsTest $
--             describeAvailabilityOptions
--
--         , describeDomainsTest $
--             describeDomains
--
--         , describeExpressionsTest $
--             describeExpressions
--
--         , describeIndexFieldsTest $
--             describeIndexFields
--
--         , describeScalingParametersTest $
--             describeScalingParameters
--
--         , describeServiceAccessPoliciesTest $
--             describeServiceAccessPolicies
--
--         , describeSuggestersTest $
--             describeSuggesters
--
--         , indexDocumentsTest $
--             indexDocuments
--
--         , listDomainNamesTest $
--             listDomainNames
--
--         , updateAvailabilityOptionsTest $
--             updateAvailabilityOptions
--
--         , updateScalingParametersTest $
--             updateScalingParameters
--
--         , updateServiceAccessPoliciesTest $
--             updateServiceAccessPolicies
--
--           ]

--     , testGroup "response"
--         [ buildSuggestersResponseTest $
--             buildSuggestersResponse
--
--         , createDomainResponseTest $
--             createDomainResponse
--
--         , defineAnalysisSchemeResponseTest $
--             defineAnalysisSchemeResponse
--
--         , defineExpressionResponseTest $
--             defineExpressionResponse
--
--         , defineIndexFieldResponseTest $
--             defineIndexFieldResponse
--
--         , defineSuggesterResponseTest $
--             defineSuggesterResponse
--
--         , deleteAnalysisSchemeResponseTest $
--             deleteAnalysisSchemeResponse
--
--         , deleteDomainResponseTest $
--             deleteDomainResponse
--
--         , deleteExpressionResponseTest $
--             deleteExpressionResponse
--
--         , deleteIndexFieldResponseTest $
--             deleteIndexFieldResponse
--
--         , deleteSuggesterResponseTest $
--             deleteSuggesterResponse
--
--         , describeAnalysisSchemesResponseTest $
--             describeAnalysisSchemesResponse
--
--         , describeAvailabilityOptionsResponseTest $
--             describeAvailabilityOptionsResponse
--
--         , describeDomainsResponseTest $
--             describeDomainsResponse
--
--         , describeExpressionsResponseTest $
--             describeExpressionsResponse
--
--         , describeIndexFieldsResponseTest $
--             describeIndexFieldsResponse
--
--         , describeScalingParametersResponseTest $
--             describeScalingParametersResponse
--
--         , describeServiceAccessPoliciesResponseTest $
--             describeServiceAccessPoliciesResponse
--
--         , describeSuggestersResponseTest $
--             describeSuggestersResponse
--
--         , indexDocumentsResponseTest $
--             indexDocumentsResponse
--
--         , listDomainNamesResponseTest $
--             listDomainNamesResponse
--
--         , updateAvailabilityOptionsResponseTest $
--             updateAvailabilityOptionsResponse
--
--         , updateScalingParametersResponseTest $
--             updateScalingParametersResponse
--
--         , updateServiceAccessPoliciesResponseTest $
--             updateServiceAccessPoliciesResponse
--
--           ]
--     ]

-- Requests

buildSuggestersTest :: BuildSuggesters -> TestTree
buildSuggestersTest = undefined

createDomainTest :: CreateDomain -> TestTree
createDomainTest = undefined

defineAnalysisSchemeTest :: DefineAnalysisScheme -> TestTree
defineAnalysisSchemeTest = undefined

defineExpressionTest :: DefineExpression -> TestTree
defineExpressionTest = undefined

defineIndexFieldTest :: DefineIndexField -> TestTree
defineIndexFieldTest = undefined

defineSuggesterTest :: DefineSuggester -> TestTree
defineSuggesterTest = undefined

deleteAnalysisSchemeTest :: DeleteAnalysisScheme -> TestTree
deleteAnalysisSchemeTest = undefined

deleteDomainTest :: DeleteDomain -> TestTree
deleteDomainTest = undefined

deleteExpressionTest :: DeleteExpression -> TestTree
deleteExpressionTest = undefined

deleteIndexFieldTest :: DeleteIndexField -> TestTree
deleteIndexFieldTest = undefined

deleteSuggesterTest :: DeleteSuggester -> TestTree
deleteSuggesterTest = undefined

describeAnalysisSchemesTest :: DescribeAnalysisSchemes -> TestTree
describeAnalysisSchemesTest = undefined

describeAvailabilityOptionsTest :: DescribeAvailabilityOptions -> TestTree
describeAvailabilityOptionsTest = undefined

describeDomainsTest :: DescribeDomains -> TestTree
describeDomainsTest = undefined

describeExpressionsTest :: DescribeExpressions -> TestTree
describeExpressionsTest = undefined

describeIndexFieldsTest :: DescribeIndexFields -> TestTree
describeIndexFieldsTest = undefined

describeScalingParametersTest :: DescribeScalingParameters -> TestTree
describeScalingParametersTest = undefined

describeServiceAccessPoliciesTest :: DescribeServiceAccessPolicies -> TestTree
describeServiceAccessPoliciesTest = undefined

describeSuggestersTest :: DescribeSuggesters -> TestTree
describeSuggestersTest = undefined

indexDocumentsTest :: IndexDocuments -> TestTree
indexDocumentsTest = undefined

listDomainNamesTest :: ListDomainNames -> TestTree
listDomainNamesTest = undefined

updateAvailabilityOptionsTest :: UpdateAvailabilityOptions -> TestTree
updateAvailabilityOptionsTest = undefined

updateScalingParametersTest :: UpdateScalingParameters -> TestTree
updateScalingParametersTest = undefined

updateServiceAccessPoliciesTest :: UpdateServiceAccessPolicies -> TestTree
updateServiceAccessPoliciesTest = undefined

-- Responses

buildSuggestersResponseTest :: BuildSuggestersResponse -> TestTree
buildSuggestersResponseTest = resp
    "buildSuggestersResponse"
    "fixture/BuildSuggestersResponse"
    (Proxy :: Proxy BuildSuggesters)

createDomainResponseTest :: CreateDomainResponse -> TestTree
createDomainResponseTest = resp
    "createDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

defineAnalysisSchemeResponseTest :: DefineAnalysisSchemeResponse -> TestTree
defineAnalysisSchemeResponseTest = resp
    "defineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse"
    (Proxy :: Proxy DefineAnalysisScheme)

defineExpressionResponseTest :: DefineExpressionResponse -> TestTree
defineExpressionResponseTest = resp
    "defineExpressionResponse"
    "fixture/DefineExpressionResponse"
    (Proxy :: Proxy DefineExpression)

defineIndexFieldResponseTest :: DefineIndexFieldResponse -> TestTree
defineIndexFieldResponseTest = resp
    "defineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse"
    (Proxy :: Proxy DefineIndexField)

defineSuggesterResponseTest :: DefineSuggesterResponse -> TestTree
defineSuggesterResponseTest = resp
    "defineSuggesterResponse"
    "fixture/DefineSuggesterResponse"
    (Proxy :: Proxy DefineSuggester)

deleteAnalysisSchemeResponseTest :: DeleteAnalysisSchemeResponse -> TestTree
deleteAnalysisSchemeResponseTest = resp
    "deleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse"
    (Proxy :: Proxy DeleteAnalysisScheme)

deleteDomainResponseTest :: DeleteDomainResponse -> TestTree
deleteDomainResponseTest = resp
    "deleteDomainResponse"
    "fixture/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)

deleteExpressionResponseTest :: DeleteExpressionResponse -> TestTree
deleteExpressionResponseTest = resp
    "deleteExpressionResponse"
    "fixture/DeleteExpressionResponse"
    (Proxy :: Proxy DeleteExpression)

deleteIndexFieldResponseTest :: DeleteIndexFieldResponse -> TestTree
deleteIndexFieldResponseTest = resp
    "deleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse"
    (Proxy :: Proxy DeleteIndexField)

deleteSuggesterResponseTest :: DeleteSuggesterResponse -> TestTree
deleteSuggesterResponseTest = resp
    "deleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse"
    (Proxy :: Proxy DeleteSuggester)

describeAnalysisSchemesResponseTest :: DescribeAnalysisSchemesResponse -> TestTree
describeAnalysisSchemesResponseTest = resp
    "describeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse"
    (Proxy :: Proxy DescribeAnalysisSchemes)

describeAvailabilityOptionsResponseTest :: DescribeAvailabilityOptionsResponse -> TestTree
describeAvailabilityOptionsResponseTest = resp
    "describeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse"
    (Proxy :: Proxy DescribeAvailabilityOptions)

describeDomainsResponseTest :: DescribeDomainsResponse -> TestTree
describeDomainsResponseTest = resp
    "describeDomainsResponse"
    "fixture/DescribeDomainsResponse"
    (Proxy :: Proxy DescribeDomains)

describeExpressionsResponseTest :: DescribeExpressionsResponse -> TestTree
describeExpressionsResponseTest = resp
    "describeExpressionsResponse"
    "fixture/DescribeExpressionsResponse"
    (Proxy :: Proxy DescribeExpressions)

describeIndexFieldsResponseTest :: DescribeIndexFieldsResponse -> TestTree
describeIndexFieldsResponseTest = resp
    "describeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse"
    (Proxy :: Proxy DescribeIndexFields)

describeScalingParametersResponseTest :: DescribeScalingParametersResponse -> TestTree
describeScalingParametersResponseTest = resp
    "describeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse"
    (Proxy :: Proxy DescribeScalingParameters)

describeServiceAccessPoliciesResponseTest :: DescribeServiceAccessPoliciesResponse -> TestTree
describeServiceAccessPoliciesResponseTest = resp
    "describeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse"
    (Proxy :: Proxy DescribeServiceAccessPolicies)

describeSuggestersResponseTest :: DescribeSuggestersResponse -> TestTree
describeSuggestersResponseTest = resp
    "describeSuggestersResponse"
    "fixture/DescribeSuggestersResponse"
    (Proxy :: Proxy DescribeSuggesters)

indexDocumentsResponseTest :: IndexDocumentsResponse -> TestTree
indexDocumentsResponseTest = resp
    "indexDocumentsResponse"
    "fixture/IndexDocumentsResponse"
    (Proxy :: Proxy IndexDocuments)

listDomainNamesResponseTest :: ListDomainNamesResponse -> TestTree
listDomainNamesResponseTest = resp
    "listDomainNamesResponse"
    "fixture/ListDomainNamesResponse"
    (Proxy :: Proxy ListDomainNames)

updateAvailabilityOptionsResponseTest :: UpdateAvailabilityOptionsResponse -> TestTree
updateAvailabilityOptionsResponseTest = resp
    "updateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse"
    (Proxy :: Proxy UpdateAvailabilityOptions)

updateScalingParametersResponseTest :: UpdateScalingParametersResponse -> TestTree
updateScalingParametersResponseTest = resp
    "updateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse"
    (Proxy :: Proxy UpdateScalingParameters)

updateServiceAccessPoliciesResponseTest :: UpdateServiceAccessPoliciesResponse -> TestTree
updateServiceAccessPoliciesResponseTest = resp
    "updateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse"
    (Proxy :: Proxy UpdateServiceAccessPolicies)
