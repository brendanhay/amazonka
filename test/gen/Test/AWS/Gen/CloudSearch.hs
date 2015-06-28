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

import           Data.Proxy
import           Network.AWS.CloudSearch
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeAvailabilityOptionsTest $
--             describeAvailabilityOptions
--
--         , describeExpressionsTest $
--             describeExpressions
--
--         , defineExpressionTest $
--             defineExpression
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
--         , updateAvailabilityOptionsTest $
--             updateAvailabilityOptions
--
--         , deleteExpressionTest $
--             deleteExpression
--
--         , describeDomainsTest $
--             describeDomains
--
--         , defineSuggesterTest $
--             defineSuggester
--
--         , deleteAnalysisSchemeTest $
--             deleteAnalysisScheme
--
--         , listDomainNamesTest $
--             listDomainNames
--
--         , describeAnalysisSchemesTest $
--             describeAnalysisSchemes
--
--         , createDomainTest $
--             createDomain
--
--         , defineAnalysisSchemeTest $
--             defineAnalysisScheme
--
--         , deleteSuggesterTest $
--             deleteSuggester
--
--         , describeIndexFieldsTest $
--             describeIndexFields
--
--         , indexDocumentsTest $
--             indexDocuments
--
--         , deleteIndexFieldTest $
--             deleteIndexField
--
--         , buildSuggestersTest $
--             buildSuggesters
--
--         , updateScalingParametersTest $
--             updateScalingParameters
--
--         , updateServiceAccessPoliciesTest $
--             updateServiceAccessPolicies
--
--         , defineIndexFieldTest $
--             defineIndexField
--
--         , deleteDomainTest $
--             deleteDomain
--
--           ]

--     , testGroup "response"
--         [ describeAvailabilityOptionsResponseTest $
--             describeAvailabilityOptionsResponse
--
--         , describeExpressionsResponseTest $
--             describeExpressionsResponse
--
--         , defineExpressionResponseTest $
--             defineExpressionResponse
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
--         , updateAvailabilityOptionsResponseTest $
--             updateAvailabilityOptionsResponse
--
--         , deleteExpressionResponseTest $
--             deleteExpressionResponse
--
--         , describeDomainsResponseTest $
--             describeDomainsResponse
--
--         , defineSuggesterResponseTest $
--             defineSuggesterResponse
--
--         , deleteAnalysisSchemeResponseTest $
--             deleteAnalysisSchemeResponse
--
--         , listDomainNamesResponseTest $
--             listDomainNamesResponse
--
--         , describeAnalysisSchemesResponseTest $
--             describeAnalysisSchemesResponse
--
--         , createDomainResponseTest $
--             createDomainResponse
--
--         , defineAnalysisSchemeResponseTest $
--             defineAnalysisSchemeResponse
--
--         , deleteSuggesterResponseTest $
--             deleteSuggesterResponse
--
--         , describeIndexFieldsResponseTest $
--             describeIndexFieldsResponse
--
--         , indexDocumentsResponseTest $
--             indexDocumentsResponse
--
--         , deleteIndexFieldResponseTest $
--             deleteIndexFieldResponse
--
--         , buildSuggestersResponseTest $
--             buildSuggestersResponse
--
--         , updateScalingParametersResponseTest $
--             updateScalingParametersResponse
--
--         , updateServiceAccessPoliciesResponseTest $
--             updateServiceAccessPoliciesResponse
--
--         , defineIndexFieldResponseTest $
--             defineIndexFieldResponse
--
--         , deleteDomainResponseTest $
--             deleteDomainResponse
--
--           ]
--     ]

-- Requests

describeAvailabilityOptionsTest :: DescribeAvailabilityOptions -> TestTree
describeAvailabilityOptionsTest = undefined

describeExpressionsTest :: DescribeExpressions -> TestTree
describeExpressionsTest = undefined

defineExpressionTest :: DefineExpression -> TestTree
defineExpressionTest = undefined

describeScalingParametersTest :: DescribeScalingParameters -> TestTree
describeScalingParametersTest = undefined

describeServiceAccessPoliciesTest :: DescribeServiceAccessPolicies -> TestTree
describeServiceAccessPoliciesTest = undefined

describeSuggestersTest :: DescribeSuggesters -> TestTree
describeSuggestersTest = undefined

updateAvailabilityOptionsTest :: UpdateAvailabilityOptions -> TestTree
updateAvailabilityOptionsTest = undefined

deleteExpressionTest :: DeleteExpression -> TestTree
deleteExpressionTest = undefined

describeDomainsTest :: DescribeDomains -> TestTree
describeDomainsTest = undefined

defineSuggesterTest :: DefineSuggester -> TestTree
defineSuggesterTest = undefined

deleteAnalysisSchemeTest :: DeleteAnalysisScheme -> TestTree
deleteAnalysisSchemeTest = undefined

listDomainNamesTest :: ListDomainNames -> TestTree
listDomainNamesTest = undefined

describeAnalysisSchemesTest :: DescribeAnalysisSchemes -> TestTree
describeAnalysisSchemesTest = undefined

createDomainTest :: CreateDomain -> TestTree
createDomainTest = undefined

defineAnalysisSchemeTest :: DefineAnalysisScheme -> TestTree
defineAnalysisSchemeTest = undefined

deleteSuggesterTest :: DeleteSuggester -> TestTree
deleteSuggesterTest = undefined

describeIndexFieldsTest :: DescribeIndexFields -> TestTree
describeIndexFieldsTest = undefined

indexDocumentsTest :: IndexDocuments -> TestTree
indexDocumentsTest = undefined

deleteIndexFieldTest :: DeleteIndexField -> TestTree
deleteIndexFieldTest = undefined

buildSuggestersTest :: BuildSuggesters -> TestTree
buildSuggestersTest = undefined

updateScalingParametersTest :: UpdateScalingParameters -> TestTree
updateScalingParametersTest = undefined

updateServiceAccessPoliciesTest :: UpdateServiceAccessPolicies -> TestTree
updateServiceAccessPoliciesTest = undefined

defineIndexFieldTest :: DefineIndexField -> TestTree
defineIndexFieldTest = undefined

deleteDomainTest :: DeleteDomain -> TestTree
deleteDomainTest = undefined

-- Responses

describeAvailabilityOptionsResponseTest :: DescribeAvailabilityOptionsResponse -> TestTree
describeAvailabilityOptionsResponseTest = resp
    "DescribeAvailabilityOptions"
    "fixture/CloudSearch/DescribeAvailabilityOptionsResponse"
    (Proxy :: Proxy DescribeAvailabilityOptions)

describeExpressionsResponseTest :: DescribeExpressionsResponse -> TestTree
describeExpressionsResponseTest = resp
    "DescribeExpressions"
    "fixture/CloudSearch/DescribeExpressionsResponse"
    (Proxy :: Proxy DescribeExpressions)

defineExpressionResponseTest :: DefineExpressionResponse -> TestTree
defineExpressionResponseTest = resp
    "DefineExpression"
    "fixture/CloudSearch/DefineExpressionResponse"
    (Proxy :: Proxy DefineExpression)

describeScalingParametersResponseTest :: DescribeScalingParametersResponse -> TestTree
describeScalingParametersResponseTest = resp
    "DescribeScalingParameters"
    "fixture/CloudSearch/DescribeScalingParametersResponse"
    (Proxy :: Proxy DescribeScalingParameters)

describeServiceAccessPoliciesResponseTest :: DescribeServiceAccessPoliciesResponse -> TestTree
describeServiceAccessPoliciesResponseTest = resp
    "DescribeServiceAccessPolicies"
    "fixture/CloudSearch/DescribeServiceAccessPoliciesResponse"
    (Proxy :: Proxy DescribeServiceAccessPolicies)

describeSuggestersResponseTest :: DescribeSuggestersResponse -> TestTree
describeSuggestersResponseTest = resp
    "DescribeSuggesters"
    "fixture/CloudSearch/DescribeSuggestersResponse"
    (Proxy :: Proxy DescribeSuggesters)

updateAvailabilityOptionsResponseTest :: UpdateAvailabilityOptionsResponse -> TestTree
updateAvailabilityOptionsResponseTest = resp
    "UpdateAvailabilityOptions"
    "fixture/CloudSearch/UpdateAvailabilityOptionsResponse"
    (Proxy :: Proxy UpdateAvailabilityOptions)

deleteExpressionResponseTest :: DeleteExpressionResponse -> TestTree
deleteExpressionResponseTest = resp
    "DeleteExpression"
    "fixture/CloudSearch/DeleteExpressionResponse"
    (Proxy :: Proxy DeleteExpression)

describeDomainsResponseTest :: DescribeDomainsResponse -> TestTree
describeDomainsResponseTest = resp
    "DescribeDomains"
    "fixture/CloudSearch/DescribeDomainsResponse"
    (Proxy :: Proxy DescribeDomains)

defineSuggesterResponseTest :: DefineSuggesterResponse -> TestTree
defineSuggesterResponseTest = resp
    "DefineSuggester"
    "fixture/CloudSearch/DefineSuggesterResponse"
    (Proxy :: Proxy DefineSuggester)

deleteAnalysisSchemeResponseTest :: DeleteAnalysisSchemeResponse -> TestTree
deleteAnalysisSchemeResponseTest = resp
    "DeleteAnalysisScheme"
    "fixture/CloudSearch/DeleteAnalysisSchemeResponse"
    (Proxy :: Proxy DeleteAnalysisScheme)

listDomainNamesResponseTest :: ListDomainNamesResponse -> TestTree
listDomainNamesResponseTest = resp
    "ListDomainNames"
    "fixture/CloudSearch/ListDomainNamesResponse"
    (Proxy :: Proxy ListDomainNames)

describeAnalysisSchemesResponseTest :: DescribeAnalysisSchemesResponse -> TestTree
describeAnalysisSchemesResponseTest = resp
    "DescribeAnalysisSchemes"
    "fixture/CloudSearch/DescribeAnalysisSchemesResponse"
    (Proxy :: Proxy DescribeAnalysisSchemes)

createDomainResponseTest :: CreateDomainResponse -> TestTree
createDomainResponseTest = resp
    "CreateDomain"
    "fixture/CloudSearch/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

defineAnalysisSchemeResponseTest :: DefineAnalysisSchemeResponse -> TestTree
defineAnalysisSchemeResponseTest = resp
    "DefineAnalysisScheme"
    "fixture/CloudSearch/DefineAnalysisSchemeResponse"
    (Proxy :: Proxy DefineAnalysisScheme)

deleteSuggesterResponseTest :: DeleteSuggesterResponse -> TestTree
deleteSuggesterResponseTest = resp
    "DeleteSuggester"
    "fixture/CloudSearch/DeleteSuggesterResponse"
    (Proxy :: Proxy DeleteSuggester)

describeIndexFieldsResponseTest :: DescribeIndexFieldsResponse -> TestTree
describeIndexFieldsResponseTest = resp
    "DescribeIndexFields"
    "fixture/CloudSearch/DescribeIndexFieldsResponse"
    (Proxy :: Proxy DescribeIndexFields)

indexDocumentsResponseTest :: IndexDocumentsResponse -> TestTree
indexDocumentsResponseTest = resp
    "IndexDocuments"
    "fixture/CloudSearch/IndexDocumentsResponse"
    (Proxy :: Proxy IndexDocuments)

deleteIndexFieldResponseTest :: DeleteIndexFieldResponse -> TestTree
deleteIndexFieldResponseTest = resp
    "DeleteIndexField"
    "fixture/CloudSearch/DeleteIndexFieldResponse"
    (Proxy :: Proxy DeleteIndexField)

buildSuggestersResponseTest :: BuildSuggestersResponse -> TestTree
buildSuggestersResponseTest = resp
    "BuildSuggesters"
    "fixture/CloudSearch/BuildSuggestersResponse"
    (Proxy :: Proxy BuildSuggesters)

updateScalingParametersResponseTest :: UpdateScalingParametersResponse -> TestTree
updateScalingParametersResponseTest = resp
    "UpdateScalingParameters"
    "fixture/CloudSearch/UpdateScalingParametersResponse"
    (Proxy :: Proxy UpdateScalingParameters)

updateServiceAccessPoliciesResponseTest :: UpdateServiceAccessPoliciesResponse -> TestTree
updateServiceAccessPoliciesResponseTest = resp
    "UpdateServiceAccessPolicies"
    "fixture/CloudSearch/UpdateServiceAccessPoliciesResponse"
    (Proxy :: Proxy UpdateServiceAccessPolicies)

defineIndexFieldResponseTest :: DefineIndexFieldResponse -> TestTree
defineIndexFieldResponseTest = resp
    "DefineIndexField"
    "fixture/CloudSearch/DefineIndexFieldResponse"
    (Proxy :: Proxy DefineIndexField)

deleteDomainResponseTest :: DeleteDomainResponse -> TestTree
deleteDomainResponseTest = resp
    "DeleteDomain"
    "fixture/CloudSearch/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)
