{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.CloudSearch
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
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeAvailabilityOptions $
--             describeAvailabilityOptions
--
--         , testDescribeExpressions $
--             describeExpressions
--
--         , testDefineExpression $
--             defineExpression
--
--         , testDescribeScalingParameters $
--             describeScalingParameters
--
--         , testDescribeServiceAccessPolicies $
--             describeServiceAccessPolicies
--
--         , testDescribeSuggesters $
--             describeSuggesters
--
--         , testUpdateAvailabilityOptions $
--             updateAvailabilityOptions
--
--         , testDeleteExpression $
--             deleteExpression
--
--         , testDescribeDomains $
--             describeDomains
--
--         , testDefineSuggester $
--             defineSuggester
--
--         , testDeleteAnalysisScheme $
--             deleteAnalysisScheme
--
--         , testListDomainNames $
--             listDomainNames
--
--         , testDescribeAnalysisSchemes $
--             describeAnalysisSchemes
--
--         , testCreateDomain $
--             createDomain
--
--         , testDefineAnalysisScheme $
--             defineAnalysisScheme
--
--         , testDeleteSuggester $
--             deleteSuggester
--
--         , testDescribeIndexFields $
--             describeIndexFields
--
--         , testIndexDocuments $
--             indexDocuments
--
--         , testDeleteIndexField $
--             deleteIndexField
--
--         , testBuildSuggesters $
--             buildSuggesters
--
--         , testUpdateScalingParameters $
--             updateScalingParameters
--
--         , testUpdateServiceAccessPolicies $
--             updateServiceAccessPolicies
--
--         , testDefineIndexField $
--             defineIndexField
--
--         , testDeleteDomain $
--             deleteDomain
--
--           ]

--     , testGroup "response"
--         [ testDescribeAvailabilityOptionsResponse $
--             describeAvailabilityOptionsResponse
--
--         , testDescribeExpressionsResponse $
--             describeExpressionsResponse
--
--         , testDefineExpressionResponse $
--             defineExpressionResponse
--
--         , testDescribeScalingParametersResponse $
--             describeScalingParametersResponse
--
--         , testDescribeServiceAccessPoliciesResponse $
--             describeServiceAccessPoliciesResponse
--
--         , testDescribeSuggestersResponse $
--             describeSuggestersResponse
--
--         , testUpdateAvailabilityOptionsResponse $
--             updateAvailabilityOptionsResponse
--
--         , testDeleteExpressionResponse $
--             deleteExpressionResponse
--
--         , testDescribeDomainsResponse $
--             describeDomainsResponse
--
--         , testDefineSuggesterResponse $
--             defineSuggesterResponse
--
--         , testDeleteAnalysisSchemeResponse $
--             deleteAnalysisSchemeResponse
--
--         , testListDomainNamesResponse $
--             listDomainNamesResponse
--
--         , testDescribeAnalysisSchemesResponse $
--             describeAnalysisSchemesResponse
--
--         , testCreateDomainResponse $
--             createDomainResponse
--
--         , testDefineAnalysisSchemeResponse $
--             defineAnalysisSchemeResponse
--
--         , testDeleteSuggesterResponse $
--             deleteSuggesterResponse
--
--         , testDescribeIndexFieldsResponse $
--             describeIndexFieldsResponse
--
--         , testIndexDocumentsResponse $
--             indexDocumentsResponse
--
--         , testDeleteIndexFieldResponse $
--             deleteIndexFieldResponse
--
--         , testBuildSuggestersResponse $
--             buildSuggestersResponse
--
--         , testUpdateScalingParametersResponse $
--             updateScalingParametersResponse
--
--         , testUpdateServiceAccessPoliciesResponse $
--             updateServiceAccessPoliciesResponse
--
--         , testDefineIndexFieldResponse $
--             defineIndexFieldResponse
--
--         , testDeleteDomainResponse $
--             deleteDomainResponse
--
--           ]
--     ]

-- Requests

testDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
testDescribeAvailabilityOptions = undefined

testDescribeExpressions :: DescribeExpressions -> TestTree
testDescribeExpressions = undefined

testDefineExpression :: DefineExpression -> TestTree
testDefineExpression = undefined

testDescribeScalingParameters :: DescribeScalingParameters -> TestTree
testDescribeScalingParameters = undefined

testDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
testDescribeServiceAccessPolicies = undefined

testDescribeSuggesters :: DescribeSuggesters -> TestTree
testDescribeSuggesters = undefined

testUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
testUpdateAvailabilityOptions = undefined

testDeleteExpression :: DeleteExpression -> TestTree
testDeleteExpression = undefined

testDescribeDomains :: DescribeDomains -> TestTree
testDescribeDomains = undefined

testDefineSuggester :: DefineSuggester -> TestTree
testDefineSuggester = undefined

testDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
testDeleteAnalysisScheme = undefined

testListDomainNames :: ListDomainNames -> TestTree
testListDomainNames = undefined

testDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
testDescribeAnalysisSchemes = undefined

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = undefined

testDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
testDefineAnalysisScheme = undefined

testDeleteSuggester :: DeleteSuggester -> TestTree
testDeleteSuggester = undefined

testDescribeIndexFields :: DescribeIndexFields -> TestTree
testDescribeIndexFields = undefined

testIndexDocuments :: IndexDocuments -> TestTree
testIndexDocuments = undefined

testDeleteIndexField :: DeleteIndexField -> TestTree
testDeleteIndexField = undefined

testBuildSuggesters :: BuildSuggesters -> TestTree
testBuildSuggesters = undefined

testUpdateScalingParameters :: UpdateScalingParameters -> TestTree
testUpdateScalingParameters = undefined

testUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
testUpdateServiceAccessPolicies = undefined

testDefineIndexField :: DefineIndexField -> TestTree
testDefineIndexField = undefined

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = undefined

-- Responses

testDescribeAvailabilityOptionsResponse :: DescribeAvailabilityOptionsResponse -> TestTree
testDescribeAvailabilityOptionsResponse = resp
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse"
    (Proxy :: Proxy DescribeAvailabilityOptions)

testDescribeExpressionsResponse :: DescribeExpressionsResponse -> TestTree
testDescribeExpressionsResponse = resp
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse"
    (Proxy :: Proxy DescribeExpressions)

testDefineExpressionResponse :: DefineExpressionResponse -> TestTree
testDefineExpressionResponse = resp
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse"
    (Proxy :: Proxy DefineExpression)

testDescribeScalingParametersResponse :: DescribeScalingParametersResponse -> TestTree
testDescribeScalingParametersResponse = resp
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse"
    (Proxy :: Proxy DescribeScalingParameters)

testDescribeServiceAccessPoliciesResponse :: DescribeServiceAccessPoliciesResponse -> TestTree
testDescribeServiceAccessPoliciesResponse = resp
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse"
    (Proxy :: Proxy DescribeServiceAccessPolicies)

testDescribeSuggestersResponse :: DescribeSuggestersResponse -> TestTree
testDescribeSuggestersResponse = resp
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse"
    (Proxy :: Proxy DescribeSuggesters)

testUpdateAvailabilityOptionsResponse :: UpdateAvailabilityOptionsResponse -> TestTree
testUpdateAvailabilityOptionsResponse = resp
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse"
    (Proxy :: Proxy UpdateAvailabilityOptions)

testDeleteExpressionResponse :: DeleteExpressionResponse -> TestTree
testDeleteExpressionResponse = resp
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse"
    (Proxy :: Proxy DeleteExpression)

testDescribeDomainsResponse :: DescribeDomainsResponse -> TestTree
testDescribeDomainsResponse = resp
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse"
    (Proxy :: Proxy DescribeDomains)

testDefineSuggesterResponse :: DefineSuggesterResponse -> TestTree
testDefineSuggesterResponse = resp
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse"
    (Proxy :: Proxy DefineSuggester)

testDeleteAnalysisSchemeResponse :: DeleteAnalysisSchemeResponse -> TestTree
testDeleteAnalysisSchemeResponse = resp
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse"
    (Proxy :: Proxy DeleteAnalysisScheme)

testListDomainNamesResponse :: ListDomainNamesResponse -> TestTree
testListDomainNamesResponse = resp
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse"
    (Proxy :: Proxy ListDomainNames)

testDescribeAnalysisSchemesResponse :: DescribeAnalysisSchemesResponse -> TestTree
testDescribeAnalysisSchemesResponse = resp
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse"
    (Proxy :: Proxy DescribeAnalysisSchemes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = resp
    "CreateDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

testDefineAnalysisSchemeResponse :: DefineAnalysisSchemeResponse -> TestTree
testDefineAnalysisSchemeResponse = resp
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse"
    (Proxy :: Proxy DefineAnalysisScheme)

testDeleteSuggesterResponse :: DeleteSuggesterResponse -> TestTree
testDeleteSuggesterResponse = resp
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse"
    (Proxy :: Proxy DeleteSuggester)

testDescribeIndexFieldsResponse :: DescribeIndexFieldsResponse -> TestTree
testDescribeIndexFieldsResponse = resp
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse"
    (Proxy :: Proxy DescribeIndexFields)

testIndexDocumentsResponse :: IndexDocumentsResponse -> TestTree
testIndexDocumentsResponse = resp
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse"
    (Proxy :: Proxy IndexDocuments)

testDeleteIndexFieldResponse :: DeleteIndexFieldResponse -> TestTree
testDeleteIndexFieldResponse = resp
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse"
    (Proxy :: Proxy DeleteIndexField)

testBuildSuggestersResponse :: BuildSuggestersResponse -> TestTree
testBuildSuggestersResponse = resp
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse"
    (Proxy :: Proxy BuildSuggesters)

testUpdateScalingParametersResponse :: UpdateScalingParametersResponse -> TestTree
testUpdateScalingParametersResponse = resp
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse"
    (Proxy :: Proxy UpdateScalingParameters)

testUpdateServiceAccessPoliciesResponse :: UpdateServiceAccessPoliciesResponse -> TestTree
testUpdateServiceAccessPoliciesResponse = resp
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse"
    (Proxy :: Proxy UpdateServiceAccessPolicies)

testDefineIndexFieldResponse :: DefineIndexFieldResponse -> TestTree
testDefineIndexFieldResponse = resp
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse"
    (Proxy :: Proxy DefineIndexField)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = resp
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse"
    (Proxy :: Proxy DeleteDomain)

instance Out AccessPoliciesStatus
instance Out AlgorithmicStemming
instance Out AnalysisOptions
instance Out AnalysisScheme
instance Out AnalysisSchemeLanguage
instance Out AnalysisSchemeStatus
instance Out AvailabilityOptionsStatus
instance Out BuildSuggesters
instance Out BuildSuggestersResponse
instance Out CreateDomain
instance Out CreateDomainResponse
instance Out DateArrayOptions
instance Out DateOptions
instance Out DefineAnalysisScheme
instance Out DefineAnalysisSchemeResponse
instance Out DefineExpression
instance Out DefineExpressionResponse
instance Out DefineIndexField
instance Out DefineIndexFieldResponse
instance Out DefineSuggester
instance Out DefineSuggesterResponse
instance Out DeleteAnalysisScheme
instance Out DeleteAnalysisSchemeResponse
instance Out DeleteDomain
instance Out DeleteDomainResponse
instance Out DeleteExpression
instance Out DeleteExpressionResponse
instance Out DeleteIndexField
instance Out DeleteIndexFieldResponse
instance Out DeleteSuggester
instance Out DeleteSuggesterResponse
instance Out DescribeAnalysisSchemes
instance Out DescribeAnalysisSchemesResponse
instance Out DescribeAvailabilityOptions
instance Out DescribeAvailabilityOptionsResponse
instance Out DescribeDomains
instance Out DescribeDomainsResponse
instance Out DescribeExpressions
instance Out DescribeExpressionsResponse
instance Out DescribeIndexFields
instance Out DescribeIndexFieldsResponse
instance Out DescribeScalingParameters
instance Out DescribeScalingParametersResponse
instance Out DescribeServiceAccessPolicies
instance Out DescribeServiceAccessPoliciesResponse
instance Out DescribeSuggesters
instance Out DescribeSuggestersResponse
instance Out DocumentSuggesterOptions
instance Out DomainStatus
instance Out DoubleArrayOptions
instance Out DoubleOptions
instance Out Expression
instance Out ExpressionStatus
instance Out IndexDocuments
instance Out IndexDocumentsResponse
instance Out IndexField
instance Out IndexFieldStatus
instance Out IndexFieldType
instance Out IntArrayOptions
instance Out IntOptions
instance Out LatLonOptions
instance Out Limits
instance Out ListDomainNames
instance Out ListDomainNamesResponse
instance Out LiteralArrayOptions
instance Out LiteralOptions
instance Out OptionState
instance Out OptionStatus
instance Out PartitionInstanceType
instance Out ScalingParameters
instance Out ScalingParametersStatus
instance Out ServiceEndpoint
instance Out Suggester
instance Out SuggesterFuzzyMatching
instance Out SuggesterStatus
instance Out TextArrayOptions
instance Out TextOptions
instance Out UpdateAvailabilityOptions
instance Out UpdateAvailabilityOptionsResponse
instance Out UpdateScalingParameters
instance Out UpdateScalingParametersResponse
instance Out UpdateServiceAccessPolicies
instance Out UpdateServiceAccessPoliciesResponse
