{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudSearch where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudSearch
import Test.AWS.CloudSearch.Internal

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
testDescribeAvailabilityOptions = req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions"

testDescribeExpressions :: DescribeExpressions -> TestTree
testDescribeExpressions = req
    "DescribeExpressions"
    "fixture/DescribeExpressions"

testDefineExpression :: DefineExpression -> TestTree
testDefineExpression = req
    "DefineExpression"
    "fixture/DefineExpression"

testDescribeScalingParameters :: DescribeScalingParameters -> TestTree
testDescribeScalingParameters = req
    "DescribeScalingParameters"
    "fixture/DescribeScalingParameters"

testDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
testDescribeServiceAccessPolicies = req
    "DescribeServiceAccessPolicies"
    "fixture/DescribeServiceAccessPolicies"

testDescribeSuggesters :: DescribeSuggesters -> TestTree
testDescribeSuggesters = req
    "DescribeSuggesters"
    "fixture/DescribeSuggesters"

testUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
testUpdateAvailabilityOptions = req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions"

testDeleteExpression :: DeleteExpression -> TestTree
testDeleteExpression = req
    "DeleteExpression"
    "fixture/DeleteExpression"

testDescribeDomains :: DescribeDomains -> TestTree
testDescribeDomains = req
    "DescribeDomains"
    "fixture/DescribeDomains"

testDefineSuggester :: DefineSuggester -> TestTree
testDefineSuggester = req
    "DefineSuggester"
    "fixture/DefineSuggester"

testDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
testDeleteAnalysisScheme = req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme"

testListDomainNames :: ListDomainNames -> TestTree
testListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames"

testDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
testDescribeAnalysisSchemes = req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes"

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain"

testDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
testDefineAnalysisScheme = req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme"

testDeleteSuggester :: DeleteSuggester -> TestTree
testDeleteSuggester = req
    "DeleteSuggester"
    "fixture/DeleteSuggester"

testDescribeIndexFields :: DescribeIndexFields -> TestTree
testDescribeIndexFields = req
    "DescribeIndexFields"
    "fixture/DescribeIndexFields"

testIndexDocuments :: IndexDocuments -> TestTree
testIndexDocuments = req
    "IndexDocuments"
    "fixture/IndexDocuments"

testDeleteIndexField :: DeleteIndexField -> TestTree
testDeleteIndexField = req
    "DeleteIndexField"
    "fixture/DeleteIndexField"

testBuildSuggesters :: BuildSuggesters -> TestTree
testBuildSuggesters = req
    "BuildSuggesters"
    "fixture/BuildSuggesters"

testUpdateScalingParameters :: UpdateScalingParameters -> TestTree
testUpdateScalingParameters = req
    "UpdateScalingParameters"
    "fixture/UpdateScalingParameters"

testUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
testUpdateServiceAccessPolicies = req
    "UpdateServiceAccessPolicies"
    "fixture/UpdateServiceAccessPolicies"

testDefineIndexField :: DefineIndexField -> TestTree
testDefineIndexField = req
    "DefineIndexField"
    "fixture/DefineIndexField"

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain"

-- Responses

testDescribeAvailabilityOptionsResponse :: DescribeAvailabilityOptionsResponse -> TestTree
testDescribeAvailabilityOptionsResponse = res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse"
    (Proxy :: Proxy DescribeAvailabilityOptions)

testDescribeExpressionsResponse :: DescribeExpressionsResponse -> TestTree
testDescribeExpressionsResponse = res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse"
    (Proxy :: Proxy DescribeExpressions)

testDefineExpressionResponse :: DefineExpressionResponse -> TestTree
testDefineExpressionResponse = res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse"
    (Proxy :: Proxy DefineExpression)

testDescribeScalingParametersResponse :: DescribeScalingParametersResponse -> TestTree
testDescribeScalingParametersResponse = res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse"
    (Proxy :: Proxy DescribeScalingParameters)

testDescribeServiceAccessPoliciesResponse :: DescribeServiceAccessPoliciesResponse -> TestTree
testDescribeServiceAccessPoliciesResponse = res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse"
    (Proxy :: Proxy DescribeServiceAccessPolicies)

testDescribeSuggestersResponse :: DescribeSuggestersResponse -> TestTree
testDescribeSuggestersResponse = res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse"
    (Proxy :: Proxy DescribeSuggesters)

testUpdateAvailabilityOptionsResponse :: UpdateAvailabilityOptionsResponse -> TestTree
testUpdateAvailabilityOptionsResponse = res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse"
    (Proxy :: Proxy UpdateAvailabilityOptions)

testDeleteExpressionResponse :: DeleteExpressionResponse -> TestTree
testDeleteExpressionResponse = res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse"
    (Proxy :: Proxy DeleteExpression)

testDescribeDomainsResponse :: DescribeDomainsResponse -> TestTree
testDescribeDomainsResponse = res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse"
    (Proxy :: Proxy DescribeDomains)

testDefineSuggesterResponse :: DefineSuggesterResponse -> TestTree
testDefineSuggesterResponse = res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse"
    (Proxy :: Proxy DefineSuggester)

testDeleteAnalysisSchemeResponse :: DeleteAnalysisSchemeResponse -> TestTree
testDeleteAnalysisSchemeResponse = res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse"
    (Proxy :: Proxy DeleteAnalysisScheme)

testListDomainNamesResponse :: ListDomainNamesResponse -> TestTree
testListDomainNamesResponse = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse"
    (Proxy :: Proxy ListDomainNames)

testDescribeAnalysisSchemesResponse :: DescribeAnalysisSchemesResponse -> TestTree
testDescribeAnalysisSchemesResponse = res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse"
    (Proxy :: Proxy DescribeAnalysisSchemes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse"
    (Proxy :: Proxy CreateDomain)

testDefineAnalysisSchemeResponse :: DefineAnalysisSchemeResponse -> TestTree
testDefineAnalysisSchemeResponse = res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse"
    (Proxy :: Proxy DefineAnalysisScheme)

testDeleteSuggesterResponse :: DeleteSuggesterResponse -> TestTree
testDeleteSuggesterResponse = res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse"
    (Proxy :: Proxy DeleteSuggester)

testDescribeIndexFieldsResponse :: DescribeIndexFieldsResponse -> TestTree
testDescribeIndexFieldsResponse = res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse"
    (Proxy :: Proxy DescribeIndexFields)

testIndexDocumentsResponse :: IndexDocumentsResponse -> TestTree
testIndexDocumentsResponse = res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse"
    (Proxy :: Proxy IndexDocuments)

testDeleteIndexFieldResponse :: DeleteIndexFieldResponse -> TestTree
testDeleteIndexFieldResponse = res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse"
    (Proxy :: Proxy DeleteIndexField)

testBuildSuggestersResponse :: BuildSuggestersResponse -> TestTree
testBuildSuggestersResponse = res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse"
    (Proxy :: Proxy BuildSuggesters)

testUpdateScalingParametersResponse :: UpdateScalingParametersResponse -> TestTree
testUpdateScalingParametersResponse = res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse"
    (Proxy :: Proxy UpdateScalingParameters)

testUpdateServiceAccessPoliciesResponse :: UpdateServiceAccessPoliciesResponse -> TestTree
testUpdateServiceAccessPoliciesResponse = res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse"
    (Proxy :: Proxy UpdateServiceAccessPolicies)

testDefineIndexFieldResponse :: DefineIndexFieldResponse -> TestTree
testDefineIndexFieldResponse = res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse"
    (Proxy :: Proxy DefineIndexField)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = res
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
