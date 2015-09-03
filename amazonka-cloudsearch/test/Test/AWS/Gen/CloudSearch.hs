{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
--         , testListDomainNames $
--             listDomainNames
--
--         , testDefineSuggester $
--             defineSuggester
--
--         , testDescribeDomains $
--             describeDomains
--
--         , testDeleteAnalysisScheme $
--             deleteAnalysisScheme
--
--         , testDescribeAnalysisSchemes $
--             describeAnalysisSchemes
--
--         , testCreateDomain $
--             createDomain
--
--         , testDescribeIndexFields $
--             describeIndexFields
--
--         , testDeleteSuggester $
--             deleteSuggester
--
--         , testDefineAnalysisScheme $
--             defineAnalysisScheme
--
--         , testIndexDocuments $
--             indexDocuments
--
--         , testDeleteIndexField $
--             deleteIndexField
--
--         , testUpdateServiceAccessPolicies $
--             updateServiceAccessPolicies
--
--         , testUpdateScalingParameters $
--             updateScalingParameters
--
--         , testBuildSuggesters $
--             buildSuggesters
--
--         , testDeleteDomain $
--             deleteDomain
--
--         , testDefineIndexField $
--             defineIndexField
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
--         , testListDomainNamesResponse $
--             listDomainNamesResponse
--
--         , testDefineSuggesterResponse $
--             defineSuggesterResponse
--
--         , testDescribeDomainsResponse $
--             describeDomainsResponse
--
--         , testDeleteAnalysisSchemeResponse $
--             deleteAnalysisSchemeResponse
--
--         , testDescribeAnalysisSchemesResponse $
--             describeAnalysisSchemesResponse
--
--         , testCreateDomainResponse $
--             createDomainResponse
--
--         , testDescribeIndexFieldsResponse $
--             describeIndexFieldsResponse
--
--         , testDeleteSuggesterResponse $
--             deleteSuggesterResponse
--
--         , testDefineAnalysisSchemeResponse $
--             defineAnalysisSchemeResponse
--
--         , testIndexDocumentsResponse $
--             indexDocumentsResponse
--
--         , testDeleteIndexFieldResponse $
--             deleteIndexFieldResponse
--
--         , testUpdateServiceAccessPoliciesResponse $
--             updateServiceAccessPoliciesResponse
--
--         , testUpdateScalingParametersResponse $
--             updateScalingParametersResponse
--
--         , testBuildSuggestersResponse $
--             buildSuggestersResponse
--
--         , testDeleteDomainResponse $
--             deleteDomainResponse
--
--         , testDefineIndexFieldResponse $
--             defineIndexFieldResponse
--
--           ]
--     ]

-- Requests

testDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
testDescribeAvailabilityOptions = req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions.yaml"

testDescribeExpressions :: DescribeExpressions -> TestTree
testDescribeExpressions = req
    "DescribeExpressions"
    "fixture/DescribeExpressions.yaml"

testDefineExpression :: DefineExpression -> TestTree
testDefineExpression = req
    "DefineExpression"
    "fixture/DefineExpression.yaml"

testDescribeScalingParameters :: DescribeScalingParameters -> TestTree
testDescribeScalingParameters = req
    "DescribeScalingParameters"
    "fixture/DescribeScalingParameters.yaml"

testDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
testDescribeServiceAccessPolicies = req
    "DescribeServiceAccessPolicies"
    "fixture/DescribeServiceAccessPolicies.yaml"

testDescribeSuggesters :: DescribeSuggesters -> TestTree
testDescribeSuggesters = req
    "DescribeSuggesters"
    "fixture/DescribeSuggesters.yaml"

testUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
testUpdateAvailabilityOptions = req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions.yaml"

testDeleteExpression :: DeleteExpression -> TestTree
testDeleteExpression = req
    "DeleteExpression"
    "fixture/DeleteExpression.yaml"

testListDomainNames :: ListDomainNames -> TestTree
testListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

testDefineSuggester :: DefineSuggester -> TestTree
testDefineSuggester = req
    "DefineSuggester"
    "fixture/DefineSuggester.yaml"

testDescribeDomains :: DescribeDomains -> TestTree
testDescribeDomains = req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

testDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
testDeleteAnalysisScheme = req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme.yaml"

testDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
testDescribeAnalysisSchemes = req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

testCreateDomain :: CreateDomain -> TestTree
testCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

testDescribeIndexFields :: DescribeIndexFields -> TestTree
testDescribeIndexFields = req
    "DescribeIndexFields"
    "fixture/DescribeIndexFields.yaml"

testDeleteSuggester :: DeleteSuggester -> TestTree
testDeleteSuggester = req
    "DeleteSuggester"
    "fixture/DeleteSuggester.yaml"

testDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
testDefineAnalysisScheme = req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme.yaml"

testIndexDocuments :: IndexDocuments -> TestTree
testIndexDocuments = req
    "IndexDocuments"
    "fixture/IndexDocuments.yaml"

testDeleteIndexField :: DeleteIndexField -> TestTree
testDeleteIndexField = req
    "DeleteIndexField"
    "fixture/DeleteIndexField.yaml"

testUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
testUpdateServiceAccessPolicies = req
    "UpdateServiceAccessPolicies"
    "fixture/UpdateServiceAccessPolicies.yaml"

testUpdateScalingParameters :: UpdateScalingParameters -> TestTree
testUpdateScalingParameters = req
    "UpdateScalingParameters"
    "fixture/UpdateScalingParameters.yaml"

testBuildSuggesters :: BuildSuggesters -> TestTree
testBuildSuggesters = req
    "BuildSuggesters"
    "fixture/BuildSuggesters.yaml"

testDeleteDomain :: DeleteDomain -> TestTree
testDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

testDefineIndexField :: DefineIndexField -> TestTree
testDefineIndexField = req
    "DefineIndexField"
    "fixture/DefineIndexField.yaml"

-- Responses

testDescribeAvailabilityOptionsResponse :: DescribeAvailabilityOptionsResponse -> TestTree
testDescribeAvailabilityOptionsResponse = res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeAvailabilityOptions)

testDescribeExpressionsResponse :: DescribeExpressionsResponse -> TestTree
testDescribeExpressionsResponse = res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeExpressions)

testDefineExpressionResponse :: DefineExpressionResponse -> TestTree
testDefineExpressionResponse = res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineExpression)

testDescribeScalingParametersResponse :: DescribeScalingParametersResponse -> TestTree
testDescribeScalingParametersResponse = res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeScalingParameters)

testDescribeServiceAccessPoliciesResponse :: DescribeServiceAccessPoliciesResponse -> TestTree
testDescribeServiceAccessPoliciesResponse = res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeServiceAccessPolicies)

testDescribeSuggestersResponse :: DescribeSuggestersResponse -> TestTree
testDescribeSuggestersResponse = res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeSuggesters)

testUpdateAvailabilityOptionsResponse :: UpdateAvailabilityOptionsResponse -> TestTree
testUpdateAvailabilityOptionsResponse = res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateAvailabilityOptions)

testDeleteExpressionResponse :: DeleteExpressionResponse -> TestTree
testDeleteExpressionResponse = res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteExpression)

testListDomainNamesResponse :: ListDomainNamesResponse -> TestTree
testListDomainNamesResponse = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    cloudSearch
    (Proxy :: Proxy ListDomainNames)

testDefineSuggesterResponse :: DefineSuggesterResponse -> TestTree
testDefineSuggesterResponse = res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineSuggester)

testDescribeDomainsResponse :: DescribeDomainsResponse -> TestTree
testDescribeDomainsResponse = res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeDomains)

testDeleteAnalysisSchemeResponse :: DeleteAnalysisSchemeResponse -> TestTree
testDeleteAnalysisSchemeResponse = res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteAnalysisScheme)

testDescribeAnalysisSchemesResponse :: DescribeAnalysisSchemesResponse -> TestTree
testDescribeAnalysisSchemesResponse = res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeAnalysisSchemes)

testCreateDomainResponse :: CreateDomainResponse -> TestTree
testCreateDomainResponse = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    cloudSearch
    (Proxy :: Proxy CreateDomain)

testDescribeIndexFieldsResponse :: DescribeIndexFieldsResponse -> TestTree
testDescribeIndexFieldsResponse = res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeIndexFields)

testDeleteSuggesterResponse :: DeleteSuggesterResponse -> TestTree
testDeleteSuggesterResponse = res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteSuggester)

testDefineAnalysisSchemeResponse :: DefineAnalysisSchemeResponse -> TestTree
testDefineAnalysisSchemeResponse = res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineAnalysisScheme)

testIndexDocumentsResponse :: IndexDocumentsResponse -> TestTree
testIndexDocumentsResponse = res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    cloudSearch
    (Proxy :: Proxy IndexDocuments)

testDeleteIndexFieldResponse :: DeleteIndexFieldResponse -> TestTree
testDeleteIndexFieldResponse = res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteIndexField)

testUpdateServiceAccessPoliciesResponse :: UpdateServiceAccessPoliciesResponse -> TestTree
testUpdateServiceAccessPoliciesResponse = res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateServiceAccessPolicies)

testUpdateScalingParametersResponse :: UpdateScalingParametersResponse -> TestTree
testUpdateScalingParametersResponse = res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateScalingParameters)

testBuildSuggestersResponse :: BuildSuggestersResponse -> TestTree
testBuildSuggestersResponse = res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    cloudSearch
    (Proxy :: Proxy BuildSuggesters)

testDeleteDomainResponse :: DeleteDomainResponse -> TestTree
testDeleteDomainResponse = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteDomain)

testDefineIndexFieldResponse :: DefineIndexFieldResponse -> TestTree
testDefineIndexFieldResponse = res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineIndexField)
