{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudSearch where

import Data.Proxy
import Network.AWS.CloudSearch
import Test.AWS.CloudSearch.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeAvailabilityOptions $
--             describeAvailabilityOptions
--
--         , requestDescribeExpressions $
--             describeExpressions
--
--         , requestDefineExpression $
--             defineExpression
--
--         , requestDescribeScalingParameters $
--             describeScalingParameters
--
--         , requestDescribeServiceAccessPolicies $
--             describeServiceAccessPolicies
--
--         , requestDescribeSuggesters $
--             describeSuggesters
--
--         , requestUpdateAvailabilityOptions $
--             updateAvailabilityOptions
--
--         , requestDeleteExpression $
--             deleteExpression
--
--         , requestListDomainNames $
--             listDomainNames
--
--         , requestDefineSuggester $
--             defineSuggester
--
--         , requestDescribeDomains $
--             describeDomains
--
--         , requestDeleteAnalysisScheme $
--             deleteAnalysisScheme
--
--         , requestDescribeAnalysisSchemes $
--             describeAnalysisSchemes
--
--         , requestCreateDomain $
--             createDomain
--
--         , requestDescribeIndexFields $
--             describeIndexFields
--
--         , requestDeleteSuggester $
--             deleteSuggester
--
--         , requestDefineAnalysisScheme $
--             defineAnalysisScheme
--
--         , requestIndexDocuments $
--             indexDocuments
--
--         , requestDeleteIndexField $
--             deleteIndexField
--
--         , requestUpdateServiceAccessPolicies $
--             updateServiceAccessPolicies
--
--         , requestUpdateScalingParameters $
--             updateScalingParameters
--
--         , requestBuildSuggesters $
--             buildSuggesters
--
--         , requestDeleteDomain $
--             deleteDomain
--
--         , requestDefineIndexField $
--             defineIndexField
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAvailabilityOptions $
--             describeAvailabilityOptionsResponse
--
--         , responseDescribeExpressions $
--             describeExpressionsResponse
--
--         , responseDefineExpression $
--             defineExpressionResponse
--
--         , responseDescribeScalingParameters $
--             describeScalingParametersResponse
--
--         , responseDescribeServiceAccessPolicies $
--             describeServiceAccessPoliciesResponse
--
--         , responseDescribeSuggesters $
--             describeSuggestersResponse
--
--         , responseUpdateAvailabilityOptions $
--             updateAvailabilityOptionsResponse
--
--         , responseDeleteExpression $
--             deleteExpressionResponse
--
--         , responseListDomainNames $
--             listDomainNamesResponse
--
--         , responseDefineSuggester $
--             defineSuggesterResponse
--
--         , responseDescribeDomains $
--             describeDomainsResponse
--
--         , responseDeleteAnalysisScheme $
--             deleteAnalysisSchemeResponse
--
--         , responseDescribeAnalysisSchemes $
--             describeAnalysisSchemesResponse
--
--         , responseCreateDomain $
--             createDomainResponse
--
--         , responseDescribeIndexFields $
--             describeIndexFieldsResponse
--
--         , responseDeleteSuggester $
--             deleteSuggesterResponse
--
--         , responseDefineAnalysisScheme $
--             defineAnalysisSchemeResponse
--
--         , responseIndexDocuments $
--             indexDocumentsResponse
--
--         , responseDeleteIndexField $
--             deleteIndexFieldResponse
--
--         , responseUpdateServiceAccessPolicies $
--             updateServiceAccessPoliciesResponse
--
--         , responseUpdateScalingParameters $
--             updateScalingParametersResponse
--
--         , responseBuildSuggesters $
--             buildSuggestersResponse
--
--         , responseDeleteDomain $
--             deleteDomainResponse
--
--         , responseDefineIndexField $
--             defineIndexFieldResponse
--
--           ]
--     ]

-- Requests

requestDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
requestDescribeAvailabilityOptions = req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions.yaml"

requestDescribeExpressions :: DescribeExpressions -> TestTree
requestDescribeExpressions = req
    "DescribeExpressions"
    "fixture/DescribeExpressions.yaml"

requestDefineExpression :: DefineExpression -> TestTree
requestDefineExpression = req
    "DefineExpression"
    "fixture/DefineExpression.yaml"

requestDescribeScalingParameters :: DescribeScalingParameters -> TestTree
requestDescribeScalingParameters = req
    "DescribeScalingParameters"
    "fixture/DescribeScalingParameters.yaml"

requestDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
requestDescribeServiceAccessPolicies = req
    "DescribeServiceAccessPolicies"
    "fixture/DescribeServiceAccessPolicies.yaml"

requestDescribeSuggesters :: DescribeSuggesters -> TestTree
requestDescribeSuggesters = req
    "DescribeSuggesters"
    "fixture/DescribeSuggesters.yaml"

requestUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
requestUpdateAvailabilityOptions = req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions.yaml"

requestDeleteExpression :: DeleteExpression -> TestTree
requestDeleteExpression = req
    "DeleteExpression"
    "fixture/DeleteExpression.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestDefineSuggester :: DefineSuggester -> TestTree
requestDefineSuggester = req
    "DefineSuggester"
    "fixture/DefineSuggester.yaml"

requestDescribeDomains :: DescribeDomains -> TestTree
requestDescribeDomains = req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

requestDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
requestDeleteAnalysisScheme = req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme.yaml"

requestDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
requestDescribeAnalysisSchemes = req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDescribeIndexFields :: DescribeIndexFields -> TestTree
requestDescribeIndexFields = req
    "DescribeIndexFields"
    "fixture/DescribeIndexFields.yaml"

requestDeleteSuggester :: DeleteSuggester -> TestTree
requestDeleteSuggester = req
    "DeleteSuggester"
    "fixture/DeleteSuggester.yaml"

requestDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
requestDefineAnalysisScheme = req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme.yaml"

requestIndexDocuments :: IndexDocuments -> TestTree
requestIndexDocuments = req
    "IndexDocuments"
    "fixture/IndexDocuments.yaml"

requestDeleteIndexField :: DeleteIndexField -> TestTree
requestDeleteIndexField = req
    "DeleteIndexField"
    "fixture/DeleteIndexField.yaml"

requestUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
requestUpdateServiceAccessPolicies = req
    "UpdateServiceAccessPolicies"
    "fixture/UpdateServiceAccessPolicies.yaml"

requestUpdateScalingParameters :: UpdateScalingParameters -> TestTree
requestUpdateScalingParameters = req
    "UpdateScalingParameters"
    "fixture/UpdateScalingParameters.yaml"

requestBuildSuggesters :: BuildSuggesters -> TestTree
requestBuildSuggesters = req
    "BuildSuggesters"
    "fixture/BuildSuggesters.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDefineIndexField :: DefineIndexField -> TestTree
requestDefineIndexField = req
    "DefineIndexField"
    "fixture/DefineIndexField.yaml"

-- Responses

responseDescribeAvailabilityOptions :: DescribeAvailabilityOptionsResponse -> TestTree
responseDescribeAvailabilityOptions = res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeAvailabilityOptions)

responseDescribeExpressions :: DescribeExpressionsResponse -> TestTree
responseDescribeExpressions = res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeExpressions)

responseDefineExpression :: DefineExpressionResponse -> TestTree
responseDefineExpression = res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineExpression)

responseDescribeScalingParameters :: DescribeScalingParametersResponse -> TestTree
responseDescribeScalingParameters = res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeScalingParameters)

responseDescribeServiceAccessPolicies :: DescribeServiceAccessPoliciesResponse -> TestTree
responseDescribeServiceAccessPolicies = res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeServiceAccessPolicies)

responseDescribeSuggesters :: DescribeSuggestersResponse -> TestTree
responseDescribeSuggesters = res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeSuggesters)

responseUpdateAvailabilityOptions :: UpdateAvailabilityOptionsResponse -> TestTree
responseUpdateAvailabilityOptions = res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateAvailabilityOptions)

responseDeleteExpression :: DeleteExpressionResponse -> TestTree
responseDeleteExpression = res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteExpression)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    cloudSearch
    (Proxy :: Proxy ListDomainNames)

responseDefineSuggester :: DefineSuggesterResponse -> TestTree
responseDefineSuggester = res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineSuggester)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains = res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeDomains)

responseDeleteAnalysisScheme :: DeleteAnalysisSchemeResponse -> TestTree
responseDeleteAnalysisScheme = res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteAnalysisScheme)

responseDescribeAnalysisSchemes :: DescribeAnalysisSchemesResponse -> TestTree
responseDescribeAnalysisSchemes = res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeAnalysisSchemes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    cloudSearch
    (Proxy :: Proxy CreateDomain)

responseDescribeIndexFields :: DescribeIndexFieldsResponse -> TestTree
responseDescribeIndexFields = res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    cloudSearch
    (Proxy :: Proxy DescribeIndexFields)

responseDeleteSuggester :: DeleteSuggesterResponse -> TestTree
responseDeleteSuggester = res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteSuggester)

responseDefineAnalysisScheme :: DefineAnalysisSchemeResponse -> TestTree
responseDefineAnalysisScheme = res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineAnalysisScheme)

responseIndexDocuments :: IndexDocumentsResponse -> TestTree
responseIndexDocuments = res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    cloudSearch
    (Proxy :: Proxy IndexDocuments)

responseDeleteIndexField :: DeleteIndexFieldResponse -> TestTree
responseDeleteIndexField = res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteIndexField)

responseUpdateServiceAccessPolicies :: UpdateServiceAccessPoliciesResponse -> TestTree
responseUpdateServiceAccessPolicies = res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateServiceAccessPolicies)

responseUpdateScalingParameters :: UpdateScalingParametersResponse -> TestTree
responseUpdateScalingParameters = res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    cloudSearch
    (Proxy :: Proxy UpdateScalingParameters)

responseBuildSuggesters :: BuildSuggestersResponse -> TestTree
responseBuildSuggesters = res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    cloudSearch
    (Proxy :: Proxy BuildSuggesters)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    cloudSearch
    (Proxy :: Proxy DeleteDomain)

responseDefineIndexField :: DefineIndexFieldResponse -> TestTree
responseDefineIndexField = res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    cloudSearch
    (Proxy :: Proxy DefineIndexField)
