{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestDescribeAvailabilityOptions $
--             mkDescribeAvailabilityOptions
--
--         , requestDescribeExpressions $
--             mkDescribeExpressions
--
--         , requestDefineExpression $
--             mkDefineExpression
--
--         , requestDescribeScalingParameters $
--             mkDescribeScalingParameters
--
--         , requestDescribeServiceAccessPolicies $
--             mkDescribeServiceAccessPolicies
--
--         , requestDescribeSuggesters $
--             mkDescribeSuggesters
--
--         , requestUpdateAvailabilityOptions $
--             mkUpdateAvailabilityOptions
--
--         , requestDeleteExpression $
--             mkDeleteExpression
--
--         , requestListDomainNames $
--             mkListDomainNames
--
--         , requestDefineSuggester $
--             mkDefineSuggester
--
--         , requestDescribeDomains $
--             mkDescribeDomains
--
--         , requestDeleteAnalysisScheme $
--             mkDeleteAnalysisScheme
--
--         , requestDescribeDomainEndpointOptions $
--             mkDescribeDomainEndpointOptions
--
--         , requestDescribeAnalysisSchemes $
--             mkDescribeAnalysisSchemes
--
--         , requestCreateDomain $
--             mkCreateDomain
--
--         , requestUpdateDomainEndpointOptions $
--             mkUpdateDomainEndpointOptions
--
--         , requestDescribeIndexFields $
--             mkDescribeIndexFields
--
--         , requestDeleteSuggester $
--             mkDeleteSuggester
--
--         , requestDefineAnalysisScheme $
--             mkDefineAnalysisScheme
--
--         , requestIndexDocuments $
--             mkIndexDocuments
--
--         , requestDeleteIndexField $
--             mkDeleteIndexField
--
--         , requestUpdateServiceAccessPolicies $
--             mkUpdateServiceAccessPolicies
--
--         , requestUpdateScalingParameters $
--             mkUpdateScalingParameters
--
--         , requestBuildSuggesters $
--             mkBuildSuggesters
--
--         , requestDeleteDomain $
--             mkDeleteDomain
--
--         , requestDefineIndexField $
--             mkDefineIndexField
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAvailabilityOptions $
--             mkDescribeAvailabilityOptionsResponse
--
--         , responseDescribeExpressions $
--             mkDescribeExpressionsResponse
--
--         , responseDefineExpression $
--             mkDefineExpressionResponse
--
--         , responseDescribeScalingParameters $
--             mkDescribeScalingParametersResponse
--
--         , responseDescribeServiceAccessPolicies $
--             mkDescribeServiceAccessPoliciesResponse
--
--         , responseDescribeSuggesters $
--             mkDescribeSuggestersResponse
--
--         , responseUpdateAvailabilityOptions $
--             mkUpdateAvailabilityOptionsResponse
--
--         , responseDeleteExpression $
--             mkDeleteExpressionResponse
--
--         , responseListDomainNames $
--             mkListDomainNamesResponse
--
--         , responseDefineSuggester $
--             mkDefineSuggesterResponse
--
--         , responseDescribeDomains $
--             mkDescribeDomainsResponse
--
--         , responseDeleteAnalysisScheme $
--             mkDeleteAnalysisSchemeResponse
--
--         , responseDescribeDomainEndpointOptions $
--             mkDescribeDomainEndpointOptionsResponse
--
--         , responseDescribeAnalysisSchemes $
--             mkDescribeAnalysisSchemesResponse
--
--         , responseCreateDomain $
--             mkCreateDomainResponse
--
--         , responseUpdateDomainEndpointOptions $
--             mkUpdateDomainEndpointOptionsResponse
--
--         , responseDescribeIndexFields $
--             mkDescribeIndexFieldsResponse
--
--         , responseDeleteSuggester $
--             mkDeleteSuggesterResponse
--
--         , responseDefineAnalysisScheme $
--             mkDefineAnalysisSchemeResponse
--
--         , responseIndexDocuments $
--             mkIndexDocumentsResponse
--
--         , responseDeleteIndexField $
--             mkDeleteIndexFieldResponse
--
--         , responseUpdateServiceAccessPolicies $
--             mkUpdateServiceAccessPoliciesResponse
--
--         , responseUpdateScalingParameters $
--             mkUpdateScalingParametersResponse
--
--         , responseBuildSuggesters $
--             mkBuildSuggestersResponse
--
--         , responseDeleteDomain $
--             mkDeleteDomainResponse
--
--         , responseDefineIndexField $
--             mkDefineIndexFieldResponse
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

requestDescribeDomainEndpointOptions :: DescribeDomainEndpointOptions -> TestTree
requestDescribeDomainEndpointOptions = req
    "DescribeDomainEndpointOptions"
    "fixture/DescribeDomainEndpointOptions.yaml"

requestDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
requestDescribeAnalysisSchemes = req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestUpdateDomainEndpointOptions :: UpdateDomainEndpointOptions -> TestTree
requestUpdateDomainEndpointOptions = req
    "UpdateDomainEndpointOptions"
    "fixture/UpdateDomainEndpointOptions.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy DescribeAvailabilityOptions)

responseDescribeExpressions :: DescribeExpressionsResponse -> TestTree
responseDescribeExpressions = res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeExpressions)

responseDefineExpression :: DefineExpressionResponse -> TestTree
responseDefineExpression = res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DefineExpression)

responseDescribeScalingParameters :: DescribeScalingParametersResponse -> TestTree
responseDescribeScalingParameters = res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeScalingParameters)

responseDescribeServiceAccessPolicies :: DescribeServiceAccessPoliciesResponse -> TestTree
responseDescribeServiceAccessPolicies = res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeServiceAccessPolicies)

responseDescribeSuggesters :: DescribeSuggestersResponse -> TestTree
responseDescribeSuggesters = res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSuggesters)

responseUpdateAvailabilityOptions :: UpdateAvailabilityOptionsResponse -> TestTree
responseUpdateAvailabilityOptions = res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAvailabilityOptions)

responseDeleteExpression :: DeleteExpressionResponse -> TestTree
responseDeleteExpression = res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteExpression)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDomainNames)

responseDefineSuggester :: DefineSuggesterResponse -> TestTree
responseDefineSuggester = res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DefineSuggester)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains = res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDomains)

responseDeleteAnalysisScheme :: DeleteAnalysisSchemeResponse -> TestTree
responseDeleteAnalysisScheme = res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAnalysisScheme)

responseDescribeDomainEndpointOptions :: DescribeDomainEndpointOptionsResponse -> TestTree
responseDescribeDomainEndpointOptions = res
    "DescribeDomainEndpointOptionsResponse"
    "fixture/DescribeDomainEndpointOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDomainEndpointOptions)

responseDescribeAnalysisSchemes :: DescribeAnalysisSchemesResponse -> TestTree
responseDescribeAnalysisSchemes = res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAnalysisSchemes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDomain)

responseUpdateDomainEndpointOptions :: UpdateDomainEndpointOptionsResponse -> TestTree
responseUpdateDomainEndpointOptions = res
    "UpdateDomainEndpointOptionsResponse"
    "fixture/UpdateDomainEndpointOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainEndpointOptions)

responseDescribeIndexFields :: DescribeIndexFieldsResponse -> TestTree
responseDescribeIndexFields = res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIndexFields)

responseDeleteSuggester :: DeleteSuggesterResponse -> TestTree
responseDeleteSuggester = res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSuggester)

responseDefineAnalysisScheme :: DefineAnalysisSchemeResponse -> TestTree
responseDefineAnalysisScheme = res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DefineAnalysisScheme)

responseIndexDocuments :: IndexDocumentsResponse -> TestTree
responseIndexDocuments = res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy IndexDocuments)

responseDeleteIndexField :: DeleteIndexFieldResponse -> TestTree
responseDeleteIndexField = res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIndexField)

responseUpdateServiceAccessPolicies :: UpdateServiceAccessPoliciesResponse -> TestTree
responseUpdateServiceAccessPolicies = res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateServiceAccessPolicies)

responseUpdateScalingParameters :: UpdateScalingParametersResponse -> TestTree
responseUpdateScalingParameters = res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateScalingParameters)

responseBuildSuggesters :: BuildSuggestersResponse -> TestTree
responseBuildSuggesters = res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BuildSuggesters)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDomain)

responseDefineIndexField :: DefineIndexFieldResponse -> TestTree
responseDefineIndexField = res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DefineIndexField)
