{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestDefineExpression $
--             newDefineExpression
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptions
--
--         , requestDefineSuggester $
--             newDefineSuggester
--
--         , requestDescribeDomains $
--             newDescribeDomains
--
--         , requestDeleteExpression $
--             newDeleteExpression
--
--         , requestDeleteIndexField $
--             newDeleteIndexField
--
--         , requestUpdateDomainEndpointOptions $
--             newUpdateDomainEndpointOptions
--
--         , requestDescribeIndexFields $
--             newDescribeIndexFields
--
--         , requestDeleteSuggester $
--             newDeleteSuggester
--
--         , requestDescribeExpressions $
--             newDescribeExpressions
--
--         , requestDescribeAvailabilityOptions $
--             newDescribeAvailabilityOptions
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDefineIndexField $
--             newDefineIndexField
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemes
--
--         , requestDeleteAnalysisScheme $
--             newDeleteAnalysisScheme
--
--         , requestBuildSuggesters $
--             newBuildSuggesters
--
--         , requestUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPolicies
--
--         , requestUpdateScalingParameters $
--             newUpdateScalingParameters
--
--         , requestUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptions
--
--         , requestDescribeSuggesters $
--             newDescribeSuggesters
--
--         , requestIndexDocuments $
--             newIndexDocuments
--
--         , requestDescribeServiceAccessPolicies $
--             newDescribeServiceAccessPolicies
--
--         , requestDefineAnalysisScheme $
--             newDefineAnalysisScheme
--
--         , requestDescribeScalingParameters $
--             newDescribeScalingParameters
--
--           ]

--     , testGroup "response"
--         [ responseDefineExpression $
--             newDefineExpressionResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptionsResponse
--
--         , responseDefineSuggester $
--             newDefineSuggesterResponse
--
--         , responseDescribeDomains $
--             newDescribeDomainsResponse
--
--         , responseDeleteExpression $
--             newDeleteExpressionResponse
--
--         , responseDeleteIndexField $
--             newDeleteIndexFieldResponse
--
--         , responseUpdateDomainEndpointOptions $
--             newUpdateDomainEndpointOptionsResponse
--
--         , responseDescribeIndexFields $
--             newDescribeIndexFieldsResponse
--
--         , responseDeleteSuggester $
--             newDeleteSuggesterResponse
--
--         , responseDescribeExpressions $
--             newDescribeExpressionsResponse
--
--         , responseDescribeAvailabilityOptions $
--             newDescribeAvailabilityOptionsResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDefineIndexField $
--             newDefineIndexFieldResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemesResponse
--
--         , responseDeleteAnalysisScheme $
--             newDeleteAnalysisSchemeResponse
--
--         , responseBuildSuggesters $
--             newBuildSuggestersResponse
--
--         , responseUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPoliciesResponse
--
--         , responseUpdateScalingParameters $
--             newUpdateScalingParametersResponse
--
--         , responseUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptionsResponse
--
--         , responseDescribeSuggesters $
--             newDescribeSuggestersResponse
--
--         , responseIndexDocuments $
--             newIndexDocumentsResponse
--
--         , responseDescribeServiceAccessPolicies $
--             newDescribeServiceAccessPoliciesResponse
--
--         , responseDefineAnalysisScheme $
--             newDefineAnalysisSchemeResponse
--
--         , responseDescribeScalingParameters $
--             newDescribeScalingParametersResponse
--
--           ]
--     ]

-- Requests

requestDefineExpression :: DefineExpression -> TestTree
requestDefineExpression =
  req
    "DefineExpression"
    "fixture/DefineExpression.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestDescribeDomainEndpointOptions :: DescribeDomainEndpointOptions -> TestTree
requestDescribeDomainEndpointOptions =
  req
    "DescribeDomainEndpointOptions"
    "fixture/DescribeDomainEndpointOptions.yaml"

requestDefineSuggester :: DefineSuggester -> TestTree
requestDefineSuggester =
  req
    "DefineSuggester"
    "fixture/DefineSuggester.yaml"

requestDescribeDomains :: DescribeDomains -> TestTree
requestDescribeDomains =
  req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

requestDeleteExpression :: DeleteExpression -> TestTree
requestDeleteExpression =
  req
    "DeleteExpression"
    "fixture/DeleteExpression.yaml"

requestDeleteIndexField :: DeleteIndexField -> TestTree
requestDeleteIndexField =
  req
    "DeleteIndexField"
    "fixture/DeleteIndexField.yaml"

requestUpdateDomainEndpointOptions :: UpdateDomainEndpointOptions -> TestTree
requestUpdateDomainEndpointOptions =
  req
    "UpdateDomainEndpointOptions"
    "fixture/UpdateDomainEndpointOptions.yaml"

requestDescribeIndexFields :: DescribeIndexFields -> TestTree
requestDescribeIndexFields =
  req
    "DescribeIndexFields"
    "fixture/DescribeIndexFields.yaml"

requestDeleteSuggester :: DeleteSuggester -> TestTree
requestDeleteSuggester =
  req
    "DeleteSuggester"
    "fixture/DeleteSuggester.yaml"

requestDescribeExpressions :: DescribeExpressions -> TestTree
requestDescribeExpressions =
  req
    "DescribeExpressions"
    "fixture/DescribeExpressions.yaml"

requestDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
requestDescribeAvailabilityOptions =
  req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDefineIndexField :: DefineIndexField -> TestTree
requestDefineIndexField =
  req
    "DefineIndexField"
    "fixture/DefineIndexField.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
requestDescribeAnalysisSchemes =
  req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

requestDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
requestDeleteAnalysisScheme =
  req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme.yaml"

requestBuildSuggesters :: BuildSuggesters -> TestTree
requestBuildSuggesters =
  req
    "BuildSuggesters"
    "fixture/BuildSuggesters.yaml"

requestUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
requestUpdateServiceAccessPolicies =
  req
    "UpdateServiceAccessPolicies"
    "fixture/UpdateServiceAccessPolicies.yaml"

requestUpdateScalingParameters :: UpdateScalingParameters -> TestTree
requestUpdateScalingParameters =
  req
    "UpdateScalingParameters"
    "fixture/UpdateScalingParameters.yaml"

requestUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
requestUpdateAvailabilityOptions =
  req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions.yaml"

requestDescribeSuggesters :: DescribeSuggesters -> TestTree
requestDescribeSuggesters =
  req
    "DescribeSuggesters"
    "fixture/DescribeSuggesters.yaml"

requestIndexDocuments :: IndexDocuments -> TestTree
requestIndexDocuments =
  req
    "IndexDocuments"
    "fixture/IndexDocuments.yaml"

requestDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
requestDescribeServiceAccessPolicies =
  req
    "DescribeServiceAccessPolicies"
    "fixture/DescribeServiceAccessPolicies.yaml"

requestDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
requestDefineAnalysisScheme =
  req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme.yaml"

requestDescribeScalingParameters :: DescribeScalingParameters -> TestTree
requestDescribeScalingParameters =
  req
    "DescribeScalingParameters"
    "fixture/DescribeScalingParameters.yaml"

-- Responses

responseDefineExpression :: DefineExpressionResponse -> TestTree
responseDefineExpression =
  res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    defaultService
    (Proxy :: Proxy DefineExpression)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainNames)

responseDescribeDomainEndpointOptions :: DescribeDomainEndpointOptionsResponse -> TestTree
responseDescribeDomainEndpointOptions =
  res
    "DescribeDomainEndpointOptionsResponse"
    "fixture/DescribeDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomainEndpointOptions)

responseDefineSuggester :: DefineSuggesterResponse -> TestTree
responseDefineSuggester =
  res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    defaultService
    (Proxy :: Proxy DefineSuggester)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains =
  res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomains)

responseDeleteExpression :: DeleteExpressionResponse -> TestTree
responseDeleteExpression =
  res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteExpression)

responseDeleteIndexField :: DeleteIndexFieldResponse -> TestTree
responseDeleteIndexField =
  res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIndexField)

responseUpdateDomainEndpointOptions :: UpdateDomainEndpointOptionsResponse -> TestTree
responseUpdateDomainEndpointOptions =
  res
    "UpdateDomainEndpointOptionsResponse"
    "fixture/UpdateDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainEndpointOptions)

responseDescribeIndexFields :: DescribeIndexFieldsResponse -> TestTree
responseDescribeIndexFields =
  res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIndexFields)

responseDeleteSuggester :: DeleteSuggesterResponse -> TestTree
responseDeleteSuggester =
  res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSuggester)

responseDescribeExpressions :: DescribeExpressionsResponse -> TestTree
responseDescribeExpressions =
  res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExpressions)

responseDescribeAvailabilityOptions :: DescribeAvailabilityOptionsResponse -> TestTree
responseDescribeAvailabilityOptions =
  res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAvailabilityOptions)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

responseDefineIndexField :: DefineIndexFieldResponse -> TestTree
responseDefineIndexField =
  res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    defaultService
    (Proxy :: Proxy DefineIndexField)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responseDescribeAnalysisSchemes :: DescribeAnalysisSchemesResponse -> TestTree
responseDescribeAnalysisSchemes =
  res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAnalysisSchemes)

responseDeleteAnalysisScheme :: DeleteAnalysisSchemeResponse -> TestTree
responseDeleteAnalysisScheme =
  res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnalysisScheme)

responseBuildSuggesters :: BuildSuggestersResponse -> TestTree
responseBuildSuggesters =
  res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    defaultService
    (Proxy :: Proxy BuildSuggesters)

responseUpdateServiceAccessPolicies :: UpdateServiceAccessPoliciesResponse -> TestTree
responseUpdateServiceAccessPolicies =
  res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateServiceAccessPolicies)

responseUpdateScalingParameters :: UpdateScalingParametersResponse -> TestTree
responseUpdateScalingParameters =
  res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateScalingParameters)

responseUpdateAvailabilityOptions :: UpdateAvailabilityOptionsResponse -> TestTree
responseUpdateAvailabilityOptions =
  res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAvailabilityOptions)

responseDescribeSuggesters :: DescribeSuggestersResponse -> TestTree
responseDescribeSuggesters =
  res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSuggesters)

responseIndexDocuments :: IndexDocumentsResponse -> TestTree
responseIndexDocuments =
  res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    defaultService
    (Proxy :: Proxy IndexDocuments)

responseDescribeServiceAccessPolicies :: DescribeServiceAccessPoliciesResponse -> TestTree
responseDescribeServiceAccessPolicies =
  res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeServiceAccessPolicies)

responseDefineAnalysisScheme :: DefineAnalysisSchemeResponse -> TestTree
responseDefineAnalysisScheme =
  res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    defaultService
    (Proxy :: Proxy DefineAnalysisScheme)

responseDescribeScalingParameters :: DescribeScalingParametersResponse -> TestTree
responseDescribeScalingParameters =
  res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeScalingParameters)
