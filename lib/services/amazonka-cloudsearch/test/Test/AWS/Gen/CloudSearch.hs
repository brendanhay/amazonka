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

import Amazonka.CloudSearch
import qualified Data.Proxy as Proxy
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
--             newDescribeAvailabilityOptions
--
--         , requestDescribeExpressions $
--             newDescribeExpressions
--
--         , requestDefineExpression $
--             newDefineExpression
--
--         , requestDescribeScalingParameters $
--             newDescribeScalingParameters
--
--         , requestDescribeServiceAccessPolicies $
--             newDescribeServiceAccessPolicies
--
--         , requestDescribeSuggesters $
--             newDescribeSuggesters
--
--         , requestUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptions
--
--         , requestDeleteExpression $
--             newDeleteExpression
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestDefineSuggester $
--             newDefineSuggester
--
--         , requestDescribeDomains $
--             newDescribeDomains
--
--         , requestDeleteAnalysisScheme $
--             newDeleteAnalysisScheme
--
--         , requestDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptions
--
--         , requestDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemes
--
--         , requestCreateDomain $
--             newCreateDomain
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
--         , requestDefineAnalysisScheme $
--             newDefineAnalysisScheme
--
--         , requestIndexDocuments $
--             newIndexDocuments
--
--         , requestDeleteIndexField $
--             newDeleteIndexField
--
--         , requestUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPolicies
--
--         , requestUpdateScalingParameters $
--             newUpdateScalingParameters
--
--         , requestBuildSuggesters $
--             newBuildSuggesters
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDefineIndexField $
--             newDefineIndexField
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAvailabilityOptions $
--             newDescribeAvailabilityOptionsResponse
--
--         , responseDescribeExpressions $
--             newDescribeExpressionsResponse
--
--         , responseDefineExpression $
--             newDefineExpressionResponse
--
--         , responseDescribeScalingParameters $
--             newDescribeScalingParametersResponse
--
--         , responseDescribeServiceAccessPolicies $
--             newDescribeServiceAccessPoliciesResponse
--
--         , responseDescribeSuggesters $
--             newDescribeSuggestersResponse
--
--         , responseUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptionsResponse
--
--         , responseDeleteExpression $
--             newDeleteExpressionResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseDefineSuggester $
--             newDefineSuggesterResponse
--
--         , responseDescribeDomains $
--             newDescribeDomainsResponse
--
--         , responseDeleteAnalysisScheme $
--             newDeleteAnalysisSchemeResponse
--
--         , responseDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptionsResponse
--
--         , responseDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemesResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
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
--         , responseDefineAnalysisScheme $
--             newDefineAnalysisSchemeResponse
--
--         , responseIndexDocuments $
--             newIndexDocumentsResponse
--
--         , responseDeleteIndexField $
--             newDeleteIndexFieldResponse
--
--         , responseUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPoliciesResponse
--
--         , responseUpdateScalingParameters $
--             newUpdateScalingParametersResponse
--
--         , responseBuildSuggesters $
--             newBuildSuggestersResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDefineIndexField $
--             newDefineIndexFieldResponse
--
--           ]
--     ]

-- Requests

requestDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
requestDescribeAvailabilityOptions =
  req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions.yaml"

requestDescribeExpressions :: DescribeExpressions -> TestTree
requestDescribeExpressions =
  req
    "DescribeExpressions"
    "fixture/DescribeExpressions.yaml"

requestDefineExpression :: DefineExpression -> TestTree
requestDefineExpression =
  req
    "DefineExpression"
    "fixture/DefineExpression.yaml"

requestDescribeScalingParameters :: DescribeScalingParameters -> TestTree
requestDescribeScalingParameters =
  req
    "DescribeScalingParameters"
    "fixture/DescribeScalingParameters.yaml"

requestDescribeServiceAccessPolicies :: DescribeServiceAccessPolicies -> TestTree
requestDescribeServiceAccessPolicies =
  req
    "DescribeServiceAccessPolicies"
    "fixture/DescribeServiceAccessPolicies.yaml"

requestDescribeSuggesters :: DescribeSuggesters -> TestTree
requestDescribeSuggesters =
  req
    "DescribeSuggesters"
    "fixture/DescribeSuggesters.yaml"

requestUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
requestUpdateAvailabilityOptions =
  req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions.yaml"

requestDeleteExpression :: DeleteExpression -> TestTree
requestDeleteExpression =
  req
    "DeleteExpression"
    "fixture/DeleteExpression.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

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

requestDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
requestDeleteAnalysisScheme =
  req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme.yaml"

requestDescribeDomainEndpointOptions :: DescribeDomainEndpointOptions -> TestTree
requestDescribeDomainEndpointOptions =
  req
    "DescribeDomainEndpointOptions"
    "fixture/DescribeDomainEndpointOptions.yaml"

requestDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
requestDescribeAnalysisSchemes =
  req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

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

requestDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
requestDefineAnalysisScheme =
  req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme.yaml"

requestIndexDocuments :: IndexDocuments -> TestTree
requestIndexDocuments =
  req
    "IndexDocuments"
    "fixture/IndexDocuments.yaml"

requestDeleteIndexField :: DeleteIndexField -> TestTree
requestDeleteIndexField =
  req
    "DeleteIndexField"
    "fixture/DeleteIndexField.yaml"

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

requestBuildSuggesters :: BuildSuggesters -> TestTree
requestBuildSuggesters =
  req
    "BuildSuggesters"
    "fixture/BuildSuggesters.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDefineIndexField :: DefineIndexField -> TestTree
requestDefineIndexField =
  req
    "DefineIndexField"
    "fixture/DefineIndexField.yaml"

-- Responses

responseDescribeAvailabilityOptions :: DescribeAvailabilityOptionsResponse -> TestTree
responseDescribeAvailabilityOptions =
  res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityOptions)

responseDescribeExpressions :: DescribeExpressionsResponse -> TestTree
responseDescribeExpressions =
  res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExpressions)

responseDefineExpression :: DefineExpressionResponse -> TestTree
responseDefineExpression =
  res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineExpression)

responseDescribeScalingParameters :: DescribeScalingParametersResponse -> TestTree
responseDescribeScalingParameters =
  res
    "DescribeScalingParametersResponse"
    "fixture/DescribeScalingParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeScalingParameters)

responseDescribeServiceAccessPolicies :: DescribeServiceAccessPoliciesResponse -> TestTree
responseDescribeServiceAccessPolicies =
  res
    "DescribeServiceAccessPoliciesResponse"
    "fixture/DescribeServiceAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeServiceAccessPolicies)

responseDescribeSuggesters :: DescribeSuggestersResponse -> TestTree
responseDescribeSuggesters =
  res
    "DescribeSuggestersResponse"
    "fixture/DescribeSuggestersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSuggesters)

responseUpdateAvailabilityOptions :: UpdateAvailabilityOptionsResponse -> TestTree
responseUpdateAvailabilityOptions =
  res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAvailabilityOptions)

responseDeleteExpression :: DeleteExpressionResponse -> TestTree
responseDeleteExpression =
  res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExpression)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responseDefineSuggester :: DefineSuggesterResponse -> TestTree
responseDefineSuggester =
  res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineSuggester)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains =
  res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomains)

responseDeleteAnalysisScheme :: DeleteAnalysisSchemeResponse -> TestTree
responseDeleteAnalysisScheme =
  res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnalysisScheme)

responseDescribeDomainEndpointOptions :: DescribeDomainEndpointOptionsResponse -> TestTree
responseDescribeDomainEndpointOptions =
  res
    "DescribeDomainEndpointOptionsResponse"
    "fixture/DescribeDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainEndpointOptions)

responseDescribeAnalysisSchemes :: DescribeAnalysisSchemesResponse -> TestTree
responseDescribeAnalysisSchemes =
  res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisSchemes)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseUpdateDomainEndpointOptions :: UpdateDomainEndpointOptionsResponse -> TestTree
responseUpdateDomainEndpointOptions =
  res
    "UpdateDomainEndpointOptionsResponse"
    "fixture/UpdateDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainEndpointOptions)

responseDescribeIndexFields :: DescribeIndexFieldsResponse -> TestTree
responseDescribeIndexFields =
  res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndexFields)

responseDeleteSuggester :: DeleteSuggesterResponse -> TestTree
responseDeleteSuggester =
  res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSuggester)

responseDefineAnalysisScheme :: DefineAnalysisSchemeResponse -> TestTree
responseDefineAnalysisScheme =
  res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineAnalysisScheme)

responseIndexDocuments :: IndexDocumentsResponse -> TestTree
responseIndexDocuments =
  res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IndexDocuments)

responseDeleteIndexField :: DeleteIndexFieldResponse -> TestTree
responseDeleteIndexField =
  res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIndexField)

responseUpdateServiceAccessPolicies :: UpdateServiceAccessPoliciesResponse -> TestTree
responseUpdateServiceAccessPolicies =
  res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceAccessPolicies)

responseUpdateScalingParameters :: UpdateScalingParametersResponse -> TestTree
responseUpdateScalingParameters =
  res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScalingParameters)

responseBuildSuggesters :: BuildSuggestersResponse -> TestTree
responseBuildSuggesters =
  res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BuildSuggesters)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDefineIndexField :: DefineIndexFieldResponse -> TestTree
responseDefineIndexField =
  res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineIndexField)
