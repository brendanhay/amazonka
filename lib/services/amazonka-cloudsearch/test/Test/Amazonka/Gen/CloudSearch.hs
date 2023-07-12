{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudSearch where

import Amazonka.CloudSearch
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudSearch.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBuildSuggesters $
--             newBuildSuggesters
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDefineAnalysisScheme $
--             newDefineAnalysisScheme
--
--         , requestDefineExpression $
--             newDefineExpression
--
--         , requestDefineIndexField $
--             newDefineIndexField
--
--         , requestDefineSuggester $
--             newDefineSuggester
--
--         , requestDeleteAnalysisScheme $
--             newDeleteAnalysisScheme
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteExpression $
--             newDeleteExpression
--
--         , requestDeleteIndexField $
--             newDeleteIndexField
--
--         , requestDeleteSuggester $
--             newDeleteSuggester
--
--         , requestDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemes
--
--         , requestDescribeAvailabilityOptions $
--             newDescribeAvailabilityOptions
--
--         , requestDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptions
--
--         , requestDescribeDomains $
--             newDescribeDomains
--
--         , requestDescribeExpressions $
--             newDescribeExpressions
--
--         , requestDescribeIndexFields $
--             newDescribeIndexFields
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
--         , requestIndexDocuments $
--             newIndexDocuments
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptions
--
--         , requestUpdateDomainEndpointOptions $
--             newUpdateDomainEndpointOptions
--
--         , requestUpdateScalingParameters $
--             newUpdateScalingParameters
--
--         , requestUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPolicies
--
--           ]

--     , testGroup "response"
--         [ responseBuildSuggesters $
--             newBuildSuggestersResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDefineAnalysisScheme $
--             newDefineAnalysisSchemeResponse
--
--         , responseDefineExpression $
--             newDefineExpressionResponse
--
--         , responseDefineIndexField $
--             newDefineIndexFieldResponse
--
--         , responseDefineSuggester $
--             newDefineSuggesterResponse
--
--         , responseDeleteAnalysisScheme $
--             newDeleteAnalysisSchemeResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteExpression $
--             newDeleteExpressionResponse
--
--         , responseDeleteIndexField $
--             newDeleteIndexFieldResponse
--
--         , responseDeleteSuggester $
--             newDeleteSuggesterResponse
--
--         , responseDescribeAnalysisSchemes $
--             newDescribeAnalysisSchemesResponse
--
--         , responseDescribeAvailabilityOptions $
--             newDescribeAvailabilityOptionsResponse
--
--         , responseDescribeDomainEndpointOptions $
--             newDescribeDomainEndpointOptionsResponse
--
--         , responseDescribeDomains $
--             newDescribeDomainsResponse
--
--         , responseDescribeExpressions $
--             newDescribeExpressionsResponse
--
--         , responseDescribeIndexFields $
--             newDescribeIndexFieldsResponse
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
--         , responseIndexDocuments $
--             newIndexDocumentsResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseUpdateAvailabilityOptions $
--             newUpdateAvailabilityOptionsResponse
--
--         , responseUpdateDomainEndpointOptions $
--             newUpdateDomainEndpointOptionsResponse
--
--         , responseUpdateScalingParameters $
--             newUpdateScalingParametersResponse
--
--         , responseUpdateServiceAccessPolicies $
--             newUpdateServiceAccessPoliciesResponse
--
--           ]
--     ]

-- Requests

requestBuildSuggesters :: BuildSuggesters -> TestTree
requestBuildSuggesters =
  req
    "BuildSuggesters"
    "fixture/BuildSuggesters.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDefineAnalysisScheme :: DefineAnalysisScheme -> TestTree
requestDefineAnalysisScheme =
  req
    "DefineAnalysisScheme"
    "fixture/DefineAnalysisScheme.yaml"

requestDefineExpression :: DefineExpression -> TestTree
requestDefineExpression =
  req
    "DefineExpression"
    "fixture/DefineExpression.yaml"

requestDefineIndexField :: DefineIndexField -> TestTree
requestDefineIndexField =
  req
    "DefineIndexField"
    "fixture/DefineIndexField.yaml"

requestDefineSuggester :: DefineSuggester -> TestTree
requestDefineSuggester =
  req
    "DefineSuggester"
    "fixture/DefineSuggester.yaml"

requestDeleteAnalysisScheme :: DeleteAnalysisScheme -> TestTree
requestDeleteAnalysisScheme =
  req
    "DeleteAnalysisScheme"
    "fixture/DeleteAnalysisScheme.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

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

requestDeleteSuggester :: DeleteSuggester -> TestTree
requestDeleteSuggester =
  req
    "DeleteSuggester"
    "fixture/DeleteSuggester.yaml"

requestDescribeAnalysisSchemes :: DescribeAnalysisSchemes -> TestTree
requestDescribeAnalysisSchemes =
  req
    "DescribeAnalysisSchemes"
    "fixture/DescribeAnalysisSchemes.yaml"

requestDescribeAvailabilityOptions :: DescribeAvailabilityOptions -> TestTree
requestDescribeAvailabilityOptions =
  req
    "DescribeAvailabilityOptions"
    "fixture/DescribeAvailabilityOptions.yaml"

requestDescribeDomainEndpointOptions :: DescribeDomainEndpointOptions -> TestTree
requestDescribeDomainEndpointOptions =
  req
    "DescribeDomainEndpointOptions"
    "fixture/DescribeDomainEndpointOptions.yaml"

requestDescribeDomains :: DescribeDomains -> TestTree
requestDescribeDomains =
  req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

requestDescribeExpressions :: DescribeExpressions -> TestTree
requestDescribeExpressions =
  req
    "DescribeExpressions"
    "fixture/DescribeExpressions.yaml"

requestDescribeIndexFields :: DescribeIndexFields -> TestTree
requestDescribeIndexFields =
  req
    "DescribeIndexFields"
    "fixture/DescribeIndexFields.yaml"

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

requestIndexDocuments :: IndexDocuments -> TestTree
requestIndexDocuments =
  req
    "IndexDocuments"
    "fixture/IndexDocuments.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestUpdateAvailabilityOptions :: UpdateAvailabilityOptions -> TestTree
requestUpdateAvailabilityOptions =
  req
    "UpdateAvailabilityOptions"
    "fixture/UpdateAvailabilityOptions.yaml"

requestUpdateDomainEndpointOptions :: UpdateDomainEndpointOptions -> TestTree
requestUpdateDomainEndpointOptions =
  req
    "UpdateDomainEndpointOptions"
    "fixture/UpdateDomainEndpointOptions.yaml"

requestUpdateScalingParameters :: UpdateScalingParameters -> TestTree
requestUpdateScalingParameters =
  req
    "UpdateScalingParameters"
    "fixture/UpdateScalingParameters.yaml"

requestUpdateServiceAccessPolicies :: UpdateServiceAccessPolicies -> TestTree
requestUpdateServiceAccessPolicies =
  req
    "UpdateServiceAccessPolicies"
    "fixture/UpdateServiceAccessPolicies.yaml"

-- Responses

responseBuildSuggesters :: BuildSuggestersResponse -> TestTree
responseBuildSuggesters =
  res
    "BuildSuggestersResponse"
    "fixture/BuildSuggestersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BuildSuggesters)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseDefineAnalysisScheme :: DefineAnalysisSchemeResponse -> TestTree
responseDefineAnalysisScheme =
  res
    "DefineAnalysisSchemeResponse"
    "fixture/DefineAnalysisSchemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineAnalysisScheme)

responseDefineExpression :: DefineExpressionResponse -> TestTree
responseDefineExpression =
  res
    "DefineExpressionResponse"
    "fixture/DefineExpressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineExpression)

responseDefineIndexField :: DefineIndexFieldResponse -> TestTree
responseDefineIndexField =
  res
    "DefineIndexFieldResponse"
    "fixture/DefineIndexFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineIndexField)

responseDefineSuggester :: DefineSuggesterResponse -> TestTree
responseDefineSuggester =
  res
    "DefineSuggesterResponse"
    "fixture/DefineSuggesterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DefineSuggester)

responseDeleteAnalysisScheme :: DeleteAnalysisSchemeResponse -> TestTree
responseDeleteAnalysisScheme =
  res
    "DeleteAnalysisSchemeResponse"
    "fixture/DeleteAnalysisSchemeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnalysisScheme)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteExpression :: DeleteExpressionResponse -> TestTree
responseDeleteExpression =
  res
    "DeleteExpressionResponse"
    "fixture/DeleteExpressionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExpression)

responseDeleteIndexField :: DeleteIndexFieldResponse -> TestTree
responseDeleteIndexField =
  res
    "DeleteIndexFieldResponse"
    "fixture/DeleteIndexFieldResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIndexField)

responseDeleteSuggester :: DeleteSuggesterResponse -> TestTree
responseDeleteSuggester =
  res
    "DeleteSuggesterResponse"
    "fixture/DeleteSuggesterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSuggester)

responseDescribeAnalysisSchemes :: DescribeAnalysisSchemesResponse -> TestTree
responseDescribeAnalysisSchemes =
  res
    "DescribeAnalysisSchemesResponse"
    "fixture/DescribeAnalysisSchemesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAnalysisSchemes)

responseDescribeAvailabilityOptions :: DescribeAvailabilityOptionsResponse -> TestTree
responseDescribeAvailabilityOptions =
  res
    "DescribeAvailabilityOptionsResponse"
    "fixture/DescribeAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAvailabilityOptions)

responseDescribeDomainEndpointOptions :: DescribeDomainEndpointOptionsResponse -> TestTree
responseDescribeDomainEndpointOptions =
  res
    "DescribeDomainEndpointOptionsResponse"
    "fixture/DescribeDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainEndpointOptions)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains =
  res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomains)

responseDescribeExpressions :: DescribeExpressionsResponse -> TestTree
responseDescribeExpressions =
  res
    "DescribeExpressionsResponse"
    "fixture/DescribeExpressionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExpressions)

responseDescribeIndexFields :: DescribeIndexFieldsResponse -> TestTree
responseDescribeIndexFields =
  res
    "DescribeIndexFieldsResponse"
    "fixture/DescribeIndexFieldsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIndexFields)

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

responseIndexDocuments :: IndexDocumentsResponse -> TestTree
responseIndexDocuments =
  res
    "IndexDocumentsResponse"
    "fixture/IndexDocumentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IndexDocuments)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responseUpdateAvailabilityOptions :: UpdateAvailabilityOptionsResponse -> TestTree
responseUpdateAvailabilityOptions =
  res
    "UpdateAvailabilityOptionsResponse"
    "fixture/UpdateAvailabilityOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAvailabilityOptions)

responseUpdateDomainEndpointOptions :: UpdateDomainEndpointOptionsResponse -> TestTree
responseUpdateDomainEndpointOptions =
  res
    "UpdateDomainEndpointOptionsResponse"
    "fixture/UpdateDomainEndpointOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainEndpointOptions)

responseUpdateScalingParameters :: UpdateScalingParametersResponse -> TestTree
responseUpdateScalingParameters =
  res
    "UpdateScalingParametersResponse"
    "fixture/UpdateScalingParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateScalingParameters)

responseUpdateServiceAccessPolicies :: UpdateServiceAccessPoliciesResponse -> TestTree
responseUpdateServiceAccessPolicies =
  res
    "UpdateServiceAccessPoliciesResponse"
    "fixture/UpdateServiceAccessPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceAccessPolicies)
