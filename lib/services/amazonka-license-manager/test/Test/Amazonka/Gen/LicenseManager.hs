{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LicenseManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LicenseManager where

import Amazonka.LicenseManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LicenseManager.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListLicenseManagerReportGenerators $
--             newListLicenseManagerReportGenerators
--
--         , requestDeleteLicenseManagerReportGenerator $
--             newDeleteLicenseManagerReportGenerator
--
--         , requestUpdateLicenseManagerReportGenerator $
--             newUpdateLicenseManagerReportGenerator
--
--         , requestListUsageForLicenseConfiguration $
--             newListUsageForLicenseConfiguration
--
--         , requestCreateLicenseConfiguration $
--             newCreateLicenseConfiguration
--
--         , requestCreateLicense $
--             newCreateLicense
--
--         , requestListLicenseConversionTasks $
--             newListLicenseConversionTasks
--
--         , requestListResourceInventory $
--             newListResourceInventory
--
--         , requestDeleteToken $
--             newDeleteToken
--
--         , requestDeleteLicenseConfiguration $
--             newDeleteLicenseConfiguration
--
--         , requestUpdateLicenseConfiguration $
--             newUpdateLicenseConfiguration
--
--         , requestCheckInLicense $
--             newCheckInLicense
--
--         , requestListTokens $
--             newListTokens
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateGrant $
--             newCreateGrant
--
--         , requestUpdateLicenseSpecificationsForResource $
--             newUpdateLicenseSpecificationsForResource
--
--         , requestCreateLicenseVersion $
--             newCreateLicenseVersion
--
--         , requestGetLicense $
--             newGetLicense
--
--         , requestGetLicenseConfiguration $
--             newGetLicenseConfiguration
--
--         , requestListReceivedGrants $
--             newListReceivedGrants
--
--         , requestGetLicenseConversionTask $
--             newGetLicenseConversionTask
--
--         , requestGetLicenseUsage $
--             newGetLicenseUsage
--
--         , requestExtendLicenseConsumption $
--             newExtendLicenseConsumption
--
--         , requestGetGrant $
--             newGetGrant
--
--         , requestCheckoutLicense $
--             newCheckoutLicense
--
--         , requestCreateLicenseConversionTaskForResource $
--             newCreateLicenseConversionTaskForResource
--
--         , requestAcceptGrant $
--             newAcceptGrant
--
--         , requestListLicenseSpecificationsForResource $
--             newListLicenseSpecificationsForResource
--
--         , requestCheckoutBorrowLicense $
--             newCheckoutBorrowLicense
--
--         , requestGetServiceSettings $
--             newGetServiceSettings
--
--         , requestRejectGrant $
--             newRejectGrant
--
--         , requestUpdateServiceSettings $
--             newUpdateServiceSettings
--
--         , requestListDistributedGrants $
--             newListDistributedGrants
--
--         , requestListFailuresForLicenseConfigurationOperations $
--             newListFailuresForLicenseConfigurationOperations
--
--         , requestDeleteGrant $
--             newDeleteGrant
--
--         , requestCreateToken $
--             newCreateToken
--
--         , requestDeleteLicense $
--             newDeleteLicense
--
--         , requestListLicenses $
--             newListLicenses
--
--         , requestListLicenseConfigurations $
--             newListLicenseConfigurations
--
--         , requestListReceivedLicenses $
--             newListReceivedLicenses
--
--         , requestCreateGrantVersion $
--             newCreateGrantVersion
--
--         , requestListAssociationsForLicenseConfiguration $
--             newListAssociationsForLicenseConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListLicenseVersions $
--             newListLicenseVersions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetLicenseManagerReportGenerator $
--             newGetLicenseManagerReportGenerator
--
--         , requestGetAccessToken $
--             newGetAccessToken
--
--         , requestCreateLicenseManagerReportGenerator $
--             newCreateLicenseManagerReportGenerator
--
--           ]

--     , testGroup "response"
--         [ responseListLicenseManagerReportGenerators $
--             newListLicenseManagerReportGeneratorsResponse
--
--         , responseDeleteLicenseManagerReportGenerator $
--             newDeleteLicenseManagerReportGeneratorResponse
--
--         , responseUpdateLicenseManagerReportGenerator $
--             newUpdateLicenseManagerReportGeneratorResponse
--
--         , responseListUsageForLicenseConfiguration $
--             newListUsageForLicenseConfigurationResponse
--
--         , responseCreateLicenseConfiguration $
--             newCreateLicenseConfigurationResponse
--
--         , responseCreateLicense $
--             newCreateLicenseResponse
--
--         , responseListLicenseConversionTasks $
--             newListLicenseConversionTasksResponse
--
--         , responseListResourceInventory $
--             newListResourceInventoryResponse
--
--         , responseDeleteToken $
--             newDeleteTokenResponse
--
--         , responseDeleteLicenseConfiguration $
--             newDeleteLicenseConfigurationResponse
--
--         , responseUpdateLicenseConfiguration $
--             newUpdateLicenseConfigurationResponse
--
--         , responseCheckInLicense $
--             newCheckInLicenseResponse
--
--         , responseListTokens $
--             newListTokensResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateGrant $
--             newCreateGrantResponse
--
--         , responseUpdateLicenseSpecificationsForResource $
--             newUpdateLicenseSpecificationsForResourceResponse
--
--         , responseCreateLicenseVersion $
--             newCreateLicenseVersionResponse
--
--         , responseGetLicense $
--             newGetLicenseResponse
--
--         , responseGetLicenseConfiguration $
--             newGetLicenseConfigurationResponse
--
--         , responseListReceivedGrants $
--             newListReceivedGrantsResponse
--
--         , responseGetLicenseConversionTask $
--             newGetLicenseConversionTaskResponse
--
--         , responseGetLicenseUsage $
--             newGetLicenseUsageResponse
--
--         , responseExtendLicenseConsumption $
--             newExtendLicenseConsumptionResponse
--
--         , responseGetGrant $
--             newGetGrantResponse
--
--         , responseCheckoutLicense $
--             newCheckoutLicenseResponse
--
--         , responseCreateLicenseConversionTaskForResource $
--             newCreateLicenseConversionTaskForResourceResponse
--
--         , responseAcceptGrant $
--             newAcceptGrantResponse
--
--         , responseListLicenseSpecificationsForResource $
--             newListLicenseSpecificationsForResourceResponse
--
--         , responseCheckoutBorrowLicense $
--             newCheckoutBorrowLicenseResponse
--
--         , responseGetServiceSettings $
--             newGetServiceSettingsResponse
--
--         , responseRejectGrant $
--             newRejectGrantResponse
--
--         , responseUpdateServiceSettings $
--             newUpdateServiceSettingsResponse
--
--         , responseListDistributedGrants $
--             newListDistributedGrantsResponse
--
--         , responseListFailuresForLicenseConfigurationOperations $
--             newListFailuresForLicenseConfigurationOperationsResponse
--
--         , responseDeleteGrant $
--             newDeleteGrantResponse
--
--         , responseCreateToken $
--             newCreateTokenResponse
--
--         , responseDeleteLicense $
--             newDeleteLicenseResponse
--
--         , responseListLicenses $
--             newListLicensesResponse
--
--         , responseListLicenseConfigurations $
--             newListLicenseConfigurationsResponse
--
--         , responseListReceivedLicenses $
--             newListReceivedLicensesResponse
--
--         , responseCreateGrantVersion $
--             newCreateGrantVersionResponse
--
--         , responseListAssociationsForLicenseConfiguration $
--             newListAssociationsForLicenseConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListLicenseVersions $
--             newListLicenseVersionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetLicenseManagerReportGenerator $
--             newGetLicenseManagerReportGeneratorResponse
--
--         , responseGetAccessToken $
--             newGetAccessTokenResponse
--
--         , responseCreateLicenseManagerReportGenerator $
--             newCreateLicenseManagerReportGeneratorResponse
--
--           ]
--     ]

-- Requests

requestListLicenseManagerReportGenerators :: ListLicenseManagerReportGenerators -> TestTree
requestListLicenseManagerReportGenerators =
  req
    "ListLicenseManagerReportGenerators"
    "fixture/ListLicenseManagerReportGenerators.yaml"

requestDeleteLicenseManagerReportGenerator :: DeleteLicenseManagerReportGenerator -> TestTree
requestDeleteLicenseManagerReportGenerator =
  req
    "DeleteLicenseManagerReportGenerator"
    "fixture/DeleteLicenseManagerReportGenerator.yaml"

requestUpdateLicenseManagerReportGenerator :: UpdateLicenseManagerReportGenerator -> TestTree
requestUpdateLicenseManagerReportGenerator =
  req
    "UpdateLicenseManagerReportGenerator"
    "fixture/UpdateLicenseManagerReportGenerator.yaml"

requestListUsageForLicenseConfiguration :: ListUsageForLicenseConfiguration -> TestTree
requestListUsageForLicenseConfiguration =
  req
    "ListUsageForLicenseConfiguration"
    "fixture/ListUsageForLicenseConfiguration.yaml"

requestCreateLicenseConfiguration :: CreateLicenseConfiguration -> TestTree
requestCreateLicenseConfiguration =
  req
    "CreateLicenseConfiguration"
    "fixture/CreateLicenseConfiguration.yaml"

requestCreateLicense :: CreateLicense -> TestTree
requestCreateLicense =
  req
    "CreateLicense"
    "fixture/CreateLicense.yaml"

requestListLicenseConversionTasks :: ListLicenseConversionTasks -> TestTree
requestListLicenseConversionTasks =
  req
    "ListLicenseConversionTasks"
    "fixture/ListLicenseConversionTasks.yaml"

requestListResourceInventory :: ListResourceInventory -> TestTree
requestListResourceInventory =
  req
    "ListResourceInventory"
    "fixture/ListResourceInventory.yaml"

requestDeleteToken :: DeleteToken -> TestTree
requestDeleteToken =
  req
    "DeleteToken"
    "fixture/DeleteToken.yaml"

requestDeleteLicenseConfiguration :: DeleteLicenseConfiguration -> TestTree
requestDeleteLicenseConfiguration =
  req
    "DeleteLicenseConfiguration"
    "fixture/DeleteLicenseConfiguration.yaml"

requestUpdateLicenseConfiguration :: UpdateLicenseConfiguration -> TestTree
requestUpdateLicenseConfiguration =
  req
    "UpdateLicenseConfiguration"
    "fixture/UpdateLicenseConfiguration.yaml"

requestCheckInLicense :: CheckInLicense -> TestTree
requestCheckInLicense =
  req
    "CheckInLicense"
    "fixture/CheckInLicense.yaml"

requestListTokens :: ListTokens -> TestTree
requestListTokens =
  req
    "ListTokens"
    "fixture/ListTokens.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateGrant :: CreateGrant -> TestTree
requestCreateGrant =
  req
    "CreateGrant"
    "fixture/CreateGrant.yaml"

requestUpdateLicenseSpecificationsForResource :: UpdateLicenseSpecificationsForResource -> TestTree
requestUpdateLicenseSpecificationsForResource =
  req
    "UpdateLicenseSpecificationsForResource"
    "fixture/UpdateLicenseSpecificationsForResource.yaml"

requestCreateLicenseVersion :: CreateLicenseVersion -> TestTree
requestCreateLicenseVersion =
  req
    "CreateLicenseVersion"
    "fixture/CreateLicenseVersion.yaml"

requestGetLicense :: GetLicense -> TestTree
requestGetLicense =
  req
    "GetLicense"
    "fixture/GetLicense.yaml"

requestGetLicenseConfiguration :: GetLicenseConfiguration -> TestTree
requestGetLicenseConfiguration =
  req
    "GetLicenseConfiguration"
    "fixture/GetLicenseConfiguration.yaml"

requestListReceivedGrants :: ListReceivedGrants -> TestTree
requestListReceivedGrants =
  req
    "ListReceivedGrants"
    "fixture/ListReceivedGrants.yaml"

requestGetLicenseConversionTask :: GetLicenseConversionTask -> TestTree
requestGetLicenseConversionTask =
  req
    "GetLicenseConversionTask"
    "fixture/GetLicenseConversionTask.yaml"

requestGetLicenseUsage :: GetLicenseUsage -> TestTree
requestGetLicenseUsage =
  req
    "GetLicenseUsage"
    "fixture/GetLicenseUsage.yaml"

requestExtendLicenseConsumption :: ExtendLicenseConsumption -> TestTree
requestExtendLicenseConsumption =
  req
    "ExtendLicenseConsumption"
    "fixture/ExtendLicenseConsumption.yaml"

requestGetGrant :: GetGrant -> TestTree
requestGetGrant =
  req
    "GetGrant"
    "fixture/GetGrant.yaml"

requestCheckoutLicense :: CheckoutLicense -> TestTree
requestCheckoutLicense =
  req
    "CheckoutLicense"
    "fixture/CheckoutLicense.yaml"

requestCreateLicenseConversionTaskForResource :: CreateLicenseConversionTaskForResource -> TestTree
requestCreateLicenseConversionTaskForResource =
  req
    "CreateLicenseConversionTaskForResource"
    "fixture/CreateLicenseConversionTaskForResource.yaml"

requestAcceptGrant :: AcceptGrant -> TestTree
requestAcceptGrant =
  req
    "AcceptGrant"
    "fixture/AcceptGrant.yaml"

requestListLicenseSpecificationsForResource :: ListLicenseSpecificationsForResource -> TestTree
requestListLicenseSpecificationsForResource =
  req
    "ListLicenseSpecificationsForResource"
    "fixture/ListLicenseSpecificationsForResource.yaml"

requestCheckoutBorrowLicense :: CheckoutBorrowLicense -> TestTree
requestCheckoutBorrowLicense =
  req
    "CheckoutBorrowLicense"
    "fixture/CheckoutBorrowLicense.yaml"

requestGetServiceSettings :: GetServiceSettings -> TestTree
requestGetServiceSettings =
  req
    "GetServiceSettings"
    "fixture/GetServiceSettings.yaml"

requestRejectGrant :: RejectGrant -> TestTree
requestRejectGrant =
  req
    "RejectGrant"
    "fixture/RejectGrant.yaml"

requestUpdateServiceSettings :: UpdateServiceSettings -> TestTree
requestUpdateServiceSettings =
  req
    "UpdateServiceSettings"
    "fixture/UpdateServiceSettings.yaml"

requestListDistributedGrants :: ListDistributedGrants -> TestTree
requestListDistributedGrants =
  req
    "ListDistributedGrants"
    "fixture/ListDistributedGrants.yaml"

requestListFailuresForLicenseConfigurationOperations :: ListFailuresForLicenseConfigurationOperations -> TestTree
requestListFailuresForLicenseConfigurationOperations =
  req
    "ListFailuresForLicenseConfigurationOperations"
    "fixture/ListFailuresForLicenseConfigurationOperations.yaml"

requestDeleteGrant :: DeleteGrant -> TestTree
requestDeleteGrant =
  req
    "DeleteGrant"
    "fixture/DeleteGrant.yaml"

requestCreateToken :: CreateToken -> TestTree
requestCreateToken =
  req
    "CreateToken"
    "fixture/CreateToken.yaml"

requestDeleteLicense :: DeleteLicense -> TestTree
requestDeleteLicense =
  req
    "DeleteLicense"
    "fixture/DeleteLicense.yaml"

requestListLicenses :: ListLicenses -> TestTree
requestListLicenses =
  req
    "ListLicenses"
    "fixture/ListLicenses.yaml"

requestListLicenseConfigurations :: ListLicenseConfigurations -> TestTree
requestListLicenseConfigurations =
  req
    "ListLicenseConfigurations"
    "fixture/ListLicenseConfigurations.yaml"

requestListReceivedLicenses :: ListReceivedLicenses -> TestTree
requestListReceivedLicenses =
  req
    "ListReceivedLicenses"
    "fixture/ListReceivedLicenses.yaml"

requestCreateGrantVersion :: CreateGrantVersion -> TestTree
requestCreateGrantVersion =
  req
    "CreateGrantVersion"
    "fixture/CreateGrantVersion.yaml"

requestListAssociationsForLicenseConfiguration :: ListAssociationsForLicenseConfiguration -> TestTree
requestListAssociationsForLicenseConfiguration =
  req
    "ListAssociationsForLicenseConfiguration"
    "fixture/ListAssociationsForLicenseConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListLicenseVersions :: ListLicenseVersions -> TestTree
requestListLicenseVersions =
  req
    "ListLicenseVersions"
    "fixture/ListLicenseVersions.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetLicenseManagerReportGenerator :: GetLicenseManagerReportGenerator -> TestTree
requestGetLicenseManagerReportGenerator =
  req
    "GetLicenseManagerReportGenerator"
    "fixture/GetLicenseManagerReportGenerator.yaml"

requestGetAccessToken :: GetAccessToken -> TestTree
requestGetAccessToken =
  req
    "GetAccessToken"
    "fixture/GetAccessToken.yaml"

requestCreateLicenseManagerReportGenerator :: CreateLicenseManagerReportGenerator -> TestTree
requestCreateLicenseManagerReportGenerator =
  req
    "CreateLicenseManagerReportGenerator"
    "fixture/CreateLicenseManagerReportGenerator.yaml"

-- Responses

responseListLicenseManagerReportGenerators :: ListLicenseManagerReportGeneratorsResponse -> TestTree
responseListLicenseManagerReportGenerators =
  res
    "ListLicenseManagerReportGeneratorsResponse"
    "fixture/ListLicenseManagerReportGeneratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseManagerReportGenerators)

responseDeleteLicenseManagerReportGenerator :: DeleteLicenseManagerReportGeneratorResponse -> TestTree
responseDeleteLicenseManagerReportGenerator =
  res
    "DeleteLicenseManagerReportGeneratorResponse"
    "fixture/DeleteLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicenseManagerReportGenerator)

responseUpdateLicenseManagerReportGenerator :: UpdateLicenseManagerReportGeneratorResponse -> TestTree
responseUpdateLicenseManagerReportGenerator =
  res
    "UpdateLicenseManagerReportGeneratorResponse"
    "fixture/UpdateLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseManagerReportGenerator)

responseListUsageForLicenseConfiguration :: ListUsageForLicenseConfigurationResponse -> TestTree
responseListUsageForLicenseConfiguration =
  res
    "ListUsageForLicenseConfigurationResponse"
    "fixture/ListUsageForLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsageForLicenseConfiguration)

responseCreateLicenseConfiguration :: CreateLicenseConfigurationResponse -> TestTree
responseCreateLicenseConfiguration =
  res
    "CreateLicenseConfigurationResponse"
    "fixture/CreateLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseConfiguration)

responseCreateLicense :: CreateLicenseResponse -> TestTree
responseCreateLicense =
  res
    "CreateLicenseResponse"
    "fixture/CreateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicense)

responseListLicenseConversionTasks :: ListLicenseConversionTasksResponse -> TestTree
responseListLicenseConversionTasks =
  res
    "ListLicenseConversionTasksResponse"
    "fixture/ListLicenseConversionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseConversionTasks)

responseListResourceInventory :: ListResourceInventoryResponse -> TestTree
responseListResourceInventory =
  res
    "ListResourceInventoryResponse"
    "fixture/ListResourceInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceInventory)

responseDeleteToken :: DeleteTokenResponse -> TestTree
responseDeleteToken =
  res
    "DeleteTokenResponse"
    "fixture/DeleteTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteToken)

responseDeleteLicenseConfiguration :: DeleteLicenseConfigurationResponse -> TestTree
responseDeleteLicenseConfiguration =
  res
    "DeleteLicenseConfigurationResponse"
    "fixture/DeleteLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicenseConfiguration)

responseUpdateLicenseConfiguration :: UpdateLicenseConfigurationResponse -> TestTree
responseUpdateLicenseConfiguration =
  res
    "UpdateLicenseConfigurationResponse"
    "fixture/UpdateLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseConfiguration)

responseCheckInLicense :: CheckInLicenseResponse -> TestTree
responseCheckInLicense =
  res
    "CheckInLicenseResponse"
    "fixture/CheckInLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckInLicense)

responseListTokens :: ListTokensResponse -> TestTree
responseListTokens =
  res
    "ListTokensResponse"
    "fixture/ListTokensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTokens)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateGrant :: CreateGrantResponse -> TestTree
responseCreateGrant =
  res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrant)

responseUpdateLicenseSpecificationsForResource :: UpdateLicenseSpecificationsForResourceResponse -> TestTree
responseUpdateLicenseSpecificationsForResource =
  res
    "UpdateLicenseSpecificationsForResourceResponse"
    "fixture/UpdateLicenseSpecificationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseSpecificationsForResource)

responseCreateLicenseVersion :: CreateLicenseVersionResponse -> TestTree
responseCreateLicenseVersion =
  res
    "CreateLicenseVersionResponse"
    "fixture/CreateLicenseVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseVersion)

responseGetLicense :: GetLicenseResponse -> TestTree
responseGetLicense =
  res
    "GetLicenseResponse"
    "fixture/GetLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicense)

responseGetLicenseConfiguration :: GetLicenseConfigurationResponse -> TestTree
responseGetLicenseConfiguration =
  res
    "GetLicenseConfigurationResponse"
    "fixture/GetLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseConfiguration)

responseListReceivedGrants :: ListReceivedGrantsResponse -> TestTree
responseListReceivedGrants =
  res
    "ListReceivedGrantsResponse"
    "fixture/ListReceivedGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedGrants)

responseGetLicenseConversionTask :: GetLicenseConversionTaskResponse -> TestTree
responseGetLicenseConversionTask =
  res
    "GetLicenseConversionTaskResponse"
    "fixture/GetLicenseConversionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseConversionTask)

responseGetLicenseUsage :: GetLicenseUsageResponse -> TestTree
responseGetLicenseUsage =
  res
    "GetLicenseUsageResponse"
    "fixture/GetLicenseUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseUsage)

responseExtendLicenseConsumption :: ExtendLicenseConsumptionResponse -> TestTree
responseExtendLicenseConsumption =
  res
    "ExtendLicenseConsumptionResponse"
    "fixture/ExtendLicenseConsumptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExtendLicenseConsumption)

responseGetGrant :: GetGrantResponse -> TestTree
responseGetGrant =
  res
    "GetGrantResponse"
    "fixture/GetGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGrant)

responseCheckoutLicense :: CheckoutLicenseResponse -> TestTree
responseCheckoutLicense =
  res
    "CheckoutLicenseResponse"
    "fixture/CheckoutLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckoutLicense)

responseCreateLicenseConversionTaskForResource :: CreateLicenseConversionTaskForResourceResponse -> TestTree
responseCreateLicenseConversionTaskForResource =
  res
    "CreateLicenseConversionTaskForResourceResponse"
    "fixture/CreateLicenseConversionTaskForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseConversionTaskForResource)

responseAcceptGrant :: AcceptGrantResponse -> TestTree
responseAcceptGrant =
  res
    "AcceptGrantResponse"
    "fixture/AcceptGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptGrant)

responseListLicenseSpecificationsForResource :: ListLicenseSpecificationsForResourceResponse -> TestTree
responseListLicenseSpecificationsForResource =
  res
    "ListLicenseSpecificationsForResourceResponse"
    "fixture/ListLicenseSpecificationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseSpecificationsForResource)

responseCheckoutBorrowLicense :: CheckoutBorrowLicenseResponse -> TestTree
responseCheckoutBorrowLicense =
  res
    "CheckoutBorrowLicenseResponse"
    "fixture/CheckoutBorrowLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckoutBorrowLicense)

responseGetServiceSettings :: GetServiceSettingsResponse -> TestTree
responseGetServiceSettings =
  res
    "GetServiceSettingsResponse"
    "fixture/GetServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceSettings)

responseRejectGrant :: RejectGrantResponse -> TestTree
responseRejectGrant =
  res
    "RejectGrantResponse"
    "fixture/RejectGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectGrant)

responseUpdateServiceSettings :: UpdateServiceSettingsResponse -> TestTree
responseUpdateServiceSettings =
  res
    "UpdateServiceSettingsResponse"
    "fixture/UpdateServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSettings)

responseListDistributedGrants :: ListDistributedGrantsResponse -> TestTree
responseListDistributedGrants =
  res
    "ListDistributedGrantsResponse"
    "fixture/ListDistributedGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributedGrants)

responseListFailuresForLicenseConfigurationOperations :: ListFailuresForLicenseConfigurationOperationsResponse -> TestTree
responseListFailuresForLicenseConfigurationOperations =
  res
    "ListFailuresForLicenseConfigurationOperationsResponse"
    "fixture/ListFailuresForLicenseConfigurationOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFailuresForLicenseConfigurationOperations)

responseDeleteGrant :: DeleteGrantResponse -> TestTree
responseDeleteGrant =
  res
    "DeleteGrantResponse"
    "fixture/DeleteGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGrant)

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateToken)

responseDeleteLicense :: DeleteLicenseResponse -> TestTree
responseDeleteLicense =
  res
    "DeleteLicenseResponse"
    "fixture/DeleteLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicense)

responseListLicenses :: ListLicensesResponse -> TestTree
responseListLicenses =
  res
    "ListLicensesResponse"
    "fixture/ListLicensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenses)

responseListLicenseConfigurations :: ListLicenseConfigurationsResponse -> TestTree
responseListLicenseConfigurations =
  res
    "ListLicenseConfigurationsResponse"
    "fixture/ListLicenseConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseConfigurations)

responseListReceivedLicenses :: ListReceivedLicensesResponse -> TestTree
responseListReceivedLicenses =
  res
    "ListReceivedLicensesResponse"
    "fixture/ListReceivedLicensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedLicenses)

responseCreateGrantVersion :: CreateGrantVersionResponse -> TestTree
responseCreateGrantVersion =
  res
    "CreateGrantVersionResponse"
    "fixture/CreateGrantVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrantVersion)

responseListAssociationsForLicenseConfiguration :: ListAssociationsForLicenseConfigurationResponse -> TestTree
responseListAssociationsForLicenseConfiguration =
  res
    "ListAssociationsForLicenseConfigurationResponse"
    "fixture/ListAssociationsForLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociationsForLicenseConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListLicenseVersions :: ListLicenseVersionsResponse -> TestTree
responseListLicenseVersions =
  res
    "ListLicenseVersionsResponse"
    "fixture/ListLicenseVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseVersions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetLicenseManagerReportGenerator :: GetLicenseManagerReportGeneratorResponse -> TestTree
responseGetLicenseManagerReportGenerator =
  res
    "GetLicenseManagerReportGeneratorResponse"
    "fixture/GetLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseManagerReportGenerator)

responseGetAccessToken :: GetAccessTokenResponse -> TestTree
responseGetAccessToken =
  res
    "GetAccessTokenResponse"
    "fixture/GetAccessTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessToken)

responseCreateLicenseManagerReportGenerator :: CreateLicenseManagerReportGeneratorResponse -> TestTree
responseCreateLicenseManagerReportGenerator =
  res
    "CreateLicenseManagerReportGeneratorResponse"
    "fixture/CreateLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseManagerReportGenerator)
