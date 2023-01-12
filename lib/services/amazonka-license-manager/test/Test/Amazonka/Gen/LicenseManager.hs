{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LicenseManager
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestAcceptGrant $
--             newAcceptGrant
--
--         , requestCheckInLicense $
--             newCheckInLicense
--
--         , requestCheckoutBorrowLicense $
--             newCheckoutBorrowLicense
--
--         , requestCheckoutLicense $
--             newCheckoutLicense
--
--         , requestCreateGrant $
--             newCreateGrant
--
--         , requestCreateGrantVersion $
--             newCreateGrantVersion
--
--         , requestCreateLicense $
--             newCreateLicense
--
--         , requestCreateLicenseConfiguration $
--             newCreateLicenseConfiguration
--
--         , requestCreateLicenseConversionTaskForResource $
--             newCreateLicenseConversionTaskForResource
--
--         , requestCreateLicenseManagerReportGenerator $
--             newCreateLicenseManagerReportGenerator
--
--         , requestCreateLicenseVersion $
--             newCreateLicenseVersion
--
--         , requestCreateToken $
--             newCreateToken
--
--         , requestDeleteGrant $
--             newDeleteGrant
--
--         , requestDeleteLicense $
--             newDeleteLicense
--
--         , requestDeleteLicenseConfiguration $
--             newDeleteLicenseConfiguration
--
--         , requestDeleteLicenseManagerReportGenerator $
--             newDeleteLicenseManagerReportGenerator
--
--         , requestDeleteToken $
--             newDeleteToken
--
--         , requestExtendLicenseConsumption $
--             newExtendLicenseConsumption
--
--         , requestGetAccessToken $
--             newGetAccessToken
--
--         , requestGetGrant $
--             newGetGrant
--
--         , requestGetLicense $
--             newGetLicense
--
--         , requestGetLicenseConfiguration $
--             newGetLicenseConfiguration
--
--         , requestGetLicenseConversionTask $
--             newGetLicenseConversionTask
--
--         , requestGetLicenseManagerReportGenerator $
--             newGetLicenseManagerReportGenerator
--
--         , requestGetLicenseUsage $
--             newGetLicenseUsage
--
--         , requestGetServiceSettings $
--             newGetServiceSettings
--
--         , requestListAssociationsForLicenseConfiguration $
--             newListAssociationsForLicenseConfiguration
--
--         , requestListDistributedGrants $
--             newListDistributedGrants
--
--         , requestListFailuresForLicenseConfigurationOperations $
--             newListFailuresForLicenseConfigurationOperations
--
--         , requestListLicenseConfigurations $
--             newListLicenseConfigurations
--
--         , requestListLicenseConversionTasks $
--             newListLicenseConversionTasks
--
--         , requestListLicenseManagerReportGenerators $
--             newListLicenseManagerReportGenerators
--
--         , requestListLicenseSpecificationsForResource $
--             newListLicenseSpecificationsForResource
--
--         , requestListLicenseVersions $
--             newListLicenseVersions
--
--         , requestListLicenses $
--             newListLicenses
--
--         , requestListReceivedGrants $
--             newListReceivedGrants
--
--         , requestListReceivedGrantsForOrganization $
--             newListReceivedGrantsForOrganization
--
--         , requestListReceivedLicenses $
--             newListReceivedLicenses
--
--         , requestListReceivedLicensesForOrganization $
--             newListReceivedLicensesForOrganization
--
--         , requestListResourceInventory $
--             newListResourceInventory
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTokens $
--             newListTokens
--
--         , requestListUsageForLicenseConfiguration $
--             newListUsageForLicenseConfiguration
--
--         , requestRejectGrant $
--             newRejectGrant
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateLicenseConfiguration $
--             newUpdateLicenseConfiguration
--
--         , requestUpdateLicenseManagerReportGenerator $
--             newUpdateLicenseManagerReportGenerator
--
--         , requestUpdateLicenseSpecificationsForResource $
--             newUpdateLicenseSpecificationsForResource
--
--         , requestUpdateServiceSettings $
--             newUpdateServiceSettings
--
--           ]

--     , testGroup "response"
--         [ responseAcceptGrant $
--             newAcceptGrantResponse
--
--         , responseCheckInLicense $
--             newCheckInLicenseResponse
--
--         , responseCheckoutBorrowLicense $
--             newCheckoutBorrowLicenseResponse
--
--         , responseCheckoutLicense $
--             newCheckoutLicenseResponse
--
--         , responseCreateGrant $
--             newCreateGrantResponse
--
--         , responseCreateGrantVersion $
--             newCreateGrantVersionResponse
--
--         , responseCreateLicense $
--             newCreateLicenseResponse
--
--         , responseCreateLicenseConfiguration $
--             newCreateLicenseConfigurationResponse
--
--         , responseCreateLicenseConversionTaskForResource $
--             newCreateLicenseConversionTaskForResourceResponse
--
--         , responseCreateLicenseManagerReportGenerator $
--             newCreateLicenseManagerReportGeneratorResponse
--
--         , responseCreateLicenseVersion $
--             newCreateLicenseVersionResponse
--
--         , responseCreateToken $
--             newCreateTokenResponse
--
--         , responseDeleteGrant $
--             newDeleteGrantResponse
--
--         , responseDeleteLicense $
--             newDeleteLicenseResponse
--
--         , responseDeleteLicenseConfiguration $
--             newDeleteLicenseConfigurationResponse
--
--         , responseDeleteLicenseManagerReportGenerator $
--             newDeleteLicenseManagerReportGeneratorResponse
--
--         , responseDeleteToken $
--             newDeleteTokenResponse
--
--         , responseExtendLicenseConsumption $
--             newExtendLicenseConsumptionResponse
--
--         , responseGetAccessToken $
--             newGetAccessTokenResponse
--
--         , responseGetGrant $
--             newGetGrantResponse
--
--         , responseGetLicense $
--             newGetLicenseResponse
--
--         , responseGetLicenseConfiguration $
--             newGetLicenseConfigurationResponse
--
--         , responseGetLicenseConversionTask $
--             newGetLicenseConversionTaskResponse
--
--         , responseGetLicenseManagerReportGenerator $
--             newGetLicenseManagerReportGeneratorResponse
--
--         , responseGetLicenseUsage $
--             newGetLicenseUsageResponse
--
--         , responseGetServiceSettings $
--             newGetServiceSettingsResponse
--
--         , responseListAssociationsForLicenseConfiguration $
--             newListAssociationsForLicenseConfigurationResponse
--
--         , responseListDistributedGrants $
--             newListDistributedGrantsResponse
--
--         , responseListFailuresForLicenseConfigurationOperations $
--             newListFailuresForLicenseConfigurationOperationsResponse
--
--         , responseListLicenseConfigurations $
--             newListLicenseConfigurationsResponse
--
--         , responseListLicenseConversionTasks $
--             newListLicenseConversionTasksResponse
--
--         , responseListLicenseManagerReportGenerators $
--             newListLicenseManagerReportGeneratorsResponse
--
--         , responseListLicenseSpecificationsForResource $
--             newListLicenseSpecificationsForResourceResponse
--
--         , responseListLicenseVersions $
--             newListLicenseVersionsResponse
--
--         , responseListLicenses $
--             newListLicensesResponse
--
--         , responseListReceivedGrants $
--             newListReceivedGrantsResponse
--
--         , responseListReceivedGrantsForOrganization $
--             newListReceivedGrantsForOrganizationResponse
--
--         , responseListReceivedLicenses $
--             newListReceivedLicensesResponse
--
--         , responseListReceivedLicensesForOrganization $
--             newListReceivedLicensesForOrganizationResponse
--
--         , responseListResourceInventory $
--             newListResourceInventoryResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTokens $
--             newListTokensResponse
--
--         , responseListUsageForLicenseConfiguration $
--             newListUsageForLicenseConfigurationResponse
--
--         , responseRejectGrant $
--             newRejectGrantResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateLicenseConfiguration $
--             newUpdateLicenseConfigurationResponse
--
--         , responseUpdateLicenseManagerReportGenerator $
--             newUpdateLicenseManagerReportGeneratorResponse
--
--         , responseUpdateLicenseSpecificationsForResource $
--             newUpdateLicenseSpecificationsForResourceResponse
--
--         , responseUpdateServiceSettings $
--             newUpdateServiceSettingsResponse
--
--           ]
--     ]

-- Requests

requestAcceptGrant :: AcceptGrant -> TestTree
requestAcceptGrant =
  req
    "AcceptGrant"
    "fixture/AcceptGrant.yaml"

requestCheckInLicense :: CheckInLicense -> TestTree
requestCheckInLicense =
  req
    "CheckInLicense"
    "fixture/CheckInLicense.yaml"

requestCheckoutBorrowLicense :: CheckoutBorrowLicense -> TestTree
requestCheckoutBorrowLicense =
  req
    "CheckoutBorrowLicense"
    "fixture/CheckoutBorrowLicense.yaml"

requestCheckoutLicense :: CheckoutLicense -> TestTree
requestCheckoutLicense =
  req
    "CheckoutLicense"
    "fixture/CheckoutLicense.yaml"

requestCreateGrant :: CreateGrant -> TestTree
requestCreateGrant =
  req
    "CreateGrant"
    "fixture/CreateGrant.yaml"

requestCreateGrantVersion :: CreateGrantVersion -> TestTree
requestCreateGrantVersion =
  req
    "CreateGrantVersion"
    "fixture/CreateGrantVersion.yaml"

requestCreateLicense :: CreateLicense -> TestTree
requestCreateLicense =
  req
    "CreateLicense"
    "fixture/CreateLicense.yaml"

requestCreateLicenseConfiguration :: CreateLicenseConfiguration -> TestTree
requestCreateLicenseConfiguration =
  req
    "CreateLicenseConfiguration"
    "fixture/CreateLicenseConfiguration.yaml"

requestCreateLicenseConversionTaskForResource :: CreateLicenseConversionTaskForResource -> TestTree
requestCreateLicenseConversionTaskForResource =
  req
    "CreateLicenseConversionTaskForResource"
    "fixture/CreateLicenseConversionTaskForResource.yaml"

requestCreateLicenseManagerReportGenerator :: CreateLicenseManagerReportGenerator -> TestTree
requestCreateLicenseManagerReportGenerator =
  req
    "CreateLicenseManagerReportGenerator"
    "fixture/CreateLicenseManagerReportGenerator.yaml"

requestCreateLicenseVersion :: CreateLicenseVersion -> TestTree
requestCreateLicenseVersion =
  req
    "CreateLicenseVersion"
    "fixture/CreateLicenseVersion.yaml"

requestCreateToken :: CreateToken -> TestTree
requestCreateToken =
  req
    "CreateToken"
    "fixture/CreateToken.yaml"

requestDeleteGrant :: DeleteGrant -> TestTree
requestDeleteGrant =
  req
    "DeleteGrant"
    "fixture/DeleteGrant.yaml"

requestDeleteLicense :: DeleteLicense -> TestTree
requestDeleteLicense =
  req
    "DeleteLicense"
    "fixture/DeleteLicense.yaml"

requestDeleteLicenseConfiguration :: DeleteLicenseConfiguration -> TestTree
requestDeleteLicenseConfiguration =
  req
    "DeleteLicenseConfiguration"
    "fixture/DeleteLicenseConfiguration.yaml"

requestDeleteLicenseManagerReportGenerator :: DeleteLicenseManagerReportGenerator -> TestTree
requestDeleteLicenseManagerReportGenerator =
  req
    "DeleteLicenseManagerReportGenerator"
    "fixture/DeleteLicenseManagerReportGenerator.yaml"

requestDeleteToken :: DeleteToken -> TestTree
requestDeleteToken =
  req
    "DeleteToken"
    "fixture/DeleteToken.yaml"

requestExtendLicenseConsumption :: ExtendLicenseConsumption -> TestTree
requestExtendLicenseConsumption =
  req
    "ExtendLicenseConsumption"
    "fixture/ExtendLicenseConsumption.yaml"

requestGetAccessToken :: GetAccessToken -> TestTree
requestGetAccessToken =
  req
    "GetAccessToken"
    "fixture/GetAccessToken.yaml"

requestGetGrant :: GetGrant -> TestTree
requestGetGrant =
  req
    "GetGrant"
    "fixture/GetGrant.yaml"

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

requestGetLicenseConversionTask :: GetLicenseConversionTask -> TestTree
requestGetLicenseConversionTask =
  req
    "GetLicenseConversionTask"
    "fixture/GetLicenseConversionTask.yaml"

requestGetLicenseManagerReportGenerator :: GetLicenseManagerReportGenerator -> TestTree
requestGetLicenseManagerReportGenerator =
  req
    "GetLicenseManagerReportGenerator"
    "fixture/GetLicenseManagerReportGenerator.yaml"

requestGetLicenseUsage :: GetLicenseUsage -> TestTree
requestGetLicenseUsage =
  req
    "GetLicenseUsage"
    "fixture/GetLicenseUsage.yaml"

requestGetServiceSettings :: GetServiceSettings -> TestTree
requestGetServiceSettings =
  req
    "GetServiceSettings"
    "fixture/GetServiceSettings.yaml"

requestListAssociationsForLicenseConfiguration :: ListAssociationsForLicenseConfiguration -> TestTree
requestListAssociationsForLicenseConfiguration =
  req
    "ListAssociationsForLicenseConfiguration"
    "fixture/ListAssociationsForLicenseConfiguration.yaml"

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

requestListLicenseConfigurations :: ListLicenseConfigurations -> TestTree
requestListLicenseConfigurations =
  req
    "ListLicenseConfigurations"
    "fixture/ListLicenseConfigurations.yaml"

requestListLicenseConversionTasks :: ListLicenseConversionTasks -> TestTree
requestListLicenseConversionTasks =
  req
    "ListLicenseConversionTasks"
    "fixture/ListLicenseConversionTasks.yaml"

requestListLicenseManagerReportGenerators :: ListLicenseManagerReportGenerators -> TestTree
requestListLicenseManagerReportGenerators =
  req
    "ListLicenseManagerReportGenerators"
    "fixture/ListLicenseManagerReportGenerators.yaml"

requestListLicenseSpecificationsForResource :: ListLicenseSpecificationsForResource -> TestTree
requestListLicenseSpecificationsForResource =
  req
    "ListLicenseSpecificationsForResource"
    "fixture/ListLicenseSpecificationsForResource.yaml"

requestListLicenseVersions :: ListLicenseVersions -> TestTree
requestListLicenseVersions =
  req
    "ListLicenseVersions"
    "fixture/ListLicenseVersions.yaml"

requestListLicenses :: ListLicenses -> TestTree
requestListLicenses =
  req
    "ListLicenses"
    "fixture/ListLicenses.yaml"

requestListReceivedGrants :: ListReceivedGrants -> TestTree
requestListReceivedGrants =
  req
    "ListReceivedGrants"
    "fixture/ListReceivedGrants.yaml"

requestListReceivedGrantsForOrganization :: ListReceivedGrantsForOrganization -> TestTree
requestListReceivedGrantsForOrganization =
  req
    "ListReceivedGrantsForOrganization"
    "fixture/ListReceivedGrantsForOrganization.yaml"

requestListReceivedLicenses :: ListReceivedLicenses -> TestTree
requestListReceivedLicenses =
  req
    "ListReceivedLicenses"
    "fixture/ListReceivedLicenses.yaml"

requestListReceivedLicensesForOrganization :: ListReceivedLicensesForOrganization -> TestTree
requestListReceivedLicensesForOrganization =
  req
    "ListReceivedLicensesForOrganization"
    "fixture/ListReceivedLicensesForOrganization.yaml"

requestListResourceInventory :: ListResourceInventory -> TestTree
requestListResourceInventory =
  req
    "ListResourceInventory"
    "fixture/ListResourceInventory.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTokens :: ListTokens -> TestTree
requestListTokens =
  req
    "ListTokens"
    "fixture/ListTokens.yaml"

requestListUsageForLicenseConfiguration :: ListUsageForLicenseConfiguration -> TestTree
requestListUsageForLicenseConfiguration =
  req
    "ListUsageForLicenseConfiguration"
    "fixture/ListUsageForLicenseConfiguration.yaml"

requestRejectGrant :: RejectGrant -> TestTree
requestRejectGrant =
  req
    "RejectGrant"
    "fixture/RejectGrant.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateLicenseConfiguration :: UpdateLicenseConfiguration -> TestTree
requestUpdateLicenseConfiguration =
  req
    "UpdateLicenseConfiguration"
    "fixture/UpdateLicenseConfiguration.yaml"

requestUpdateLicenseManagerReportGenerator :: UpdateLicenseManagerReportGenerator -> TestTree
requestUpdateLicenseManagerReportGenerator =
  req
    "UpdateLicenseManagerReportGenerator"
    "fixture/UpdateLicenseManagerReportGenerator.yaml"

requestUpdateLicenseSpecificationsForResource :: UpdateLicenseSpecificationsForResource -> TestTree
requestUpdateLicenseSpecificationsForResource =
  req
    "UpdateLicenseSpecificationsForResource"
    "fixture/UpdateLicenseSpecificationsForResource.yaml"

requestUpdateServiceSettings :: UpdateServiceSettings -> TestTree
requestUpdateServiceSettings =
  req
    "UpdateServiceSettings"
    "fixture/UpdateServiceSettings.yaml"

-- Responses

responseAcceptGrant :: AcceptGrantResponse -> TestTree
responseAcceptGrant =
  res
    "AcceptGrantResponse"
    "fixture/AcceptGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptGrant)

responseCheckInLicense :: CheckInLicenseResponse -> TestTree
responseCheckInLicense =
  res
    "CheckInLicenseResponse"
    "fixture/CheckInLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckInLicense)

responseCheckoutBorrowLicense :: CheckoutBorrowLicenseResponse -> TestTree
responseCheckoutBorrowLicense =
  res
    "CheckoutBorrowLicenseResponse"
    "fixture/CheckoutBorrowLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckoutBorrowLicense)

responseCheckoutLicense :: CheckoutLicenseResponse -> TestTree
responseCheckoutLicense =
  res
    "CheckoutLicenseResponse"
    "fixture/CheckoutLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckoutLicense)

responseCreateGrant :: CreateGrantResponse -> TestTree
responseCreateGrant =
  res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrant)

responseCreateGrantVersion :: CreateGrantVersionResponse -> TestTree
responseCreateGrantVersion =
  res
    "CreateGrantVersionResponse"
    "fixture/CreateGrantVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrantVersion)

responseCreateLicense :: CreateLicenseResponse -> TestTree
responseCreateLicense =
  res
    "CreateLicenseResponse"
    "fixture/CreateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicense)

responseCreateLicenseConfiguration :: CreateLicenseConfigurationResponse -> TestTree
responseCreateLicenseConfiguration =
  res
    "CreateLicenseConfigurationResponse"
    "fixture/CreateLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseConfiguration)

responseCreateLicenseConversionTaskForResource :: CreateLicenseConversionTaskForResourceResponse -> TestTree
responseCreateLicenseConversionTaskForResource =
  res
    "CreateLicenseConversionTaskForResourceResponse"
    "fixture/CreateLicenseConversionTaskForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseConversionTaskForResource)

responseCreateLicenseManagerReportGenerator :: CreateLicenseManagerReportGeneratorResponse -> TestTree
responseCreateLicenseManagerReportGenerator =
  res
    "CreateLicenseManagerReportGeneratorResponse"
    "fixture/CreateLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseManagerReportGenerator)

responseCreateLicenseVersion :: CreateLicenseVersionResponse -> TestTree
responseCreateLicenseVersion =
  res
    "CreateLicenseVersionResponse"
    "fixture/CreateLicenseVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLicenseVersion)

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateToken)

responseDeleteGrant :: DeleteGrantResponse -> TestTree
responseDeleteGrant =
  res
    "DeleteGrantResponse"
    "fixture/DeleteGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGrant)

responseDeleteLicense :: DeleteLicenseResponse -> TestTree
responseDeleteLicense =
  res
    "DeleteLicenseResponse"
    "fixture/DeleteLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicense)

responseDeleteLicenseConfiguration :: DeleteLicenseConfigurationResponse -> TestTree
responseDeleteLicenseConfiguration =
  res
    "DeleteLicenseConfigurationResponse"
    "fixture/DeleteLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicenseConfiguration)

responseDeleteLicenseManagerReportGenerator :: DeleteLicenseManagerReportGeneratorResponse -> TestTree
responseDeleteLicenseManagerReportGenerator =
  res
    "DeleteLicenseManagerReportGeneratorResponse"
    "fixture/DeleteLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLicenseManagerReportGenerator)

responseDeleteToken :: DeleteTokenResponse -> TestTree
responseDeleteToken =
  res
    "DeleteTokenResponse"
    "fixture/DeleteTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteToken)

responseExtendLicenseConsumption :: ExtendLicenseConsumptionResponse -> TestTree
responseExtendLicenseConsumption =
  res
    "ExtendLicenseConsumptionResponse"
    "fixture/ExtendLicenseConsumptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExtendLicenseConsumption)

responseGetAccessToken :: GetAccessTokenResponse -> TestTree
responseGetAccessToken =
  res
    "GetAccessTokenResponse"
    "fixture/GetAccessTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessToken)

responseGetGrant :: GetGrantResponse -> TestTree
responseGetGrant =
  res
    "GetGrantResponse"
    "fixture/GetGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGrant)

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

responseGetLicenseConversionTask :: GetLicenseConversionTaskResponse -> TestTree
responseGetLicenseConversionTask =
  res
    "GetLicenseConversionTaskResponse"
    "fixture/GetLicenseConversionTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseConversionTask)

responseGetLicenseManagerReportGenerator :: GetLicenseManagerReportGeneratorResponse -> TestTree
responseGetLicenseManagerReportGenerator =
  res
    "GetLicenseManagerReportGeneratorResponse"
    "fixture/GetLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseManagerReportGenerator)

responseGetLicenseUsage :: GetLicenseUsageResponse -> TestTree
responseGetLicenseUsage =
  res
    "GetLicenseUsageResponse"
    "fixture/GetLicenseUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLicenseUsage)

responseGetServiceSettings :: GetServiceSettingsResponse -> TestTree
responseGetServiceSettings =
  res
    "GetServiceSettingsResponse"
    "fixture/GetServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceSettings)

responseListAssociationsForLicenseConfiguration :: ListAssociationsForLicenseConfigurationResponse -> TestTree
responseListAssociationsForLicenseConfiguration =
  res
    "ListAssociationsForLicenseConfigurationResponse"
    "fixture/ListAssociationsForLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociationsForLicenseConfiguration)

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

responseListLicenseConfigurations :: ListLicenseConfigurationsResponse -> TestTree
responseListLicenseConfigurations =
  res
    "ListLicenseConfigurationsResponse"
    "fixture/ListLicenseConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseConfigurations)

responseListLicenseConversionTasks :: ListLicenseConversionTasksResponse -> TestTree
responseListLicenseConversionTasks =
  res
    "ListLicenseConversionTasksResponse"
    "fixture/ListLicenseConversionTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseConversionTasks)

responseListLicenseManagerReportGenerators :: ListLicenseManagerReportGeneratorsResponse -> TestTree
responseListLicenseManagerReportGenerators =
  res
    "ListLicenseManagerReportGeneratorsResponse"
    "fixture/ListLicenseManagerReportGeneratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseManagerReportGenerators)

responseListLicenseSpecificationsForResource :: ListLicenseSpecificationsForResourceResponse -> TestTree
responseListLicenseSpecificationsForResource =
  res
    "ListLicenseSpecificationsForResourceResponse"
    "fixture/ListLicenseSpecificationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseSpecificationsForResource)

responseListLicenseVersions :: ListLicenseVersionsResponse -> TestTree
responseListLicenseVersions =
  res
    "ListLicenseVersionsResponse"
    "fixture/ListLicenseVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenseVersions)

responseListLicenses :: ListLicensesResponse -> TestTree
responseListLicenses =
  res
    "ListLicensesResponse"
    "fixture/ListLicensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLicenses)

responseListReceivedGrants :: ListReceivedGrantsResponse -> TestTree
responseListReceivedGrants =
  res
    "ListReceivedGrantsResponse"
    "fixture/ListReceivedGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedGrants)

responseListReceivedGrantsForOrganization :: ListReceivedGrantsForOrganizationResponse -> TestTree
responseListReceivedGrantsForOrganization =
  res
    "ListReceivedGrantsForOrganizationResponse"
    "fixture/ListReceivedGrantsForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedGrantsForOrganization)

responseListReceivedLicenses :: ListReceivedLicensesResponse -> TestTree
responseListReceivedLicenses =
  res
    "ListReceivedLicensesResponse"
    "fixture/ListReceivedLicensesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedLicenses)

responseListReceivedLicensesForOrganization :: ListReceivedLicensesForOrganizationResponse -> TestTree
responseListReceivedLicensesForOrganization =
  res
    "ListReceivedLicensesForOrganizationResponse"
    "fixture/ListReceivedLicensesForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReceivedLicensesForOrganization)

responseListResourceInventory :: ListResourceInventoryResponse -> TestTree
responseListResourceInventory =
  res
    "ListResourceInventoryResponse"
    "fixture/ListResourceInventoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceInventory)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTokens :: ListTokensResponse -> TestTree
responseListTokens =
  res
    "ListTokensResponse"
    "fixture/ListTokensResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTokens)

responseListUsageForLicenseConfiguration :: ListUsageForLicenseConfigurationResponse -> TestTree
responseListUsageForLicenseConfiguration =
  res
    "ListUsageForLicenseConfigurationResponse"
    "fixture/ListUsageForLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsageForLicenseConfiguration)

responseRejectGrant :: RejectGrantResponse -> TestTree
responseRejectGrant =
  res
    "RejectGrantResponse"
    "fixture/RejectGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectGrant)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateLicenseConfiguration :: UpdateLicenseConfigurationResponse -> TestTree
responseUpdateLicenseConfiguration =
  res
    "UpdateLicenseConfigurationResponse"
    "fixture/UpdateLicenseConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseConfiguration)

responseUpdateLicenseManagerReportGenerator :: UpdateLicenseManagerReportGeneratorResponse -> TestTree
responseUpdateLicenseManagerReportGenerator =
  res
    "UpdateLicenseManagerReportGeneratorResponse"
    "fixture/UpdateLicenseManagerReportGeneratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseManagerReportGenerator)

responseUpdateLicenseSpecificationsForResource :: UpdateLicenseSpecificationsForResourceResponse -> TestTree
responseUpdateLicenseSpecificationsForResource =
  res
    "UpdateLicenseSpecificationsForResourceResponse"
    "fixture/UpdateLicenseSpecificationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLicenseSpecificationsForResource)

responseUpdateServiceSettings :: UpdateServiceSettingsResponse -> TestTree
responseUpdateServiceSettings =
  res
    "UpdateServiceSettingsResponse"
    "fixture/UpdateServiceSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServiceSettings)
