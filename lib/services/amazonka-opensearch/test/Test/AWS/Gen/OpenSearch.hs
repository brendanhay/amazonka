{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.OpenSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.OpenSearch where

import qualified Data.Proxy as Proxy
import Network.AWS.OpenSearch
import Test.AWS.Fixture
import Test.AWS.OpenSearch.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRejectInboundConnection $
--             newRejectInboundConnection
--
--         , requestDescribeOutboundConnections $
--             newDescribeOutboundConnections
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestDescribeInstanceTypeLimits $
--             newDescribeInstanceTypeLimits
--
--         , requestDescribeInboundConnections $
--             newDescribeInboundConnections
--
--         , requestCancelServiceSoftwareUpdate $
--             newCancelServiceSoftwareUpdate
--
--         , requestListDomainsForPackage $
--             newListDomainsForPackage
--
--         , requestListPackagesForDomain $
--             newListPackagesForDomain
--
--         , requestUpgradeDomain $
--             newUpgradeDomain
--
--         , requestDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunes
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestStartServiceSoftwareUpdate $
--             newStartServiceSoftwareUpdate
--
--         , requestDeleteOutboundConnection $
--             newDeleteOutboundConnection
--
--         , requestListVersions $
--             newListVersions
--
--         , requestDescribeReservedInstanceOfferings $
--             newDescribeReservedInstanceOfferings
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestPurchaseReservedInstanceOffering $
--             newPurchaseReservedInstanceOffering
--
--         , requestDescribeDomains $
--             newDescribeDomains
--
--         , requestAssociatePackage $
--             newAssociatePackage
--
--         , requestListInstanceTypeDetails $
--             newListInstanceTypeDetails
--
--         , requestGetPackageVersionHistory $
--             newGetPackageVersionHistory
--
--         , requestGetUpgradeHistory $
--             newGetUpgradeHistory
--
--         , requestDescribePackages $
--             newDescribePackages
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDescribeDomainConfig $
--             newDescribeDomainConfig
--
--         , requestGetUpgradeStatus $
--             newGetUpgradeStatus
--
--         , requestDeleteInboundConnection $
--             newDeleteInboundConnection
--
--         , requestDissociatePackage $
--             newDissociatePackage
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestAddTags $
--             newAddTags
--
--         , requestAcceptInboundConnection $
--             newAcceptInboundConnection
--
--         , requestUpdateDomainConfig $
--             newUpdateDomainConfig
--
--         , requestListTags $
--             newListTags
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestUpdatePackage $
--             newUpdatePackage
--
--         , requestCreateOutboundConnection $
--             newCreateOutboundConnection
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestGetCompatibleVersions $
--             newGetCompatibleVersions
--
--           ]

--     , testGroup "response"
--         [ responseRejectInboundConnection $
--             newRejectInboundConnectionResponse
--
--         , responseDescribeOutboundConnections $
--             newDescribeOutboundConnectionsResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseDescribeInstanceTypeLimits $
--             newDescribeInstanceTypeLimitsResponse
--
--         , responseDescribeInboundConnections $
--             newDescribeInboundConnectionsResponse
--
--         , responseCancelServiceSoftwareUpdate $
--             newCancelServiceSoftwareUpdateResponse
--
--         , responseListDomainsForPackage $
--             newListDomainsForPackageResponse
--
--         , responseListPackagesForDomain $
--             newListPackagesForDomainResponse
--
--         , responseUpgradeDomain $
--             newUpgradeDomainResponse
--
--         , responseDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunesResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseStartServiceSoftwareUpdate $
--             newStartServiceSoftwareUpdateResponse
--
--         , responseDeleteOutboundConnection $
--             newDeleteOutboundConnectionResponse
--
--         , responseListVersions $
--             newListVersionsResponse
--
--         , responseDescribeReservedInstanceOfferings $
--             newDescribeReservedInstanceOfferingsResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responsePurchaseReservedInstanceOffering $
--             newPurchaseReservedInstanceOfferingResponse
--
--         , responseDescribeDomains $
--             newDescribeDomainsResponse
--
--         , responseAssociatePackage $
--             newAssociatePackageResponse
--
--         , responseListInstanceTypeDetails $
--             newListInstanceTypeDetailsResponse
--
--         , responseGetPackageVersionHistory $
--             newGetPackageVersionHistoryResponse
--
--         , responseGetUpgradeHistory $
--             newGetUpgradeHistoryResponse
--
--         , responseDescribePackages $
--             newDescribePackagesResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDescribeDomainConfig $
--             newDescribeDomainConfigResponse
--
--         , responseGetUpgradeStatus $
--             newGetUpgradeStatusResponse
--
--         , responseDeleteInboundConnection $
--             newDeleteInboundConnectionResponse
--
--         , responseDissociatePackage $
--             newDissociatePackageResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseAcceptInboundConnection $
--             newAcceptInboundConnectionResponse
--
--         , responseUpdateDomainConfig $
--             newUpdateDomainConfigResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseUpdatePackage $
--             newUpdatePackageResponse
--
--         , responseCreateOutboundConnection $
--             newCreateOutboundConnectionResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseGetCompatibleVersions $
--             newGetCompatibleVersionsResponse
--
--           ]
--     ]

-- Requests

requestRejectInboundConnection :: RejectInboundConnection -> TestTree
requestRejectInboundConnection =
  req
    "RejectInboundConnection"
    "fixture/RejectInboundConnection.yaml"

requestDescribeOutboundConnections :: DescribeOutboundConnections -> TestTree
requestDescribeOutboundConnections =
  req
    "DescribeOutboundConnections"
    "fixture/DescribeOutboundConnections.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeInstanceTypeLimits :: DescribeInstanceTypeLimits -> TestTree
requestDescribeInstanceTypeLimits =
  req
    "DescribeInstanceTypeLimits"
    "fixture/DescribeInstanceTypeLimits.yaml"

requestDescribeInboundConnections :: DescribeInboundConnections -> TestTree
requestDescribeInboundConnections =
  req
    "DescribeInboundConnections"
    "fixture/DescribeInboundConnections.yaml"

requestCancelServiceSoftwareUpdate :: CancelServiceSoftwareUpdate -> TestTree
requestCancelServiceSoftwareUpdate =
  req
    "CancelServiceSoftwareUpdate"
    "fixture/CancelServiceSoftwareUpdate.yaml"

requestListDomainsForPackage :: ListDomainsForPackage -> TestTree
requestListDomainsForPackage =
  req
    "ListDomainsForPackage"
    "fixture/ListDomainsForPackage.yaml"

requestListPackagesForDomain :: ListPackagesForDomain -> TestTree
requestListPackagesForDomain =
  req
    "ListPackagesForDomain"
    "fixture/ListPackagesForDomain.yaml"

requestUpgradeDomain :: UpgradeDomain -> TestTree
requestUpgradeDomain =
  req
    "UpgradeDomain"
    "fixture/UpgradeDomain.yaml"

requestDescribeDomainAutoTunes :: DescribeDomainAutoTunes -> TestTree
requestDescribeDomainAutoTunes =
  req
    "DescribeDomainAutoTunes"
    "fixture/DescribeDomainAutoTunes.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances =
  req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

requestStartServiceSoftwareUpdate :: StartServiceSoftwareUpdate -> TestTree
requestStartServiceSoftwareUpdate =
  req
    "StartServiceSoftwareUpdate"
    "fixture/StartServiceSoftwareUpdate.yaml"

requestDeleteOutboundConnection :: DeleteOutboundConnection -> TestTree
requestDeleteOutboundConnection =
  req
    "DeleteOutboundConnection"
    "fixture/DeleteOutboundConnection.yaml"

requestListVersions :: ListVersions -> TestTree
requestListVersions =
  req
    "ListVersions"
    "fixture/ListVersions.yaml"

requestDescribeReservedInstanceOfferings :: DescribeReservedInstanceOfferings -> TestTree
requestDescribeReservedInstanceOfferings =
  req
    "DescribeReservedInstanceOfferings"
    "fixture/DescribeReservedInstanceOfferings.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestPurchaseReservedInstanceOffering :: PurchaseReservedInstanceOffering -> TestTree
requestPurchaseReservedInstanceOffering =
  req
    "PurchaseReservedInstanceOffering"
    "fixture/PurchaseReservedInstanceOffering.yaml"

requestDescribeDomains :: DescribeDomains -> TestTree
requestDescribeDomains =
  req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

requestAssociatePackage :: AssociatePackage -> TestTree
requestAssociatePackage =
  req
    "AssociatePackage"
    "fixture/AssociatePackage.yaml"

requestListInstanceTypeDetails :: ListInstanceTypeDetails -> TestTree
requestListInstanceTypeDetails =
  req
    "ListInstanceTypeDetails"
    "fixture/ListInstanceTypeDetails.yaml"

requestGetPackageVersionHistory :: GetPackageVersionHistory -> TestTree
requestGetPackageVersionHistory =
  req
    "GetPackageVersionHistory"
    "fixture/GetPackageVersionHistory.yaml"

requestGetUpgradeHistory :: GetUpgradeHistory -> TestTree
requestGetUpgradeHistory =
  req
    "GetUpgradeHistory"
    "fixture/GetUpgradeHistory.yaml"

requestDescribePackages :: DescribePackages -> TestTree
requestDescribePackages =
  req
    "DescribePackages"
    "fixture/DescribePackages.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDescribeDomainConfig :: DescribeDomainConfig -> TestTree
requestDescribeDomainConfig =
  req
    "DescribeDomainConfig"
    "fixture/DescribeDomainConfig.yaml"

requestGetUpgradeStatus :: GetUpgradeStatus -> TestTree
requestGetUpgradeStatus =
  req
    "GetUpgradeStatus"
    "fixture/GetUpgradeStatus.yaml"

requestDeleteInboundConnection :: DeleteInboundConnection -> TestTree
requestDeleteInboundConnection =
  req
    "DeleteInboundConnection"
    "fixture/DeleteInboundConnection.yaml"

requestDissociatePackage :: DissociatePackage -> TestTree
requestDissociatePackage =
  req
    "DissociatePackage"
    "fixture/DissociatePackage.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestAcceptInboundConnection :: AcceptInboundConnection -> TestTree
requestAcceptInboundConnection =
  req
    "AcceptInboundConnection"
    "fixture/AcceptInboundConnection.yaml"

requestUpdateDomainConfig :: UpdateDomainConfig -> TestTree
requestUpdateDomainConfig =
  req
    "UpdateDomainConfig"
    "fixture/UpdateDomainConfig.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestUpdatePackage :: UpdatePackage -> TestTree
requestUpdatePackage =
  req
    "UpdatePackage"
    "fixture/UpdatePackage.yaml"

requestCreateOutboundConnection :: CreateOutboundConnection -> TestTree
requestCreateOutboundConnection =
  req
    "CreateOutboundConnection"
    "fixture/CreateOutboundConnection.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage =
  req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestGetCompatibleVersions :: GetCompatibleVersions -> TestTree
requestGetCompatibleVersions =
  req
    "GetCompatibleVersions"
    "fixture/GetCompatibleVersions.yaml"

-- Responses

responseRejectInboundConnection :: RejectInboundConnectionResponse -> TestTree
responseRejectInboundConnection =
  res
    "RejectInboundConnectionResponse"
    "fixture/RejectInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInboundConnection)

responseDescribeOutboundConnections :: DescribeOutboundConnectionsResponse -> TestTree
responseDescribeOutboundConnections =
  res
    "DescribeOutboundConnectionsResponse"
    "fixture/DescribeOutboundConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOutboundConnections)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseDescribeInstanceTypeLimits :: DescribeInstanceTypeLimitsResponse -> TestTree
responseDescribeInstanceTypeLimits =
  res
    "DescribeInstanceTypeLimitsResponse"
    "fixture/DescribeInstanceTypeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypeLimits)

responseDescribeInboundConnections :: DescribeInboundConnectionsResponse -> TestTree
responseDescribeInboundConnections =
  res
    "DescribeInboundConnectionsResponse"
    "fixture/DescribeInboundConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundConnections)

responseCancelServiceSoftwareUpdate :: CancelServiceSoftwareUpdateResponse -> TestTree
responseCancelServiceSoftwareUpdate =
  res
    "CancelServiceSoftwareUpdateResponse"
    "fixture/CancelServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServiceSoftwareUpdate)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage =
  res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainsForPackage)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain =
  res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackagesForDomain)

responseUpgradeDomain :: UpgradeDomainResponse -> TestTree
responseUpgradeDomain =
  res
    "UpgradeDomainResponse"
    "fixture/UpgradeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeDomain)

responseDescribeDomainAutoTunes :: DescribeDomainAutoTunesResponse -> TestTree
responseDescribeDomainAutoTunes =
  res
    "DescribeDomainAutoTunesResponse"
    "fixture/DescribeDomainAutoTunesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainAutoTunes)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstances)

responseStartServiceSoftwareUpdate :: StartServiceSoftwareUpdateResponse -> TestTree
responseStartServiceSoftwareUpdate =
  res
    "StartServiceSoftwareUpdateResponse"
    "fixture/StartServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartServiceSoftwareUpdate)

responseDeleteOutboundConnection :: DeleteOutboundConnectionResponse -> TestTree
responseDeleteOutboundConnection =
  res
    "DeleteOutboundConnectionResponse"
    "fixture/DeleteOutboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutboundConnection)

responseListVersions :: ListVersionsResponse -> TestTree
responseListVersions =
  res
    "ListVersionsResponse"
    "fixture/ListVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVersions)

responseDescribeReservedInstanceOfferings :: DescribeReservedInstanceOfferingsResponse -> TestTree
responseDescribeReservedInstanceOfferings =
  res
    "DescribeReservedInstanceOfferingsResponse"
    "fixture/DescribeReservedInstanceOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstanceOfferings)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responsePurchaseReservedInstanceOffering :: PurchaseReservedInstanceOfferingResponse -> TestTree
responsePurchaseReservedInstanceOffering =
  res
    "PurchaseReservedInstanceOfferingResponse"
    "fixture/PurchaseReservedInstanceOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedInstanceOffering)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains =
  res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomains)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePackage)

responseListInstanceTypeDetails :: ListInstanceTypeDetailsResponse -> TestTree
responseListInstanceTypeDetails =
  res
    "ListInstanceTypeDetailsResponse"
    "fixture/ListInstanceTypeDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceTypeDetails)

responseGetPackageVersionHistory :: GetPackageVersionHistoryResponse -> TestTree
responseGetPackageVersionHistory =
  res
    "GetPackageVersionHistoryResponse"
    "fixture/GetPackageVersionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPackageVersionHistory)

responseGetUpgradeHistory :: GetUpgradeHistoryResponse -> TestTree
responseGetUpgradeHistory =
  res
    "GetUpgradeHistoryResponse"
    "fixture/GetUpgradeHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpgradeHistory)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackages)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseDescribeDomainConfig :: DescribeDomainConfigResponse -> TestTree
responseDescribeDomainConfig =
  res
    "DescribeDomainConfigResponse"
    "fixture/DescribeDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainConfig)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpgradeStatus)

responseDeleteInboundConnection :: DeleteInboundConnectionResponse -> TestTree
responseDeleteInboundConnection =
  res
    "DeleteInboundConnectionResponse"
    "fixture/DeleteInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInboundConnection)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DissociatePackage)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseAcceptInboundConnection :: AcceptInboundConnectionResponse -> TestTree
responseAcceptInboundConnection =
  res
    "AcceptInboundConnectionResponse"
    "fixture/AcceptInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInboundConnection)

responseUpdateDomainConfig :: UpdateDomainConfigResponse -> TestTree
responseUpdateDomainConfig =
  res
    "UpdateDomainConfigResponse"
    "fixture/UpdateDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainConfig)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackage)

responseUpdatePackage :: UpdatePackageResponse -> TestTree
responseUpdatePackage =
  res
    "UpdatePackageResponse"
    "fixture/UpdatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePackage)

responseCreateOutboundConnection :: CreateOutboundConnectionResponse -> TestTree
responseCreateOutboundConnection =
  res
    "CreateOutboundConnectionResponse"
    "fixture/CreateOutboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOutboundConnection)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackage)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseGetCompatibleVersions :: GetCompatibleVersionsResponse -> TestTree
responseGetCompatibleVersions =
  res
    "GetCompatibleVersionsResponse"
    "fixture/GetCompatibleVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleVersions)
