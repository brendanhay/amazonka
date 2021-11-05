{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticSearch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ElasticSearch where

import Amazonka.ElasticSearch
import qualified Data.Proxy as Proxy
import Test.AWS.ElasticSearch.Internal
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
--         [ requestCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnection
--
--         , requestDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnections
--
--         , requestCreateElasticsearchDomain $
--             newCreateElasticsearchDomain
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersions
--
--         , requestDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomains
--
--         , requestListDomainsForPackage $
--             newListDomainsForPackage
--
--         , requestListPackagesForDomain $
--             newListPackagesForDomain
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdate
--
--         , requestDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunes
--
--         , requestListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypes
--
--         , requestDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRole
--
--         , requestDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomain
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestAssociatePackage $
--             newAssociatePackage
--
--         , requestDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnection
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimits
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
--         , requestDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfig
--
--         , requestGetUpgradeStatus $
--             newGetUpgradeStatus
--
--         , requestDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomain
--
--         , requestDissociatePackage $
--             newDissociatePackage
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOffering
--
--         , requestDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstances
--
--         , requestUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             newListElasticsearchVersions
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnection
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferings
--
--         , requestUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomain
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
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdate
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnection
--
--         , requestDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnections
--
--         , requestAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnection
--
--           ]

--     , testGroup "response"
--         [ responseCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnectionsResponse
--
--         , responseCreateElasticsearchDomain $
--             newCreateElasticsearchDomainResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersionsResponse
--
--         , responseDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomainsResponse
--
--         , responseListDomainsForPackage $
--             newListDomainsForPackageResponse
--
--         , responseListPackagesForDomain $
--             newListPackagesForDomainResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdateResponse
--
--         , responseDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunesResponse
--
--         , responseListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypesResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRoleResponse
--
--         , responseDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomainResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseAssociatePackage $
--             newAssociatePackageResponse
--
--         , responseDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimitsResponse
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
--         , responseDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfigResponse
--
--         , responseGetUpgradeStatus $
--             newGetUpgradeStatusResponse
--
--         , responseDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomainResponse
--
--         , responseDissociatePackage $
--             newDissociatePackageResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstancesResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             newListElasticsearchVersionsResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomainResponse
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
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnectionsResponse
--
--         , responseAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnectionResponse
--
--           ]
--     ]

-- Requests

requestCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnection -> TestTree
requestCreateOutboundCrossClusterSearchConnection =
  req
    "CreateOutboundCrossClusterSearchConnection"
    "fixture/CreateOutboundCrossClusterSearchConnection.yaml"

requestDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnections -> TestTree
requestDescribeInboundCrossClusterSearchConnections =
  req
    "DescribeInboundCrossClusterSearchConnections"
    "fixture/DescribeInboundCrossClusterSearchConnections.yaml"

requestCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
requestCreateElasticsearchDomain =
  req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersions -> TestTree
requestGetCompatibleElasticsearchVersions =
  req
    "GetCompatibleElasticsearchVersions"
    "fixture/GetCompatibleElasticsearchVersions.yaml"

requestDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
requestDescribeElasticsearchDomains =
  req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

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

requestStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdate -> TestTree
requestStartElasticsearchServiceSoftwareUpdate =
  req
    "StartElasticsearchServiceSoftwareUpdate"
    "fixture/StartElasticsearchServiceSoftwareUpdate.yaml"

requestDescribeDomainAutoTunes :: DescribeDomainAutoTunes -> TestTree
requestDescribeDomainAutoTunes =
  req
    "DescribeDomainAutoTunes"
    "fixture/DescribeDomainAutoTunes.yaml"

requestListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypes -> TestTree
requestListElasticsearchInstanceTypes =
  req
    "ListElasticsearchInstanceTypes"
    "fixture/ListElasticsearchInstanceTypes.yaml"

requestDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRole -> TestTree
requestDeleteElasticsearchServiceRole =
  req
    "DeleteElasticsearchServiceRole"
    "fixture/DeleteElasticsearchServiceRole.yaml"

requestDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
requestDescribeElasticsearchDomain =
  req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestAssociatePackage :: AssociatePackage -> TestTree
requestAssociatePackage =
  req
    "AssociatePackage"
    "fixture/AssociatePackage.yaml"

requestDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnection -> TestTree
requestDeleteOutboundCrossClusterSearchConnection =
  req
    "DeleteOutboundCrossClusterSearchConnection"
    "fixture/DeleteOutboundCrossClusterSearchConnection.yaml"

requestDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimits -> TestTree
requestDescribeElasticsearchInstanceTypeLimits =
  req
    "DescribeElasticsearchInstanceTypeLimits"
    "fixture/DescribeElasticsearchInstanceTypeLimits.yaml"

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

requestDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
requestDescribeElasticsearchDomainConfig =
  req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

requestGetUpgradeStatus :: GetUpgradeStatus -> TestTree
requestGetUpgradeStatus =
  req
    "GetUpgradeStatus"
    "fixture/GetUpgradeStatus.yaml"

requestDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
requestDeleteElasticsearchDomain =
  req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

requestDissociatePackage :: DissociatePackage -> TestTree
requestDissociatePackage =
  req
    "DissociatePackage"
    "fixture/DissociatePackage.yaml"

requestPurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOffering -> TestTree
requestPurchaseReservedElasticsearchInstanceOffering =
  req
    "PurchaseReservedElasticsearchInstanceOffering"
    "fixture/PurchaseReservedElasticsearchInstanceOffering.yaml"

requestDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstances -> TestTree
requestDescribeReservedElasticsearchInstances =
  req
    "DescribeReservedElasticsearchInstances"
    "fixture/DescribeReservedElasticsearchInstances.yaml"

requestUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfig -> TestTree
requestUpdateElasticsearchDomainConfig =
  req
    "UpdateElasticsearchDomainConfig"
    "fixture/UpdateElasticsearchDomainConfig.yaml"

requestListElasticsearchVersions :: ListElasticsearchVersions -> TestTree
requestListElasticsearchVersions =
  req
    "ListElasticsearchVersions"
    "fixture/ListElasticsearchVersions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnection -> TestTree
requestDeleteInboundCrossClusterSearchConnection =
  req
    "DeleteInboundCrossClusterSearchConnection"
    "fixture/DeleteInboundCrossClusterSearchConnection.yaml"

requestDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferings -> TestTree
requestDescribeReservedElasticsearchInstanceOfferings =
  req
    "DescribeReservedElasticsearchInstanceOfferings"
    "fixture/DescribeReservedElasticsearchInstanceOfferings.yaml"

requestUpgradeElasticsearchDomain :: UpgradeElasticsearchDomain -> TestTree
requestUpgradeElasticsearchDomain =
  req
    "UpgradeElasticsearchDomain"
    "fixture/UpgradeElasticsearchDomain.yaml"

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

requestCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdate -> TestTree
requestCancelElasticsearchServiceSoftwareUpdate =
  req
    "CancelElasticsearchServiceSoftwareUpdate"
    "fixture/CancelElasticsearchServiceSoftwareUpdate.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage =
  req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

requestRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnection -> TestTree
requestRejectInboundCrossClusterSearchConnection =
  req
    "RejectInboundCrossClusterSearchConnection"
    "fixture/RejectInboundCrossClusterSearchConnection.yaml"

requestDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnections -> TestTree
requestDescribeOutboundCrossClusterSearchConnections =
  req
    "DescribeOutboundCrossClusterSearchConnections"
    "fixture/DescribeOutboundCrossClusterSearchConnections.yaml"

requestAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnection -> TestTree
requestAcceptInboundCrossClusterSearchConnection =
  req
    "AcceptInboundCrossClusterSearchConnection"
    "fixture/AcceptInboundCrossClusterSearchConnection.yaml"

-- Responses

responseCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnectionResponse -> TestTree
responseCreateOutboundCrossClusterSearchConnection =
  res
    "CreateOutboundCrossClusterSearchConnectionResponse"
    "fixture/CreateOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOutboundCrossClusterSearchConnection)

responseDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeInboundCrossClusterSearchConnections =
  res
    "DescribeInboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeInboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundCrossClusterSearchConnections)

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain =
  res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateElasticsearchDomain)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions =
  res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleElasticsearchVersions)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains =
  res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomains)

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

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate =
  res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartElasticsearchServiceSoftwareUpdate)

responseDescribeDomainAutoTunes :: DescribeDomainAutoTunesResponse -> TestTree
responseDescribeDomainAutoTunes =
  res
    "DescribeDomainAutoTunesResponse"
    "fixture/DescribeDomainAutoTunesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainAutoTunes)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes =
  res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListElasticsearchInstanceTypes)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole =
  res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteElasticsearchServiceRole)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain =
  res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomain)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePackage)

responseDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteOutboundCrossClusterSearchConnection =
  res
    "DeleteOutboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutboundCrossClusterSearchConnection)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits =
  res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchInstanceTypeLimits)

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

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig =
  res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomainConfig)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpgradeStatus)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain =
  res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteElasticsearchDomain)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DissociatePackage)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering =
  res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedElasticsearchInstanceOffering)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances =
  res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedElasticsearchInstances)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig =
  res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions =
  res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListElasticsearchVersions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteInboundCrossClusterSearchConnection =
  res
    "DeleteInboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInboundCrossClusterSearchConnection)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings =
  res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedElasticsearchInstanceOfferings)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain =
  res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeElasticsearchDomain)

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

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate =
  res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelElasticsearchServiceSoftwareUpdate)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackage)

responseRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> TestTree
responseRejectInboundCrossClusterSearchConnection =
  res
    "RejectInboundCrossClusterSearchConnectionResponse"
    "fixture/RejectInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInboundCrossClusterSearchConnection)

responseDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeOutboundCrossClusterSearchConnections =
  res
    "DescribeOutboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeOutboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOutboundCrossClusterSearchConnections)

responseAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> TestTree
responseAcceptInboundCrossClusterSearchConnection =
  res
    "AcceptInboundCrossClusterSearchConnectionResponse"
    "fixture/AcceptInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInboundCrossClusterSearchConnection)
