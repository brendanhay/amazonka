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

import Data.Proxy
import Network.AWS.ElasticSearch
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
--         [ requestDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnections
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnection
--
--         , requestGetUpgradeHistory $
--             newGetUpgradeHistory
--
--         , requestDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfig
--
--         , requestAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnection
--
--         , requestDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnection
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdate
--
--         , requestDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomain
--
--         , requestDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRole
--
--         , requestListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypes
--
--         , requestUpdatePackage $
--             newUpdatePackage
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnection
--
--         , requestUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             newListElasticsearchVersions
--
--         , requestDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomain
--
--         , requestGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersions
--
--         , requestDissociatePackage $
--             newDissociatePackage
--
--         , requestCreateElasticsearchDomain $
--             newCreateElasticsearchDomain
--
--         , requestDescribePackages $
--             newDescribePackages
--
--         , requestGetPackageVersionHistory $
--             newGetPackageVersionHistory
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimits
--
--         , requestDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnections
--
--         , requestAssociatePackage $
--             newAssociatePackage
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnection
--
--         , requestDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunes
--
--         , requestListTags $
--             newListTags
--
--         , requestUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomain
--
--         , requestListPackagesForDomain $
--             newListPackagesForDomain
--
--         , requestDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstances
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferings
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdate
--
--         , requestListDomainsForPackage $
--             newListDomainsForPackage
--
--         , requestDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomains
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOffering
--
--         , requestGetUpgradeStatus $
--             newGetUpgradeStatus
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnectionsResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnectionResponse
--
--         , responseGetUpgradeHistory $
--             newGetUpgradeHistoryResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfigResponse
--
--         , responseAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnectionResponse
--
--         , responseDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnectionResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomainResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRoleResponse
--
--         , responseListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypesResponse
--
--         , responseUpdatePackage $
--             newUpdatePackageResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnectionResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             newListElasticsearchVersionsResponse
--
--         , responseDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomainResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersionsResponse
--
--         , responseDissociatePackage $
--             newDissociatePackageResponse
--
--         , responseCreateElasticsearchDomain $
--             newCreateElasticsearchDomainResponse
--
--         , responseDescribePackages $
--             newDescribePackagesResponse
--
--         , responseGetPackageVersionHistory $
--             newGetPackageVersionHistoryResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimitsResponse
--
--         , responseDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnectionsResponse
--
--         , responseAssociatePackage $
--             newAssociatePackageResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunesResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomainResponse
--
--         , responseListPackagesForDomain $
--             newListPackagesForDomainResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstancesResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdateResponse
--
--         , responseListDomainsForPackage $
--             newListDomainsForPackageResponse
--
--         , responseDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomainsResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseGetUpgradeStatus $
--             newGetUpgradeStatusResponse
--
--           ]
--     ]

-- Requests

requestDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnections -> TestTree
requestDescribeInboundCrossClusterSearchConnections =
  req
    "DescribeInboundCrossClusterSearchConnections"
    "fixture/DescribeInboundCrossClusterSearchConnections.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnection -> TestTree
requestCreateOutboundCrossClusterSearchConnection =
  req
    "CreateOutboundCrossClusterSearchConnection"
    "fixture/CreateOutboundCrossClusterSearchConnection.yaml"

requestGetUpgradeHistory :: GetUpgradeHistory -> TestTree
requestGetUpgradeHistory =
  req
    "GetUpgradeHistory"
    "fixture/GetUpgradeHistory.yaml"

requestDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
requestDescribeElasticsearchDomainConfig =
  req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

requestAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnection -> TestTree
requestAcceptInboundCrossClusterSearchConnection =
  req
    "AcceptInboundCrossClusterSearchConnection"
    "fixture/AcceptInboundCrossClusterSearchConnection.yaml"

requestDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnection -> TestTree
requestDeleteOutboundCrossClusterSearchConnection =
  req
    "DeleteOutboundCrossClusterSearchConnection"
    "fixture/DeleteOutboundCrossClusterSearchConnection.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdate -> TestTree
requestCancelElasticsearchServiceSoftwareUpdate =
  req
    "CancelElasticsearchServiceSoftwareUpdate"
    "fixture/CancelElasticsearchServiceSoftwareUpdate.yaml"

requestDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
requestDescribeElasticsearchDomain =
  req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

requestDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRole -> TestTree
requestDeleteElasticsearchServiceRole =
  req
    "DeleteElasticsearchServiceRole"
    "fixture/DeleteElasticsearchServiceRole.yaml"

requestListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypes -> TestTree
requestListElasticsearchInstanceTypes =
  req
    "ListElasticsearchInstanceTypes"
    "fixture/ListElasticsearchInstanceTypes.yaml"

requestUpdatePackage :: UpdatePackage -> TestTree
requestUpdatePackage =
  req
    "UpdatePackage"
    "fixture/UpdatePackage.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

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

requestDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
requestDeleteElasticsearchDomain =
  req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

requestGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersions -> TestTree
requestGetCompatibleElasticsearchVersions =
  req
    "GetCompatibleElasticsearchVersions"
    "fixture/GetCompatibleElasticsearchVersions.yaml"

requestDissociatePackage :: DissociatePackage -> TestTree
requestDissociatePackage =
  req
    "DissociatePackage"
    "fixture/DissociatePackage.yaml"

requestCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
requestCreateElasticsearchDomain =
  req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

requestDescribePackages :: DescribePackages -> TestTree
requestDescribePackages =
  req
    "DescribePackages"
    "fixture/DescribePackages.yaml"

requestGetPackageVersionHistory :: GetPackageVersionHistory -> TestTree
requestGetPackageVersionHistory =
  req
    "GetPackageVersionHistory"
    "fixture/GetPackageVersionHistory.yaml"

requestDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimits -> TestTree
requestDescribeElasticsearchInstanceTypeLimits =
  req
    "DescribeElasticsearchInstanceTypeLimits"
    "fixture/DescribeElasticsearchInstanceTypeLimits.yaml"

requestDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnections -> TestTree
requestDescribeOutboundCrossClusterSearchConnections =
  req
    "DescribeOutboundCrossClusterSearchConnections"
    "fixture/DescribeOutboundCrossClusterSearchConnections.yaml"

requestAssociatePackage :: AssociatePackage -> TestTree
requestAssociatePackage =
  req
    "AssociatePackage"
    "fixture/AssociatePackage.yaml"

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

requestDescribeDomainAutoTunes :: DescribeDomainAutoTunes -> TestTree
requestDescribeDomainAutoTunes =
  req
    "DescribeDomainAutoTunes"
    "fixture/DescribeDomainAutoTunes.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestUpgradeElasticsearchDomain :: UpgradeElasticsearchDomain -> TestTree
requestUpgradeElasticsearchDomain =
  req
    "UpgradeElasticsearchDomain"
    "fixture/UpgradeElasticsearchDomain.yaml"

requestListPackagesForDomain :: ListPackagesForDomain -> TestTree
requestListPackagesForDomain =
  req
    "ListPackagesForDomain"
    "fixture/ListPackagesForDomain.yaml"

requestDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstances -> TestTree
requestDescribeReservedElasticsearchInstances =
  req
    "DescribeReservedElasticsearchInstances"
    "fixture/DescribeReservedElasticsearchInstances.yaml"

requestDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferings -> TestTree
requestDescribeReservedElasticsearchInstanceOfferings =
  req
    "DescribeReservedElasticsearchInstanceOfferings"
    "fixture/DescribeReservedElasticsearchInstanceOfferings.yaml"

requestStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdate -> TestTree
requestStartElasticsearchServiceSoftwareUpdate =
  req
    "StartElasticsearchServiceSoftwareUpdate"
    "fixture/StartElasticsearchServiceSoftwareUpdate.yaml"

requestListDomainsForPackage :: ListDomainsForPackage -> TestTree
requestListDomainsForPackage =
  req
    "ListDomainsForPackage"
    "fixture/ListDomainsForPackage.yaml"

requestDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
requestDescribeElasticsearchDomains =
  req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

requestPurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOffering -> TestTree
requestPurchaseReservedElasticsearchInstanceOffering =
  req
    "PurchaseReservedElasticsearchInstanceOffering"
    "fixture/PurchaseReservedElasticsearchInstanceOffering.yaml"

requestGetUpgradeStatus :: GetUpgradeStatus -> TestTree
requestGetUpgradeStatus =
  req
    "GetUpgradeStatus"
    "fixture/GetUpgradeStatus.yaml"

-- Responses

responseDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeInboundCrossClusterSearchConnections =
  res
    "DescribeInboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeInboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInboundCrossClusterSearchConnections)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveTags)

responseCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnectionResponse -> TestTree
responseCreateOutboundCrossClusterSearchConnection =
  res
    "CreateOutboundCrossClusterSearchConnectionResponse"
    "fixture/CreateOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOutboundCrossClusterSearchConnection)

responseGetUpgradeHistory :: GetUpgradeHistoryResponse -> TestTree
responseGetUpgradeHistory =
  res
    "GetUpgradeHistoryResponse"
    "fixture/GetUpgradeHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetUpgradeHistory)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig =
  res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

responseAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> TestTree
responseAcceptInboundCrossClusterSearchConnection =
  res
    "AcceptInboundCrossClusterSearchConnectionResponse"
    "fixture/AcceptInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptInboundCrossClusterSearchConnection)

responseDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteOutboundCrossClusterSearchConnection =
  res
    "DeleteOutboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOutboundCrossClusterSearchConnection)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainNames)

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate =
  res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy CancelElasticsearchServiceSoftwareUpdate)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain =
  res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticsearchDomain)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole =
  res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteElasticsearchServiceRole)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes =
  res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListElasticsearchInstanceTypes)

responseUpdatePackage :: UpdatePackageResponse -> TestTree
responseUpdatePackage =
  res
    "UpdatePackageResponse"
    "fixture/UpdatePackageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePackage)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePackage)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteInboundCrossClusterSearchConnection =
  res
    "DeleteInboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInboundCrossClusterSearchConnection)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig =
  res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions =
  res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListElasticsearchVersions)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain =
  res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteElasticsearchDomain)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions =
  res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCompatibleElasticsearchVersions)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    defaultService
    (Proxy :: Proxy DissociatePackage)

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain =
  res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateElasticsearchDomain)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackages)

responseGetPackageVersionHistory :: GetPackageVersionHistoryResponse -> TestTree
responseGetPackageVersionHistory =
  res
    "GetPackageVersionHistoryResponse"
    "fixture/GetPackageVersionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetPackageVersionHistory)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits =
  res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticsearchInstanceTypeLimits)

responseDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeOutboundCrossClusterSearchConnections =
  res
    "DescribeOutboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeOutboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOutboundCrossClusterSearchConnections)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    defaultService
    (Proxy :: Proxy AssociatePackage)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePackage)

responseRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> TestTree
responseRejectInboundCrossClusterSearchConnection =
  res
    "RejectInboundCrossClusterSearchConnectionResponse"
    "fixture/RejectInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy RejectInboundCrossClusterSearchConnection)

responseDescribeDomainAutoTunes :: DescribeDomainAutoTunesResponse -> TestTree
responseDescribeDomainAutoTunes =
  res
    "DescribeDomainAutoTunesResponse"
    "fixture/DescribeDomainAutoTunesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomainAutoTunes)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain =
  res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpgradeElasticsearchDomain)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain =
  res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    defaultService
    (Proxy :: Proxy ListPackagesForDomain)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances =
  res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedElasticsearchInstances)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings =
  res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReservedElasticsearchInstanceOfferings)

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate =
  res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy StartElasticsearchServiceSoftwareUpdate)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage =
  res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomainsForPackage)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains =
  res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeElasticsearchDomains)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering =
  res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    defaultService
    (Proxy :: Proxy PurchaseReservedElasticsearchInstanceOffering)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetUpgradeStatus)
