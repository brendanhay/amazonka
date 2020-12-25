{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticSearch
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestCreateOutboundCrossClusterSearchConnection $
--             mkCreateOutboundCrossClusterSearchConnection
--
--         , requestDescribeInboundCrossClusterSearchConnections $
--             mkDescribeInboundCrossClusterSearchConnections
--
--         , requestCreateElasticsearchDomain $
--             mkCreateElasticsearchDomain
--
--         , requestRemoveTags $
--             mkRemoveTags
--
--         , requestGetCompatibleElasticsearchVersions $
--             mkGetCompatibleElasticsearchVersions
--
--         , requestDescribeElasticsearchDomains $
--             mkDescribeElasticsearchDomains
--
--         , requestListDomainsForPackage $
--             mkListDomainsForPackage
--
--         , requestListPackagesForDomain $
--             mkListPackagesForDomain
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             mkStartElasticsearchServiceSoftwareUpdate
--
--         , requestListElasticsearchInstanceTypes $
--             mkListElasticsearchInstanceTypes
--
--         , requestDeleteElasticsearchServiceRole $
--             mkDeleteElasticsearchServiceRole
--
--         , requestDescribeElasticsearchDomain $
--             mkDescribeElasticsearchDomain
--
--         , requestListDomainNames $
--             mkListDomainNames
--
--         , requestAssociatePackage $
--             mkAssociatePackage
--
--         , requestDeleteOutboundCrossClusterSearchConnection $
--             mkDeleteOutboundCrossClusterSearchConnection
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             mkDescribeElasticsearchInstanceTypeLimits
--
--         , requestGetPackageVersionHistory $
--             mkGetPackageVersionHistory
--
--         , requestGetUpgradeHistory $
--             mkGetUpgradeHistory
--
--         , requestDescribePackages $
--             mkDescribePackages
--
--         , requestDescribeElasticsearchDomainConfig $
--             mkDescribeElasticsearchDomainConfig
--
--         , requestGetUpgradeStatus $
--             mkGetUpgradeStatus
--
--         , requestDeleteElasticsearchDomain $
--             mkDeleteElasticsearchDomain
--
--         , requestDissociatePackage $
--             mkDissociatePackage
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             mkPurchaseReservedElasticsearchInstanceOffering
--
--         , requestDescribeReservedElasticsearchInstances $
--             mkDescribeReservedElasticsearchInstances
--
--         , requestUpdateElasticsearchDomainConfig $
--             mkUpdateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             mkListElasticsearchVersions
--
--         , requestAddTags $
--             mkAddTags
--
--         , requestDeleteInboundCrossClusterSearchConnection $
--             mkDeleteInboundCrossClusterSearchConnection
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             mkDescribeReservedElasticsearchInstanceOfferings
--
--         , requestUpgradeElasticsearchDomain $
--             mkUpgradeElasticsearchDomain
--
--         , requestListTags $
--             mkListTags
--
--         , requestDeletePackage $
--             mkDeletePackage
--
--         , requestUpdatePackage $
--             mkUpdatePackage
--
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             mkCancelElasticsearchServiceSoftwareUpdate
--
--         , requestCreatePackage $
--             mkCreatePackage
--
--         , requestRejectInboundCrossClusterSearchConnection $
--             mkRejectInboundCrossClusterSearchConnection
--
--         , requestDescribeOutboundCrossClusterSearchConnections $
--             mkDescribeOutboundCrossClusterSearchConnections
--
--         , requestAcceptInboundCrossClusterSearchConnection $
--             mkAcceptInboundCrossClusterSearchConnection
--
--           ]

--     , testGroup "response"
--         [ responseCreateOutboundCrossClusterSearchConnection $
--             mkCreateOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeInboundCrossClusterSearchConnections $
--             mkDescribeInboundCrossClusterSearchConnectionsResponse
--
--         , responseCreateElasticsearchDomain $
--             mkCreateElasticsearchDomainResponse
--
--         , responseRemoveTags $
--             mkRemoveTagsResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             mkGetCompatibleElasticsearchVersionsResponse
--
--         , responseDescribeElasticsearchDomains $
--             mkDescribeElasticsearchDomainsResponse
--
--         , responseListDomainsForPackage $
--             mkListDomainsForPackageResponse
--
--         , responseListPackagesForDomain $
--             mkListPackagesForDomainResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             mkStartElasticsearchServiceSoftwareUpdateResponse
--
--         , responseListElasticsearchInstanceTypes $
--             mkListElasticsearchInstanceTypesResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             mkDeleteElasticsearchServiceRoleResponse
--
--         , responseDescribeElasticsearchDomain $
--             mkDescribeElasticsearchDomainResponse
--
--         , responseListDomainNames $
--             mkListDomainNamesResponse
--
--         , responseAssociatePackage $
--             mkAssociatePackageResponse
--
--         , responseDeleteOutboundCrossClusterSearchConnection $
--             mkDeleteOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             mkDescribeElasticsearchInstanceTypeLimitsResponse
--
--         , responseGetPackageVersionHistory $
--             mkGetPackageVersionHistoryResponse
--
--         , responseGetUpgradeHistory $
--             mkGetUpgradeHistoryResponse
--
--         , responseDescribePackages $
--             mkDescribePackagesResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             mkDescribeElasticsearchDomainConfigResponse
--
--         , responseGetUpgradeStatus $
--             mkGetUpgradeStatusResponse
--
--         , responseDeleteElasticsearchDomain $
--             mkDeleteElasticsearchDomainResponse
--
--         , responseDissociatePackage $
--             mkDissociatePackageResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             mkPurchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             mkDescribeReservedElasticsearchInstancesResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             mkUpdateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             mkListElasticsearchVersionsResponse
--
--         , responseAddTags $
--             mkAddTagsResponse
--
--         , responseDeleteInboundCrossClusterSearchConnection $
--             mkDeleteInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             mkDescribeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseUpgradeElasticsearchDomain $
--             mkUpgradeElasticsearchDomainResponse
--
--         , responseListTags $
--             mkListTagsResponse
--
--         , responseDeletePackage $
--             mkDeletePackageResponse
--
--         , responseUpdatePackage $
--             mkUpdatePackageResponse
--
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             mkCancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseCreatePackage $
--             mkCreatePackageResponse
--
--         , responseRejectInboundCrossClusterSearchConnection $
--             mkRejectInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeOutboundCrossClusterSearchConnections $
--             mkDescribeOutboundCrossClusterSearchConnectionsResponse
--
--         , responseAcceptInboundCrossClusterSearchConnection $
--             mkAcceptInboundCrossClusterSearchConnectionResponse
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
    mkServiceConfig
    (Proxy :: Proxy CreateOutboundCrossClusterSearchConnection)

responseDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeInboundCrossClusterSearchConnections =
  res
    "DescribeInboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeInboundCrossClusterSearchConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeInboundCrossClusterSearchConnections)

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain =
  res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateElasticsearchDomain)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveTags)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions =
  res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCompatibleElasticsearchVersions)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains =
  res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticsearchDomains)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage =
  res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDomainsForPackage)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain =
  res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPackagesForDomain)

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate =
  res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartElasticsearchServiceSoftwareUpdate)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes =
  res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListElasticsearchInstanceTypes)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole =
  res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteElasticsearchServiceRole)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain =
  res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticsearchDomain)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDomainNames)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociatePackage)

responseDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteOutboundCrossClusterSearchConnection =
  res
    "DeleteOutboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteOutboundCrossClusterSearchConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOutboundCrossClusterSearchConnection)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits =
  res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticsearchInstanceTypeLimits)

responseGetPackageVersionHistory :: GetPackageVersionHistoryResponse -> TestTree
responseGetPackageVersionHistory =
  res
    "GetPackageVersionHistoryResponse"
    "fixture/GetPackageVersionHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPackageVersionHistory)

responseGetUpgradeHistory :: GetUpgradeHistoryResponse -> TestTree
responseGetUpgradeHistory =
  res
    "GetUpgradeHistoryResponse"
    "fixture/GetUpgradeHistoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUpgradeHistory)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePackages)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig =
  res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUpgradeStatus)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain =
  res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteElasticsearchDomain)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DissociatePackage)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering =
  res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PurchaseReservedElasticsearchInstanceOffering)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances =
  res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedElasticsearchInstances)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig =
  res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions =
  res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListElasticsearchVersions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddTags)

responseDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteInboundCrossClusterSearchConnection =
  res
    "DeleteInboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteInboundCrossClusterSearchConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInboundCrossClusterSearchConnection)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings =
  res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeReservedElasticsearchInstanceOfferings)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain =
  res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpgradeElasticsearchDomain)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTags)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePackage)

responseUpdatePackage :: UpdatePackageResponse -> TestTree
responseUpdatePackage =
  res
    "UpdatePackageResponse"
    "fixture/UpdatePackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePackage)

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate =
  res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelElasticsearchServiceSoftwareUpdate)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePackage)

responseRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> TestTree
responseRejectInboundCrossClusterSearchConnection =
  res
    "RejectInboundCrossClusterSearchConnectionResponse"
    "fixture/RejectInboundCrossClusterSearchConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RejectInboundCrossClusterSearchConnection)

responseDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeOutboundCrossClusterSearchConnections =
  res
    "DescribeOutboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeOutboundCrossClusterSearchConnectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOutboundCrossClusterSearchConnections)

responseAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> TestTree
responseAcceptInboundCrossClusterSearchConnection =
  res
    "AcceptInboundCrossClusterSearchConnectionResponse"
    "fixture/AcceptInboundCrossClusterSearchConnectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptInboundCrossClusterSearchConnection)
