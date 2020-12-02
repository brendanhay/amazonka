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
--             createOutboundCrossClusterSearchConnection
--
--         , requestDescribeInboundCrossClusterSearchConnections $
--             describeInboundCrossClusterSearchConnections
--
--         , requestCreateElasticsearchDomain $
--             createElasticsearchDomain
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestGetCompatibleElasticsearchVersions $
--             getCompatibleElasticsearchVersions
--
--         , requestDescribeElasticsearchDomains $
--             describeElasticsearchDomains
--
--         , requestListDomainsForPackage $
--             listDomainsForPackage
--
--         , requestListPackagesForDomain $
--             listPackagesForDomain
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             startElasticsearchServiceSoftwareUpdate
--
--         , requestListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypes
--
--         , requestDeleteElasticsearchServiceRole $
--             deleteElasticsearchServiceRole
--
--         , requestDescribeElasticsearchDomain $
--             describeElasticsearchDomain
--
--         , requestListDomainNames $
--             listDomainNames
--
--         , requestAssociatePackage $
--             associatePackage
--
--         , requestDeleteOutboundCrossClusterSearchConnection $
--             deleteOutboundCrossClusterSearchConnection
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimits
--
--         , requestGetPackageVersionHistory $
--             getPackageVersionHistory
--
--         , requestGetUpgradeHistory $
--             getUpgradeHistory
--
--         , requestDescribePackages $
--             describePackages
--
--         , requestDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfig
--
--         , requestGetUpgradeStatus $
--             getUpgradeStatus
--
--         , requestDeleteElasticsearchDomain $
--             deleteElasticsearchDomain
--
--         , requestDissociatePackage $
--             dissociatePackage
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             purchaseReservedElasticsearchInstanceOffering
--
--         , requestDescribeReservedElasticsearchInstances $
--             describeReservedElasticsearchInstances
--
--         , requestUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             listElasticsearchVersions
--
--         , requestAddTags $
--             addTags
--
--         , requestDeleteInboundCrossClusterSearchConnection $
--             deleteInboundCrossClusterSearchConnection
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             describeReservedElasticsearchInstanceOfferings
--
--         , requestUpgradeElasticsearchDomain $
--             upgradeElasticsearchDomain
--
--         , requestListTags $
--             listTags
--
--         , requestDeletePackage $
--             deletePackage
--
--         , requestUpdatePackage $
--             updatePackage
--
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             cancelElasticsearchServiceSoftwareUpdate
--
--         , requestCreatePackage $
--             createPackage
--
--         , requestRejectInboundCrossClusterSearchConnection $
--             rejectInboundCrossClusterSearchConnection
--
--         , requestDescribeOutboundCrossClusterSearchConnections $
--             describeOutboundCrossClusterSearchConnections
--
--         , requestAcceptInboundCrossClusterSearchConnection $
--             acceptInboundCrossClusterSearchConnection
--
--           ]

--     , testGroup "response"
--         [ responseCreateOutboundCrossClusterSearchConnection $
--             createOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeInboundCrossClusterSearchConnections $
--             describeInboundCrossClusterSearchConnectionsResponse
--
--         , responseCreateElasticsearchDomain $
--             createElasticsearchDomainResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             getCompatibleElasticsearchVersionsResponse
--
--         , responseDescribeElasticsearchDomains $
--             describeElasticsearchDomainsResponse
--
--         , responseListDomainsForPackage $
--             listDomainsForPackageResponse
--
--         , responseListPackagesForDomain $
--             listPackagesForDomainResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             startElasticsearchServiceSoftwareUpdateResponse
--
--         , responseListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypesResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             deleteElasticsearchServiceRoleResponse
--
--         , responseDescribeElasticsearchDomain $
--             describeElasticsearchDomainResponse
--
--         , responseListDomainNames $
--             listDomainNamesResponse
--
--         , responseAssociatePackage $
--             associatePackageResponse
--
--         , responseDeleteOutboundCrossClusterSearchConnection $
--             deleteOutboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimitsResponse
--
--         , responseGetPackageVersionHistory $
--             getPackageVersionHistoryResponse
--
--         , responseGetUpgradeHistory $
--             getUpgradeHistoryResponse
--
--         , responseDescribePackages $
--             describePackagesResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfigResponse
--
--         , responseGetUpgradeStatus $
--             getUpgradeStatusResponse
--
--         , responseDeleteElasticsearchDomain $
--             deleteElasticsearchDomainResponse
--
--         , responseDissociatePackage $
--             dissociatePackageResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             purchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             describeReservedElasticsearchInstancesResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             listElasticsearchVersionsResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseDeleteInboundCrossClusterSearchConnection $
--             deleteInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             describeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseUpgradeElasticsearchDomain $
--             upgradeElasticsearchDomainResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDeletePackage $
--             deletePackageResponse
--
--         , responseUpdatePackage $
--             updatePackageResponse
--
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             cancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseCreatePackage $
--             createPackageResponse
--
--         , responseRejectInboundCrossClusterSearchConnection $
--             rejectInboundCrossClusterSearchConnectionResponse
--
--         , responseDescribeOutboundCrossClusterSearchConnections $
--             describeOutboundCrossClusterSearchConnectionsResponse
--
--         , responseAcceptInboundCrossClusterSearchConnection $
--             acceptInboundCrossClusterSearchConnectionResponse
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
    elasticSearch
    (Proxy :: Proxy CreateOutboundCrossClusterSearchConnection)

responseDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeInboundCrossClusterSearchConnections =
  res
    "DescribeInboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeInboundCrossClusterSearchConnectionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeInboundCrossClusterSearchConnections)

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain =
  res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreateElasticsearchDomain)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy RemoveTags)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions =
  res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetCompatibleElasticsearchVersions)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains =
  res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomains)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage =
  res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainsForPackage)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain =
  res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListPackagesForDomain)

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate =
  res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    elasticSearch
    (Proxy :: Proxy StartElasticsearchServiceSoftwareUpdate)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes =
  res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchInstanceTypes)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole =
  res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchServiceRole)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain =
  res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomain)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainNames)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy AssociatePackage)

responseDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteOutboundCrossClusterSearchConnection =
  res
    "DeleteOutboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteOutboundCrossClusterSearchConnectionResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteOutboundCrossClusterSearchConnection)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits =
  res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchInstanceTypeLimits)

responseGetPackageVersionHistory :: GetPackageVersionHistoryResponse -> TestTree
responseGetPackageVersionHistory =
  res
    "GetPackageVersionHistoryResponse"
    "fixture/GetPackageVersionHistoryResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetPackageVersionHistory)

responseGetUpgradeHistory :: GetUpgradeHistoryResponse -> TestTree
responseGetUpgradeHistory =
  res
    "GetUpgradeHistoryResponse"
    "fixture/GetUpgradeHistoryResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetUpgradeHistory)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribePackages)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig =
  res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetUpgradeStatus)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain =
  res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchDomain)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy DissociatePackage)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering =
  res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    elasticSearch
    (Proxy :: Proxy PurchaseReservedElasticsearchInstanceOffering)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances =
  res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeReservedElasticsearchInstances)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig =
  res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions =
  res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchVersions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy AddTags)

responseDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteInboundCrossClusterSearchConnection =
  res
    "DeleteInboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteInboundCrossClusterSearchConnectionResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteInboundCrossClusterSearchConnection)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings =
  res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeReservedElasticsearchInstanceOfferings)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain =
  res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpgradeElasticsearchDomain)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListTags)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeletePackage)

responseUpdatePackage :: UpdatePackageResponse -> TestTree
responseUpdatePackage =
  res
    "UpdatePackageResponse"
    "fixture/UpdatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpdatePackage)

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate =
  res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    elasticSearch
    (Proxy :: Proxy CancelElasticsearchServiceSoftwareUpdate)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreatePackage)

responseRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> TestTree
responseRejectInboundCrossClusterSearchConnection =
  res
    "RejectInboundCrossClusterSearchConnectionResponse"
    "fixture/RejectInboundCrossClusterSearchConnectionResponse.proto"
    elasticSearch
    (Proxy :: Proxy RejectInboundCrossClusterSearchConnection)

responseDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeOutboundCrossClusterSearchConnections =
  res
    "DescribeOutboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeOutboundCrossClusterSearchConnectionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeOutboundCrossClusterSearchConnections)

responseAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> TestTree
responseAcceptInboundCrossClusterSearchConnection =
  res
    "AcceptInboundCrossClusterSearchConnectionResponse"
    "fixture/AcceptInboundCrossClusterSearchConnectionResponse.proto"
    elasticSearch
    (Proxy :: Proxy AcceptInboundCrossClusterSearchConnection)
