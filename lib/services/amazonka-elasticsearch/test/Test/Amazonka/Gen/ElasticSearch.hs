{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElasticSearch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ElasticSearch where

import Amazonka.ElasticSearch
import qualified Data.Proxy as Proxy
import Test.Amazonka.ElasticSearch.Internal
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
--         [ requestAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnection
--
--         , requestAddTags $
--             newAddTags
--
--         , requestAssociatePackage $
--             newAssociatePackage
--
--         , requestAuthorizeVpcEndpointAccess $
--             newAuthorizeVpcEndpointAccess
--
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdate
--
--         , requestCreateElasticsearchDomain $
--             newCreateElasticsearchDomain
--
--         , requestCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnection
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomain
--
--         , requestDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRole
--
--         , requestDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnection
--
--         , requestDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnection
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestDeleteVpcEndpoint $
--             newDeleteVpcEndpoint
--
--         , requestDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunes
--
--         , requestDescribeDomainChangeProgress $
--             newDescribeDomainChangeProgress
--
--         , requestDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomain
--
--         , requestDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfig
--
--         , requestDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomains
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimits
--
--         , requestDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnections
--
--         , requestDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnections
--
--         , requestDescribePackages $
--             newDescribePackages
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferings
--
--         , requestDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstances
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestDissociatePackage $
--             newDissociatePackage
--
--         , requestGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersions
--
--         , requestGetPackageVersionHistory $
--             newGetPackageVersionHistory
--
--         , requestGetUpgradeHistory $
--             newGetUpgradeHistory
--
--         , requestGetUpgradeStatus $
--             newGetUpgradeStatus
--
--         , requestListDomainNames $
--             newListDomainNames
--
--         , requestListDomainsForPackage $
--             newListDomainsForPackage
--
--         , requestListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypes
--
--         , requestListElasticsearchVersions $
--             newListElasticsearchVersions
--
--         , requestListPackagesForDomain $
--             newListPackagesForDomain
--
--         , requestListTags $
--             newListTags
--
--         , requestListVpcEndpointAccess $
--             newListVpcEndpointAccess
--
--         , requestListVpcEndpoints $
--             newListVpcEndpoints
--
--         , requestListVpcEndpointsForDomain $
--             newListVpcEndpointsForDomain
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOffering
--
--         , requestRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnection
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestRevokeVpcEndpointAccess $
--             newRevokeVpcEndpointAccess
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdate
--
--         , requestUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfig
--
--         , requestUpdatePackage $
--             newUpdatePackage
--
--         , requestUpdateVpcEndpoint $
--             newUpdateVpcEndpoint
--
--         , requestUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomain
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInboundCrossClusterSearchConnection $
--             newAcceptInboundCrossClusterSearchConnectionResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseAssociatePackage $
--             newAssociatePackageResponse
--
--         , responseAuthorizeVpcEndpointAccess $
--             newAuthorizeVpcEndpointAccessResponse
--
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             newCancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseCreateElasticsearchDomain $
--             newCreateElasticsearchDomainResponse
--
--         , responseCreateOutboundCrossClusterSearchConnection $
--             newCreateOutboundCrossClusterSearchConnectionResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseDeleteElasticsearchDomain $
--             newDeleteElasticsearchDomainResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             newDeleteElasticsearchServiceRoleResponse
--
--         , responseDeleteInboundCrossClusterSearchConnection $
--             newDeleteInboundCrossClusterSearchConnectionResponse
--
--         , responseDeleteOutboundCrossClusterSearchConnection $
--             newDeleteOutboundCrossClusterSearchConnectionResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseDeleteVpcEndpoint $
--             newDeleteVpcEndpointResponse
--
--         , responseDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunesResponse
--
--         , responseDescribeDomainChangeProgress $
--             newDescribeDomainChangeProgressResponse
--
--         , responseDescribeElasticsearchDomain $
--             newDescribeElasticsearchDomainResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             newDescribeElasticsearchDomainConfigResponse
--
--         , responseDescribeElasticsearchDomains $
--             newDescribeElasticsearchDomainsResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             newDescribeElasticsearchInstanceTypeLimitsResponse
--
--         , responseDescribeInboundCrossClusterSearchConnections $
--             newDescribeInboundCrossClusterSearchConnectionsResponse
--
--         , responseDescribeOutboundCrossClusterSearchConnections $
--             newDescribeOutboundCrossClusterSearchConnectionsResponse
--
--         , responseDescribePackages $
--             newDescribePackagesResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             newDescribeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             newDescribeReservedElasticsearchInstancesResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseDissociatePackage $
--             newDissociatePackageResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             newGetCompatibleElasticsearchVersionsResponse
--
--         , responseGetPackageVersionHistory $
--             newGetPackageVersionHistoryResponse
--
--         , responseGetUpgradeHistory $
--             newGetUpgradeHistoryResponse
--
--         , responseGetUpgradeStatus $
--             newGetUpgradeStatusResponse
--
--         , responseListDomainNames $
--             newListDomainNamesResponse
--
--         , responseListDomainsForPackage $
--             newListDomainsForPackageResponse
--
--         , responseListElasticsearchInstanceTypes $
--             newListElasticsearchInstanceTypesResponse
--
--         , responseListElasticsearchVersions $
--             newListElasticsearchVersionsResponse
--
--         , responseListPackagesForDomain $
--             newListPackagesForDomainResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseListVpcEndpointAccess $
--             newListVpcEndpointAccessResponse
--
--         , responseListVpcEndpoints $
--             newListVpcEndpointsResponse
--
--         , responseListVpcEndpointsForDomain $
--             newListVpcEndpointsForDomainResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             newPurchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseRejectInboundCrossClusterSearchConnection $
--             newRejectInboundCrossClusterSearchConnectionResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseRevokeVpcEndpointAccess $
--             newRevokeVpcEndpointAccessResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             newStartElasticsearchServiceSoftwareUpdateResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             newUpdateElasticsearchDomainConfigResponse
--
--         , responseUpdatePackage $
--             newUpdatePackageResponse
--
--         , responseUpdateVpcEndpoint $
--             newUpdateVpcEndpointResponse
--
--         , responseUpgradeElasticsearchDomain $
--             newUpgradeElasticsearchDomainResponse
--
--           ]
--     ]

-- Requests

requestAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnection -> TestTree
requestAcceptInboundCrossClusterSearchConnection =
  req
    "AcceptInboundCrossClusterSearchConnection"
    "fixture/AcceptInboundCrossClusterSearchConnection.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestAssociatePackage :: AssociatePackage -> TestTree
requestAssociatePackage =
  req
    "AssociatePackage"
    "fixture/AssociatePackage.yaml"

requestAuthorizeVpcEndpointAccess :: AuthorizeVpcEndpointAccess -> TestTree
requestAuthorizeVpcEndpointAccess =
  req
    "AuthorizeVpcEndpointAccess"
    "fixture/AuthorizeVpcEndpointAccess.yaml"

requestCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdate -> TestTree
requestCancelElasticsearchServiceSoftwareUpdate =
  req
    "CancelElasticsearchServiceSoftwareUpdate"
    "fixture/CancelElasticsearchServiceSoftwareUpdate.yaml"

requestCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
requestCreateElasticsearchDomain =
  req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

requestCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnection -> TestTree
requestCreateOutboundCrossClusterSearchConnection =
  req
    "CreateOutboundCrossClusterSearchConnection"
    "fixture/CreateOutboundCrossClusterSearchConnection.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage =
  req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
requestDeleteElasticsearchDomain =
  req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

requestDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRole -> TestTree
requestDeleteElasticsearchServiceRole =
  req
    "DeleteElasticsearchServiceRole"
    "fixture/DeleteElasticsearchServiceRole.yaml"

requestDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnection -> TestTree
requestDeleteInboundCrossClusterSearchConnection =
  req
    "DeleteInboundCrossClusterSearchConnection"
    "fixture/DeleteInboundCrossClusterSearchConnection.yaml"

requestDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnection -> TestTree
requestDeleteOutboundCrossClusterSearchConnection =
  req
    "DeleteOutboundCrossClusterSearchConnection"
    "fixture/DeleteOutboundCrossClusterSearchConnection.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestDeleteVpcEndpoint :: DeleteVpcEndpoint -> TestTree
requestDeleteVpcEndpoint =
  req
    "DeleteVpcEndpoint"
    "fixture/DeleteVpcEndpoint.yaml"

requestDescribeDomainAutoTunes :: DescribeDomainAutoTunes -> TestTree
requestDescribeDomainAutoTunes =
  req
    "DescribeDomainAutoTunes"
    "fixture/DescribeDomainAutoTunes.yaml"

requestDescribeDomainChangeProgress :: DescribeDomainChangeProgress -> TestTree
requestDescribeDomainChangeProgress =
  req
    "DescribeDomainChangeProgress"
    "fixture/DescribeDomainChangeProgress.yaml"

requestDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
requestDescribeElasticsearchDomain =
  req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

requestDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
requestDescribeElasticsearchDomainConfig =
  req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

requestDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
requestDescribeElasticsearchDomains =
  req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

requestDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimits -> TestTree
requestDescribeElasticsearchInstanceTypeLimits =
  req
    "DescribeElasticsearchInstanceTypeLimits"
    "fixture/DescribeElasticsearchInstanceTypeLimits.yaml"

requestDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnections -> TestTree
requestDescribeInboundCrossClusterSearchConnections =
  req
    "DescribeInboundCrossClusterSearchConnections"
    "fixture/DescribeInboundCrossClusterSearchConnections.yaml"

requestDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnections -> TestTree
requestDescribeOutboundCrossClusterSearchConnections =
  req
    "DescribeOutboundCrossClusterSearchConnections"
    "fixture/DescribeOutboundCrossClusterSearchConnections.yaml"

requestDescribePackages :: DescribePackages -> TestTree
requestDescribePackages =
  req
    "DescribePackages"
    "fixture/DescribePackages.yaml"

requestDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferings -> TestTree
requestDescribeReservedElasticsearchInstanceOfferings =
  req
    "DescribeReservedElasticsearchInstanceOfferings"
    "fixture/DescribeReservedElasticsearchInstanceOfferings.yaml"

requestDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstances -> TestTree
requestDescribeReservedElasticsearchInstances =
  req
    "DescribeReservedElasticsearchInstances"
    "fixture/DescribeReservedElasticsearchInstances.yaml"

requestDescribeVpcEndpoints :: DescribeVpcEndpoints -> TestTree
requestDescribeVpcEndpoints =
  req
    "DescribeVpcEndpoints"
    "fixture/DescribeVpcEndpoints.yaml"

requestDissociatePackage :: DissociatePackage -> TestTree
requestDissociatePackage =
  req
    "DissociatePackage"
    "fixture/DissociatePackage.yaml"

requestGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersions -> TestTree
requestGetCompatibleElasticsearchVersions =
  req
    "GetCompatibleElasticsearchVersions"
    "fixture/GetCompatibleElasticsearchVersions.yaml"

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

requestGetUpgradeStatus :: GetUpgradeStatus -> TestTree
requestGetUpgradeStatus =
  req
    "GetUpgradeStatus"
    "fixture/GetUpgradeStatus.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames =
  req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestListDomainsForPackage :: ListDomainsForPackage -> TestTree
requestListDomainsForPackage =
  req
    "ListDomainsForPackage"
    "fixture/ListDomainsForPackage.yaml"

requestListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypes -> TestTree
requestListElasticsearchInstanceTypes =
  req
    "ListElasticsearchInstanceTypes"
    "fixture/ListElasticsearchInstanceTypes.yaml"

requestListElasticsearchVersions :: ListElasticsearchVersions -> TestTree
requestListElasticsearchVersions =
  req
    "ListElasticsearchVersions"
    "fixture/ListElasticsearchVersions.yaml"

requestListPackagesForDomain :: ListPackagesForDomain -> TestTree
requestListPackagesForDomain =
  req
    "ListPackagesForDomain"
    "fixture/ListPackagesForDomain.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestListVpcEndpointAccess :: ListVpcEndpointAccess -> TestTree
requestListVpcEndpointAccess =
  req
    "ListVpcEndpointAccess"
    "fixture/ListVpcEndpointAccess.yaml"

requestListVpcEndpoints :: ListVpcEndpoints -> TestTree
requestListVpcEndpoints =
  req
    "ListVpcEndpoints"
    "fixture/ListVpcEndpoints.yaml"

requestListVpcEndpointsForDomain :: ListVpcEndpointsForDomain -> TestTree
requestListVpcEndpointsForDomain =
  req
    "ListVpcEndpointsForDomain"
    "fixture/ListVpcEndpointsForDomain.yaml"

requestPurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOffering -> TestTree
requestPurchaseReservedElasticsearchInstanceOffering =
  req
    "PurchaseReservedElasticsearchInstanceOffering"
    "fixture/PurchaseReservedElasticsearchInstanceOffering.yaml"

requestRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnection -> TestTree
requestRejectInboundCrossClusterSearchConnection =
  req
    "RejectInboundCrossClusterSearchConnection"
    "fixture/RejectInboundCrossClusterSearchConnection.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags =
  req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestRevokeVpcEndpointAccess :: RevokeVpcEndpointAccess -> TestTree
requestRevokeVpcEndpointAccess =
  req
    "RevokeVpcEndpointAccess"
    "fixture/RevokeVpcEndpointAccess.yaml"

requestStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdate -> TestTree
requestStartElasticsearchServiceSoftwareUpdate =
  req
    "StartElasticsearchServiceSoftwareUpdate"
    "fixture/StartElasticsearchServiceSoftwareUpdate.yaml"

requestUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfig -> TestTree
requestUpdateElasticsearchDomainConfig =
  req
    "UpdateElasticsearchDomainConfig"
    "fixture/UpdateElasticsearchDomainConfig.yaml"

requestUpdatePackage :: UpdatePackage -> TestTree
requestUpdatePackage =
  req
    "UpdatePackage"
    "fixture/UpdatePackage.yaml"

requestUpdateVpcEndpoint :: UpdateVpcEndpoint -> TestTree
requestUpdateVpcEndpoint =
  req
    "UpdateVpcEndpoint"
    "fixture/UpdateVpcEndpoint.yaml"

requestUpgradeElasticsearchDomain :: UpgradeElasticsearchDomain -> TestTree
requestUpgradeElasticsearchDomain =
  req
    "UpgradeElasticsearchDomain"
    "fixture/UpgradeElasticsearchDomain.yaml"

-- Responses

responseAcceptInboundCrossClusterSearchConnection :: AcceptInboundCrossClusterSearchConnectionResponse -> TestTree
responseAcceptInboundCrossClusterSearchConnection =
  res
    "AcceptInboundCrossClusterSearchConnectionResponse"
    "fixture/AcceptInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInboundCrossClusterSearchConnection)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage =
  res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePackage)

responseAuthorizeVpcEndpointAccess :: AuthorizeVpcEndpointAccessResponse -> TestTree
responseAuthorizeVpcEndpointAccess =
  res
    "AuthorizeVpcEndpointAccessResponse"
    "fixture/AuthorizeVpcEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AuthorizeVpcEndpointAccess)

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate =
  res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelElasticsearchServiceSoftwareUpdate)

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain =
  res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateElasticsearchDomain)

responseCreateOutboundCrossClusterSearchConnection :: CreateOutboundCrossClusterSearchConnectionResponse -> TestTree
responseCreateOutboundCrossClusterSearchConnection =
  res
    "CreateOutboundCrossClusterSearchConnectionResponse"
    "fixture/CreateOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOutboundCrossClusterSearchConnection)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackage)

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpoint)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain =
  res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteElasticsearchDomain)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole =
  res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteElasticsearchServiceRole)

responseDeleteInboundCrossClusterSearchConnection :: DeleteInboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteInboundCrossClusterSearchConnection =
  res
    "DeleteInboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInboundCrossClusterSearchConnection)

responseDeleteOutboundCrossClusterSearchConnection :: DeleteOutboundCrossClusterSearchConnectionResponse -> TestTree
responseDeleteOutboundCrossClusterSearchConnection =
  res
    "DeleteOutboundCrossClusterSearchConnectionResponse"
    "fixture/DeleteOutboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutboundCrossClusterSearchConnection)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackage)

responseDeleteVpcEndpoint :: DeleteVpcEndpointResponse -> TestTree
responseDeleteVpcEndpoint =
  res
    "DeleteVpcEndpointResponse"
    "fixture/DeleteVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVpcEndpoint)

responseDescribeDomainAutoTunes :: DescribeDomainAutoTunesResponse -> TestTree
responseDescribeDomainAutoTunes =
  res
    "DescribeDomainAutoTunesResponse"
    "fixture/DescribeDomainAutoTunesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainAutoTunes)

responseDescribeDomainChangeProgress :: DescribeDomainChangeProgressResponse -> TestTree
responseDescribeDomainChangeProgress =
  res
    "DescribeDomainChangeProgressResponse"
    "fixture/DescribeDomainChangeProgressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainChangeProgress)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain =
  res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomain)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig =
  res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomainConfig)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains =
  res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchDomains)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits =
  res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeElasticsearchInstanceTypeLimits)

responseDescribeInboundCrossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeInboundCrossClusterSearchConnections =
  res
    "DescribeInboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeInboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundCrossClusterSearchConnections)

responseDescribeOutboundCrossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> TestTree
responseDescribeOutboundCrossClusterSearchConnections =
  res
    "DescribeOutboundCrossClusterSearchConnectionsResponse"
    "fixture/DescribeOutboundCrossClusterSearchConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOutboundCrossClusterSearchConnections)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackages)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings =
  res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedElasticsearchInstanceOfferings)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances =
  res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedElasticsearchInstances)

responseDescribeVpcEndpoints :: DescribeVpcEndpointsResponse -> TestTree
responseDescribeVpcEndpoints =
  res
    "DescribeVpcEndpointsResponse"
    "fixture/DescribeVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeVpcEndpoints)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage =
  res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DissociatePackage)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions =
  res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleElasticsearchVersions)

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

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus =
  res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUpgradeStatus)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames =
  res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainNames)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage =
  res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomainsForPackage)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes =
  res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListElasticsearchInstanceTypes)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions =
  res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListElasticsearchVersions)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain =
  res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackagesForDomain)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseListVpcEndpointAccess :: ListVpcEndpointAccessResponse -> TestTree
responseListVpcEndpointAccess =
  res
    "ListVpcEndpointAccessResponse"
    "fixture/ListVpcEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcEndpointAccess)

responseListVpcEndpoints :: ListVpcEndpointsResponse -> TestTree
responseListVpcEndpoints =
  res
    "ListVpcEndpointsResponse"
    "fixture/ListVpcEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcEndpoints)

responseListVpcEndpointsForDomain :: ListVpcEndpointsForDomainResponse -> TestTree
responseListVpcEndpointsForDomain =
  res
    "ListVpcEndpointsForDomainResponse"
    "fixture/ListVpcEndpointsForDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVpcEndpointsForDomain)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering =
  res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedElasticsearchInstanceOffering)

responseRejectInboundCrossClusterSearchConnection :: RejectInboundCrossClusterSearchConnectionResponse -> TestTree
responseRejectInboundCrossClusterSearchConnection =
  res
    "RejectInboundCrossClusterSearchConnectionResponse"
    "fixture/RejectInboundCrossClusterSearchConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInboundCrossClusterSearchConnection)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags =
  res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveTags)

responseRevokeVpcEndpointAccess :: RevokeVpcEndpointAccessResponse -> TestTree
responseRevokeVpcEndpointAccess =
  res
    "RevokeVpcEndpointAccessResponse"
    "fixture/RevokeVpcEndpointAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeVpcEndpointAccess)

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate =
  res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartElasticsearchServiceSoftwareUpdate)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig =
  res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateElasticsearchDomainConfig)

responseUpdatePackage :: UpdatePackageResponse -> TestTree
responseUpdatePackage =
  res
    "UpdatePackageResponse"
    "fixture/UpdatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePackage)

responseUpdateVpcEndpoint :: UpdateVpcEndpointResponse -> TestTree
responseUpdateVpcEndpoint =
  res
    "UpdateVpcEndpointResponse"
    "fixture/UpdateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVpcEndpoint)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain =
  res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeElasticsearchDomain)
