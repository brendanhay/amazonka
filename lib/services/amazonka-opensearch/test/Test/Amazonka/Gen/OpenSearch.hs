{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OpenSearch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OpenSearch where

import Amazonka.OpenSearch
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OpenSearch.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptInboundConnection $
--             newAcceptInboundConnection
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
--         , requestCancelServiceSoftwareUpdate $
--             newCancelServiceSoftwareUpdate
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateOutboundConnection $
--             newCreateOutboundConnection
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestCreateVpcEndpoint $
--             newCreateVpcEndpoint
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteInboundConnection $
--             newDeleteInboundConnection
--
--         , requestDeleteOutboundConnection $
--             newDeleteOutboundConnection
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestDeleteVpcEndpoint $
--             newDeleteVpcEndpoint
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunes
--
--         , requestDescribeDomainChangeProgress $
--             newDescribeDomainChangeProgress
--
--         , requestDescribeDomainConfig $
--             newDescribeDomainConfig
--
--         , requestDescribeDomains $
--             newDescribeDomains
--
--         , requestDescribeInboundConnections $
--             newDescribeInboundConnections
--
--         , requestDescribeInstanceTypeLimits $
--             newDescribeInstanceTypeLimits
--
--         , requestDescribeOutboundConnections $
--             newDescribeOutboundConnections
--
--         , requestDescribePackages $
--             newDescribePackages
--
--         , requestDescribeReservedInstanceOfferings $
--             newDescribeReservedInstanceOfferings
--
--         , requestDescribeReservedInstances $
--             newDescribeReservedInstances
--
--         , requestDescribeVpcEndpoints $
--             newDescribeVpcEndpoints
--
--         , requestDissociatePackage $
--             newDissociatePackage
--
--         , requestGetCompatibleVersions $
--             newGetCompatibleVersions
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
--         , requestListInstanceTypeDetails $
--             newListInstanceTypeDetails
--
--         , requestListPackagesForDomain $
--             newListPackagesForDomain
--
--         , requestListTags $
--             newListTags
--
--         , requestListVersions $
--             newListVersions
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
--         , requestPurchaseReservedInstanceOffering $
--             newPurchaseReservedInstanceOffering
--
--         , requestRejectInboundConnection $
--             newRejectInboundConnection
--
--         , requestRemoveTags $
--             newRemoveTags
--
--         , requestRevokeVpcEndpointAccess $
--             newRevokeVpcEndpointAccess
--
--         , requestStartServiceSoftwareUpdate $
--             newStartServiceSoftwareUpdate
--
--         , requestUpdateDomainConfig $
--             newUpdateDomainConfig
--
--         , requestUpdatePackage $
--             newUpdatePackage
--
--         , requestUpdateVpcEndpoint $
--             newUpdateVpcEndpoint
--
--         , requestUpgradeDomain $
--             newUpgradeDomain
--
--           ]

--     , testGroup "response"
--         [ responseAcceptInboundConnection $
--             newAcceptInboundConnectionResponse
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
--         , responseCancelServiceSoftwareUpdate $
--             newCancelServiceSoftwareUpdateResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateOutboundConnection $
--             newCreateOutboundConnectionResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseCreateVpcEndpoint $
--             newCreateVpcEndpointResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteInboundConnection $
--             newDeleteInboundConnectionResponse
--
--         , responseDeleteOutboundConnection $
--             newDeleteOutboundConnectionResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseDeleteVpcEndpoint $
--             newDeleteVpcEndpointResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribeDomainAutoTunes $
--             newDescribeDomainAutoTunesResponse
--
--         , responseDescribeDomainChangeProgress $
--             newDescribeDomainChangeProgressResponse
--
--         , responseDescribeDomainConfig $
--             newDescribeDomainConfigResponse
--
--         , responseDescribeDomains $
--             newDescribeDomainsResponse
--
--         , responseDescribeInboundConnections $
--             newDescribeInboundConnectionsResponse
--
--         , responseDescribeInstanceTypeLimits $
--             newDescribeInstanceTypeLimitsResponse
--
--         , responseDescribeOutboundConnections $
--             newDescribeOutboundConnectionsResponse
--
--         , responseDescribePackages $
--             newDescribePackagesResponse
--
--         , responseDescribeReservedInstanceOfferings $
--             newDescribeReservedInstanceOfferingsResponse
--
--         , responseDescribeReservedInstances $
--             newDescribeReservedInstancesResponse
--
--         , responseDescribeVpcEndpoints $
--             newDescribeVpcEndpointsResponse
--
--         , responseDissociatePackage $
--             newDissociatePackageResponse
--
--         , responseGetCompatibleVersions $
--             newGetCompatibleVersionsResponse
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
--         , responseListInstanceTypeDetails $
--             newListInstanceTypeDetailsResponse
--
--         , responseListPackagesForDomain $
--             newListPackagesForDomainResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseListVersions $
--             newListVersionsResponse
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
--         , responsePurchaseReservedInstanceOffering $
--             newPurchaseReservedInstanceOfferingResponse
--
--         , responseRejectInboundConnection $
--             newRejectInboundConnectionResponse
--
--         , responseRemoveTags $
--             newRemoveTagsResponse
--
--         , responseRevokeVpcEndpointAccess $
--             newRevokeVpcEndpointAccessResponse
--
--         , responseStartServiceSoftwareUpdate $
--             newStartServiceSoftwareUpdateResponse
--
--         , responseUpdateDomainConfig $
--             newUpdateDomainConfigResponse
--
--         , responseUpdatePackage $
--             newUpdatePackageResponse
--
--         , responseUpdateVpcEndpoint $
--             newUpdateVpcEndpointResponse
--
--         , responseUpgradeDomain $
--             newUpgradeDomainResponse
--
--           ]
--     ]

-- Requests

requestAcceptInboundConnection :: AcceptInboundConnection -> TestTree
requestAcceptInboundConnection =
  req
    "AcceptInboundConnection"
    "fixture/AcceptInboundConnection.yaml"

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

requestCancelServiceSoftwareUpdate :: CancelServiceSoftwareUpdate -> TestTree
requestCancelServiceSoftwareUpdate =
  req
    "CancelServiceSoftwareUpdate"
    "fixture/CancelServiceSoftwareUpdate.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

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

requestCreateVpcEndpoint :: CreateVpcEndpoint -> TestTree
requestCreateVpcEndpoint =
  req
    "CreateVpcEndpoint"
    "fixture/CreateVpcEndpoint.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteInboundConnection :: DeleteInboundConnection -> TestTree
requestDeleteInboundConnection =
  req
    "DeleteInboundConnection"
    "fixture/DeleteInboundConnection.yaml"

requestDeleteOutboundConnection :: DeleteOutboundConnection -> TestTree
requestDeleteOutboundConnection =
  req
    "DeleteOutboundConnection"
    "fixture/DeleteOutboundConnection.yaml"

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

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

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

requestDescribeDomainConfig :: DescribeDomainConfig -> TestTree
requestDescribeDomainConfig =
  req
    "DescribeDomainConfig"
    "fixture/DescribeDomainConfig.yaml"

requestDescribeDomains :: DescribeDomains -> TestTree
requestDescribeDomains =
  req
    "DescribeDomains"
    "fixture/DescribeDomains.yaml"

requestDescribeInboundConnections :: DescribeInboundConnections -> TestTree
requestDescribeInboundConnections =
  req
    "DescribeInboundConnections"
    "fixture/DescribeInboundConnections.yaml"

requestDescribeInstanceTypeLimits :: DescribeInstanceTypeLimits -> TestTree
requestDescribeInstanceTypeLimits =
  req
    "DescribeInstanceTypeLimits"
    "fixture/DescribeInstanceTypeLimits.yaml"

requestDescribeOutboundConnections :: DescribeOutboundConnections -> TestTree
requestDescribeOutboundConnections =
  req
    "DescribeOutboundConnections"
    "fixture/DescribeOutboundConnections.yaml"

requestDescribePackages :: DescribePackages -> TestTree
requestDescribePackages =
  req
    "DescribePackages"
    "fixture/DescribePackages.yaml"

requestDescribeReservedInstanceOfferings :: DescribeReservedInstanceOfferings -> TestTree
requestDescribeReservedInstanceOfferings =
  req
    "DescribeReservedInstanceOfferings"
    "fixture/DescribeReservedInstanceOfferings.yaml"

requestDescribeReservedInstances :: DescribeReservedInstances -> TestTree
requestDescribeReservedInstances =
  req
    "DescribeReservedInstances"
    "fixture/DescribeReservedInstances.yaml"

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

requestGetCompatibleVersions :: GetCompatibleVersions -> TestTree
requestGetCompatibleVersions =
  req
    "GetCompatibleVersions"
    "fixture/GetCompatibleVersions.yaml"

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

requestListInstanceTypeDetails :: ListInstanceTypeDetails -> TestTree
requestListInstanceTypeDetails =
  req
    "ListInstanceTypeDetails"
    "fixture/ListInstanceTypeDetails.yaml"

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

requestListVersions :: ListVersions -> TestTree
requestListVersions =
  req
    "ListVersions"
    "fixture/ListVersions.yaml"

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

requestPurchaseReservedInstanceOffering :: PurchaseReservedInstanceOffering -> TestTree
requestPurchaseReservedInstanceOffering =
  req
    "PurchaseReservedInstanceOffering"
    "fixture/PurchaseReservedInstanceOffering.yaml"

requestRejectInboundConnection :: RejectInboundConnection -> TestTree
requestRejectInboundConnection =
  req
    "RejectInboundConnection"
    "fixture/RejectInboundConnection.yaml"

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

requestStartServiceSoftwareUpdate :: StartServiceSoftwareUpdate -> TestTree
requestStartServiceSoftwareUpdate =
  req
    "StartServiceSoftwareUpdate"
    "fixture/StartServiceSoftwareUpdate.yaml"

requestUpdateDomainConfig :: UpdateDomainConfig -> TestTree
requestUpdateDomainConfig =
  req
    "UpdateDomainConfig"
    "fixture/UpdateDomainConfig.yaml"

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

requestUpgradeDomain :: UpgradeDomain -> TestTree
requestUpgradeDomain =
  req
    "UpgradeDomain"
    "fixture/UpgradeDomain.yaml"

-- Responses

responseAcceptInboundConnection :: AcceptInboundConnectionResponse -> TestTree
responseAcceptInboundConnection =
  res
    "AcceptInboundConnectionResponse"
    "fixture/AcceptInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptInboundConnection)

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

responseCancelServiceSoftwareUpdate :: CancelServiceSoftwareUpdateResponse -> TestTree
responseCancelServiceSoftwareUpdate =
  res
    "CancelServiceSoftwareUpdateResponse"
    "fixture/CancelServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelServiceSoftwareUpdate)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

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

responseCreateVpcEndpoint :: CreateVpcEndpointResponse -> TestTree
responseCreateVpcEndpoint =
  res
    "CreateVpcEndpointResponse"
    "fixture/CreateVpcEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVpcEndpoint)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteInboundConnection :: DeleteInboundConnectionResponse -> TestTree
responseDeleteInboundConnection =
  res
    "DeleteInboundConnectionResponse"
    "fixture/DeleteInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInboundConnection)

responseDeleteOutboundConnection :: DeleteOutboundConnectionResponse -> TestTree
responseDeleteOutboundConnection =
  res
    "DeleteOutboundConnectionResponse"
    "fixture/DeleteOutboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOutboundConnection)

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

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

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

responseDescribeDomainConfig :: DescribeDomainConfigResponse -> TestTree
responseDescribeDomainConfig =
  res
    "DescribeDomainConfigResponse"
    "fixture/DescribeDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomainConfig)

responseDescribeDomains :: DescribeDomainsResponse -> TestTree
responseDescribeDomains =
  res
    "DescribeDomainsResponse"
    "fixture/DescribeDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomains)

responseDescribeInboundConnections :: DescribeInboundConnectionsResponse -> TestTree
responseDescribeInboundConnections =
  res
    "DescribeInboundConnectionsResponse"
    "fixture/DescribeInboundConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundConnections)

responseDescribeInstanceTypeLimits :: DescribeInstanceTypeLimitsResponse -> TestTree
responseDescribeInstanceTypeLimits =
  res
    "DescribeInstanceTypeLimitsResponse"
    "fixture/DescribeInstanceTypeLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceTypeLimits)

responseDescribeOutboundConnections :: DescribeOutboundConnectionsResponse -> TestTree
responseDescribeOutboundConnections =
  res
    "DescribeOutboundConnectionsResponse"
    "fixture/DescribeOutboundConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOutboundConnections)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages =
  res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackages)

responseDescribeReservedInstanceOfferings :: DescribeReservedInstanceOfferingsResponse -> TestTree
responseDescribeReservedInstanceOfferings =
  res
    "DescribeReservedInstanceOfferingsResponse"
    "fixture/DescribeReservedInstanceOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstanceOfferings)

responseDescribeReservedInstances :: DescribeReservedInstancesResponse -> TestTree
responseDescribeReservedInstances =
  res
    "DescribeReservedInstancesResponse"
    "fixture/DescribeReservedInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReservedInstances)

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

responseGetCompatibleVersions :: GetCompatibleVersionsResponse -> TestTree
responseGetCompatibleVersions =
  res
    "GetCompatibleVersionsResponse"
    "fixture/GetCompatibleVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleVersions)

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

responseListInstanceTypeDetails :: ListInstanceTypeDetailsResponse -> TestTree
responseListInstanceTypeDetails =
  res
    "ListInstanceTypeDetailsResponse"
    "fixture/ListInstanceTypeDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstanceTypeDetails)

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

responseListVersions :: ListVersionsResponse -> TestTree
responseListVersions =
  res
    "ListVersionsResponse"
    "fixture/ListVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVersions)

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

responsePurchaseReservedInstanceOffering :: PurchaseReservedInstanceOfferingResponse -> TestTree
responsePurchaseReservedInstanceOffering =
  res
    "PurchaseReservedInstanceOfferingResponse"
    "fixture/PurchaseReservedInstanceOfferingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PurchaseReservedInstanceOffering)

responseRejectInboundConnection :: RejectInboundConnectionResponse -> TestTree
responseRejectInboundConnection =
  res
    "RejectInboundConnectionResponse"
    "fixture/RejectInboundConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectInboundConnection)

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

responseStartServiceSoftwareUpdate :: StartServiceSoftwareUpdateResponse -> TestTree
responseStartServiceSoftwareUpdate =
  res
    "StartServiceSoftwareUpdateResponse"
    "fixture/StartServiceSoftwareUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartServiceSoftwareUpdate)

responseUpdateDomainConfig :: UpdateDomainConfigResponse -> TestTree
responseUpdateDomainConfig =
  res
    "UpdateDomainConfigResponse"
    "fixture/UpdateDomainConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainConfig)

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

responseUpgradeDomain :: UpgradeDomainResponse -> TestTree
responseUpgradeDomain =
  res
    "UpgradeDomainResponse"
    "fixture/UpgradeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeDomain)
