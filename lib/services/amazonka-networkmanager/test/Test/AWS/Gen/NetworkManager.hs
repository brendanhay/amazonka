{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.NetworkManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.NetworkManager where

import Data.Proxy
import Network.AWS.NetworkManager
import Test.AWS.Fixture
import Test.AWS.NetworkManager.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetLinkAssociations $
--             newGetLinkAssociations
--
--         , requestAssociateLink $
--             newAssociateLink
--
--         , requestAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeer
--
--         , requestCreateSite $
--             newCreateSite
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestDeregisterTransitGateway $
--             newDeregisterTransitGateway
--
--         , requestGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociations
--
--         , requestUpdateSite $
--             newUpdateSite
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDisassociateLink $
--             newDisassociateLink
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestGetDevices $
--             newGetDevices
--
--         , requestGetLinks $
--             newGetLinks
--
--         , requestDescribeGlobalNetworks $
--             newDescribeGlobalNetworks
--
--         , requestDisassociateCustomerGateway $
--             newDisassociateCustomerGateway
--
--         , requestDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeer
--
--         , requestCreateGlobalNetwork $
--             newCreateGlobalNetwork
--
--         , requestCreateLink $
--             newCreateLink
--
--         , requestDeleteGlobalNetwork $
--             newDeleteGlobalNetwork
--
--         , requestUpdateGlobalNetwork $
--             newUpdateGlobalNetwork
--
--         , requestCreateDevice $
--             newCreateDevice
--
--         , requestAssociateCustomerGateway $
--             newAssociateCustomerGateway
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociations
--
--         , requestGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrations
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetSites $
--             newGetSites
--
--         , requestRegisterTransitGateway $
--             newRegisterTransitGateway
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestDeleteLink $
--             newDeleteLink
--
--         , requestUpdateLink $
--             newUpdateLink
--
--           ]

--     , testGroup "response"
--         [ responseGetLinkAssociations $
--             newGetLinkAssociationsResponse
--
--         , responseAssociateLink $
--             newAssociateLinkResponse
--
--         , responseAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeerResponse
--
--         , responseCreateSite $
--             newCreateSiteResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseDeregisterTransitGateway $
--             newDeregisterTransitGatewayResponse
--
--         , responseGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociationsResponse
--
--         , responseUpdateSite $
--             newUpdateSiteResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDisassociateLink $
--             newDisassociateLinkResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseGetDevices $
--             newGetDevicesResponse
--
--         , responseGetLinks $
--             newGetLinksResponse
--
--         , responseDescribeGlobalNetworks $
--             newDescribeGlobalNetworksResponse
--
--         , responseDisassociateCustomerGateway $
--             newDisassociateCustomerGatewayResponse
--
--         , responseDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeerResponse
--
--         , responseCreateGlobalNetwork $
--             newCreateGlobalNetworkResponse
--
--         , responseCreateLink $
--             newCreateLinkResponse
--
--         , responseDeleteGlobalNetwork $
--             newDeleteGlobalNetworkResponse
--
--         , responseUpdateGlobalNetwork $
--             newUpdateGlobalNetworkResponse
--
--         , responseCreateDevice $
--             newCreateDeviceResponse
--
--         , responseAssociateCustomerGateway $
--             newAssociateCustomerGatewayResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociationsResponse
--
--         , responseGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrationsResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetSites $
--             newGetSitesResponse
--
--         , responseRegisterTransitGateway $
--             newRegisterTransitGatewayResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseDeleteLink $
--             newDeleteLinkResponse
--
--         , responseUpdateLink $
--             newUpdateLinkResponse
--
--           ]
--     ]

-- Requests

requestGetLinkAssociations :: GetLinkAssociations -> TestTree
requestGetLinkAssociations =
  req
    "GetLinkAssociations"
    "fixture/GetLinkAssociations.yaml"

requestAssociateLink :: AssociateLink -> TestTree
requestAssociateLink =
  req
    "AssociateLink"
    "fixture/AssociateLink.yaml"

requestAssociateTransitGatewayConnectPeer :: AssociateTransitGatewayConnectPeer -> TestTree
requestAssociateTransitGatewayConnectPeer =
  req
    "AssociateTransitGatewayConnectPeer"
    "fixture/AssociateTransitGatewayConnectPeer.yaml"

requestCreateSite :: CreateSite -> TestTree
requestCreateSite =
  req
    "CreateSite"
    "fixture/CreateSite.yaml"

requestDeleteConnection :: DeleteConnection -> TestTree
requestDeleteConnection =
  req
    "DeleteConnection"
    "fixture/DeleteConnection.yaml"

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestDeregisterTransitGateway :: DeregisterTransitGateway -> TestTree
requestDeregisterTransitGateway =
  req
    "DeregisterTransitGateway"
    "fixture/DeregisterTransitGateway.yaml"

requestGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociations -> TestTree
requestGetTransitGatewayConnectPeerAssociations =
  req
    "GetTransitGatewayConnectPeerAssociations"
    "fixture/GetTransitGatewayConnectPeerAssociations.yaml"

requestUpdateSite :: UpdateSite -> TestTree
requestUpdateSite =
  req
    "UpdateSite"
    "fixture/UpdateSite.yaml"

requestDeleteSite :: DeleteSite -> TestTree
requestDeleteSite =
  req
    "DeleteSite"
    "fixture/DeleteSite.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDisassociateLink :: DisassociateLink -> TestTree
requestDisassociateLink =
  req
    "DisassociateLink"
    "fixture/DisassociateLink.yaml"

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestGetDevices :: GetDevices -> TestTree
requestGetDevices =
  req
    "GetDevices"
    "fixture/GetDevices.yaml"

requestGetLinks :: GetLinks -> TestTree
requestGetLinks =
  req
    "GetLinks"
    "fixture/GetLinks.yaml"

requestDescribeGlobalNetworks :: DescribeGlobalNetworks -> TestTree
requestDescribeGlobalNetworks =
  req
    "DescribeGlobalNetworks"
    "fixture/DescribeGlobalNetworks.yaml"

requestDisassociateCustomerGateway :: DisassociateCustomerGateway -> TestTree
requestDisassociateCustomerGateway =
  req
    "DisassociateCustomerGateway"
    "fixture/DisassociateCustomerGateway.yaml"

requestDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeer -> TestTree
requestDisassociateTransitGatewayConnectPeer =
  req
    "DisassociateTransitGatewayConnectPeer"
    "fixture/DisassociateTransitGatewayConnectPeer.yaml"

requestCreateGlobalNetwork :: CreateGlobalNetwork -> TestTree
requestCreateGlobalNetwork =
  req
    "CreateGlobalNetwork"
    "fixture/CreateGlobalNetwork.yaml"

requestCreateLink :: CreateLink -> TestTree
requestCreateLink =
  req
    "CreateLink"
    "fixture/CreateLink.yaml"

requestDeleteGlobalNetwork :: DeleteGlobalNetwork -> TestTree
requestDeleteGlobalNetwork =
  req
    "DeleteGlobalNetwork"
    "fixture/DeleteGlobalNetwork.yaml"

requestUpdateGlobalNetwork :: UpdateGlobalNetwork -> TestTree
requestUpdateGlobalNetwork =
  req
    "UpdateGlobalNetwork"
    "fixture/UpdateGlobalNetwork.yaml"

requestCreateDevice :: CreateDevice -> TestTree
requestCreateDevice =
  req
    "CreateDevice"
    "fixture/CreateDevice.yaml"

requestAssociateCustomerGateway :: AssociateCustomerGateway -> TestTree
requestAssociateCustomerGateway =
  req
    "AssociateCustomerGateway"
    "fixture/AssociateCustomerGateway.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetCustomerGatewayAssociations :: GetCustomerGatewayAssociations -> TestTree
requestGetCustomerGatewayAssociations =
  req
    "GetCustomerGatewayAssociations"
    "fixture/GetCustomerGatewayAssociations.yaml"

requestGetTransitGatewayRegistrations :: GetTransitGatewayRegistrations -> TestTree
requestGetTransitGatewayRegistrations =
  req
    "GetTransitGatewayRegistrations"
    "fixture/GetTransitGatewayRegistrations.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetSites :: GetSites -> TestTree
requestGetSites =
  req
    "GetSites"
    "fixture/GetSites.yaml"

requestRegisterTransitGateway :: RegisterTransitGateway -> TestTree
requestRegisterTransitGateway =
  req
    "RegisterTransitGateway"
    "fixture/RegisterTransitGateway.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestDeleteLink :: DeleteLink -> TestTree
requestDeleteLink =
  req
    "DeleteLink"
    "fixture/DeleteLink.yaml"

requestUpdateLink :: UpdateLink -> TestTree
requestUpdateLink =
  req
    "UpdateLink"
    "fixture/UpdateLink.yaml"

-- Responses

responseGetLinkAssociations :: GetLinkAssociationsResponse -> TestTree
responseGetLinkAssociations =
  res
    "GetLinkAssociationsResponse"
    "fixture/GetLinkAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLinkAssociations)

responseAssociateLink :: AssociateLinkResponse -> TestTree
responseAssociateLink =
  res
    "AssociateLinkResponse"
    "fixture/AssociateLinkResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateLink)

responseAssociateTransitGatewayConnectPeer :: AssociateTransitGatewayConnectPeerResponse -> TestTree
responseAssociateTransitGatewayConnectPeer =
  res
    "AssociateTransitGatewayConnectPeerResponse"
    "fixture/AssociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTransitGatewayConnectPeer)

responseCreateSite :: CreateSiteResponse -> TestTree
responseCreateSite =
  res
    "CreateSiteResponse"
    "fixture/CreateSiteResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSite)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnection)

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnection)

responseDeregisterTransitGateway :: DeregisterTransitGatewayResponse -> TestTree
responseDeregisterTransitGateway =
  res
    "DeregisterTransitGatewayResponse"
    "fixture/DeregisterTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterTransitGateway)

responseGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociationsResponse -> TestTree
responseGetTransitGatewayConnectPeerAssociations =
  res
    "GetTransitGatewayConnectPeerAssociationsResponse"
    "fixture/GetTransitGatewayConnectPeerAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayConnectPeerAssociations)

responseUpdateSite :: UpdateSiteResponse -> TestTree
responseUpdateSite =
  res
    "UpdateSiteResponse"
    "fixture/UpdateSiteResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSite)

responseDeleteSite :: DeleteSiteResponse -> TestTree
responseDeleteSite =
  res
    "DeleteSiteResponse"
    "fixture/DeleteSiteResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSite)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDisassociateLink :: DisassociateLinkResponse -> TestTree
responseDisassociateLink =
  res
    "DisassociateLinkResponse"
    "fixture/DisassociateLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateLink)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnection)

responseGetDevices :: GetDevicesResponse -> TestTree
responseGetDevices =
  res
    "GetDevicesResponse"
    "fixture/GetDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDevices)

responseGetLinks :: GetLinksResponse -> TestTree
responseGetLinks =
  res
    "GetLinksResponse"
    "fixture/GetLinksResponse.proto"
    defaultService
    (Proxy :: Proxy GetLinks)

responseDescribeGlobalNetworks :: DescribeGlobalNetworksResponse -> TestTree
responseDescribeGlobalNetworks =
  res
    "DescribeGlobalNetworksResponse"
    "fixture/DescribeGlobalNetworksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGlobalNetworks)

responseDisassociateCustomerGateway :: DisassociateCustomerGatewayResponse -> TestTree
responseDisassociateCustomerGateway =
  res
    "DisassociateCustomerGatewayResponse"
    "fixture/DisassociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateCustomerGateway)

responseDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeerResponse -> TestTree
responseDisassociateTransitGatewayConnectPeer =
  res
    "DisassociateTransitGatewayConnectPeerResponse"
    "fixture/DisassociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTransitGatewayConnectPeer)

responseCreateGlobalNetwork :: CreateGlobalNetworkResponse -> TestTree
responseCreateGlobalNetwork =
  res
    "CreateGlobalNetworkResponse"
    "fixture/CreateGlobalNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGlobalNetwork)

responseCreateLink :: CreateLinkResponse -> TestTree
responseCreateLink =
  res
    "CreateLinkResponse"
    "fixture/CreateLinkResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLink)

responseDeleteGlobalNetwork :: DeleteGlobalNetworkResponse -> TestTree
responseDeleteGlobalNetwork =
  res
    "DeleteGlobalNetworkResponse"
    "fixture/DeleteGlobalNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGlobalNetwork)

responseUpdateGlobalNetwork :: UpdateGlobalNetworkResponse -> TestTree
responseUpdateGlobalNetwork =
  res
    "UpdateGlobalNetworkResponse"
    "fixture/UpdateGlobalNetworkResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGlobalNetwork)

responseCreateDevice :: CreateDeviceResponse -> TestTree
responseCreateDevice =
  res
    "CreateDeviceResponse"
    "fixture/CreateDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDevice)

responseAssociateCustomerGateway :: AssociateCustomerGatewayResponse -> TestTree
responseAssociateCustomerGateway =
  res
    "AssociateCustomerGatewayResponse"
    "fixture/AssociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateCustomerGateway)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetCustomerGatewayAssociations :: GetCustomerGatewayAssociationsResponse -> TestTree
responseGetCustomerGatewayAssociations =
  res
    "GetCustomerGatewayAssociationsResponse"
    "fixture/GetCustomerGatewayAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCustomerGatewayAssociations)

responseGetTransitGatewayRegistrations :: GetTransitGatewayRegistrationsResponse -> TestTree
responseGetTransitGatewayRegistrations =
  res
    "GetTransitGatewayRegistrationsResponse"
    "fixture/GetTransitGatewayRegistrationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetTransitGatewayRegistrations)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnections)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetSites :: GetSitesResponse -> TestTree
responseGetSites =
  res
    "GetSitesResponse"
    "fixture/GetSitesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSites)

responseRegisterTransitGateway :: RegisterTransitGatewayResponse -> TestTree
responseRegisterTransitGateway =
  res
    "RegisterTransitGatewayResponse"
    "fixture/RegisterTransitGatewayResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterTransitGateway)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevice)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevice)

responseDeleteLink :: DeleteLinkResponse -> TestTree
responseDeleteLink =
  res
    "DeleteLinkResponse"
    "fixture/DeleteLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLink)

responseUpdateLink :: UpdateLinkResponse -> TestTree
responseUpdateLink =
  res
    "UpdateLinkResponse"
    "fixture/UpdateLinkResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLink)
