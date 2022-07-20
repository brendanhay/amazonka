{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.NetworkManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.NetworkManager where

import Amazonka.NetworkManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.NetworkManager.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateCustomerGateway $
--             newAssociateCustomerGateway
--
--         , requestAssociateLink $
--             newAssociateLink
--
--         , requestAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeer
--
--         , requestCreateConnection $
--             newCreateConnection
--
--         , requestCreateDevice $
--             newCreateDevice
--
--         , requestCreateGlobalNetwork $
--             newCreateGlobalNetwork
--
--         , requestCreateLink $
--             newCreateLink
--
--         , requestCreateSite $
--             newCreateSite
--
--         , requestDeleteConnection $
--             newDeleteConnection
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestDeleteGlobalNetwork $
--             newDeleteGlobalNetwork
--
--         , requestDeleteLink $
--             newDeleteLink
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestDeregisterTransitGateway $
--             newDeregisterTransitGateway
--
--         , requestDescribeGlobalNetworks $
--             newDescribeGlobalNetworks
--
--         , requestDisassociateCustomerGateway $
--             newDisassociateCustomerGateway
--
--         , requestDisassociateLink $
--             newDisassociateLink
--
--         , requestDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeer
--
--         , requestGetConnections $
--             newGetConnections
--
--         , requestGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociations
--
--         , requestGetDevices $
--             newGetDevices
--
--         , requestGetLinkAssociations $
--             newGetLinkAssociations
--
--         , requestGetLinks $
--             newGetLinks
--
--         , requestGetSites $
--             newGetSites
--
--         , requestGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociations
--
--         , requestGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterTransitGateway $
--             newRegisterTransitGateway
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConnection $
--             newUpdateConnection
--
--         , requestUpdateDevice $
--             newUpdateDevice
--
--         , requestUpdateGlobalNetwork $
--             newUpdateGlobalNetwork
--
--         , requestUpdateLink $
--             newUpdateLink
--
--         , requestUpdateSite $
--             newUpdateSite
--
--           ]

--     , testGroup "response"
--         [ responseAssociateCustomerGateway $
--             newAssociateCustomerGatewayResponse
--
--         , responseAssociateLink $
--             newAssociateLinkResponse
--
--         , responseAssociateTransitGatewayConnectPeer $
--             newAssociateTransitGatewayConnectPeerResponse
--
--         , responseCreateConnection $
--             newCreateConnectionResponse
--
--         , responseCreateDevice $
--             newCreateDeviceResponse
--
--         , responseCreateGlobalNetwork $
--             newCreateGlobalNetworkResponse
--
--         , responseCreateLink $
--             newCreateLinkResponse
--
--         , responseCreateSite $
--             newCreateSiteResponse
--
--         , responseDeleteConnection $
--             newDeleteConnectionResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseDeleteGlobalNetwork $
--             newDeleteGlobalNetworkResponse
--
--         , responseDeleteLink $
--             newDeleteLinkResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseDeregisterTransitGateway $
--             newDeregisterTransitGatewayResponse
--
--         , responseDescribeGlobalNetworks $
--             newDescribeGlobalNetworksResponse
--
--         , responseDisassociateCustomerGateway $
--             newDisassociateCustomerGatewayResponse
--
--         , responseDisassociateLink $
--             newDisassociateLinkResponse
--
--         , responseDisassociateTransitGatewayConnectPeer $
--             newDisassociateTransitGatewayConnectPeerResponse
--
--         , responseGetConnections $
--             newGetConnectionsResponse
--
--         , responseGetCustomerGatewayAssociations $
--             newGetCustomerGatewayAssociationsResponse
--
--         , responseGetDevices $
--             newGetDevicesResponse
--
--         , responseGetLinkAssociations $
--             newGetLinkAssociationsResponse
--
--         , responseGetLinks $
--             newGetLinksResponse
--
--         , responseGetSites $
--             newGetSitesResponse
--
--         , responseGetTransitGatewayConnectPeerAssociations $
--             newGetTransitGatewayConnectPeerAssociationsResponse
--
--         , responseGetTransitGatewayRegistrations $
--             newGetTransitGatewayRegistrationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterTransitGateway $
--             newRegisterTransitGatewayResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConnection $
--             newUpdateConnectionResponse
--
--         , responseUpdateDevice $
--             newUpdateDeviceResponse
--
--         , responseUpdateGlobalNetwork $
--             newUpdateGlobalNetworkResponse
--
--         , responseUpdateLink $
--             newUpdateLinkResponse
--
--         , responseUpdateSite $
--             newUpdateSiteResponse
--
--           ]
--     ]

-- Requests

requestAssociateCustomerGateway :: AssociateCustomerGateway -> TestTree
requestAssociateCustomerGateway =
  req
    "AssociateCustomerGateway"
    "fixture/AssociateCustomerGateway.yaml"

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

requestCreateConnection :: CreateConnection -> TestTree
requestCreateConnection =
  req
    "CreateConnection"
    "fixture/CreateConnection.yaml"

requestCreateDevice :: CreateDevice -> TestTree
requestCreateDevice =
  req
    "CreateDevice"
    "fixture/CreateDevice.yaml"

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

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestDeleteGlobalNetwork :: DeleteGlobalNetwork -> TestTree
requestDeleteGlobalNetwork =
  req
    "DeleteGlobalNetwork"
    "fixture/DeleteGlobalNetwork.yaml"

requestDeleteLink :: DeleteLink -> TestTree
requestDeleteLink =
  req
    "DeleteLink"
    "fixture/DeleteLink.yaml"

requestDeleteSite :: DeleteSite -> TestTree
requestDeleteSite =
  req
    "DeleteSite"
    "fixture/DeleteSite.yaml"

requestDeregisterTransitGateway :: DeregisterTransitGateway -> TestTree
requestDeregisterTransitGateway =
  req
    "DeregisterTransitGateway"
    "fixture/DeregisterTransitGateway.yaml"

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

requestDisassociateLink :: DisassociateLink -> TestTree
requestDisassociateLink =
  req
    "DisassociateLink"
    "fixture/DisassociateLink.yaml"

requestDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeer -> TestTree
requestDisassociateTransitGatewayConnectPeer =
  req
    "DisassociateTransitGatewayConnectPeer"
    "fixture/DisassociateTransitGatewayConnectPeer.yaml"

requestGetConnections :: GetConnections -> TestTree
requestGetConnections =
  req
    "GetConnections"
    "fixture/GetConnections.yaml"

requestGetCustomerGatewayAssociations :: GetCustomerGatewayAssociations -> TestTree
requestGetCustomerGatewayAssociations =
  req
    "GetCustomerGatewayAssociations"
    "fixture/GetCustomerGatewayAssociations.yaml"

requestGetDevices :: GetDevices -> TestTree
requestGetDevices =
  req
    "GetDevices"
    "fixture/GetDevices.yaml"

requestGetLinkAssociations :: GetLinkAssociations -> TestTree
requestGetLinkAssociations =
  req
    "GetLinkAssociations"
    "fixture/GetLinkAssociations.yaml"

requestGetLinks :: GetLinks -> TestTree
requestGetLinks =
  req
    "GetLinks"
    "fixture/GetLinks.yaml"

requestGetSites :: GetSites -> TestTree
requestGetSites =
  req
    "GetSites"
    "fixture/GetSites.yaml"

requestGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociations -> TestTree
requestGetTransitGatewayConnectPeerAssociations =
  req
    "GetTransitGatewayConnectPeerAssociations"
    "fixture/GetTransitGatewayConnectPeerAssociations.yaml"

requestGetTransitGatewayRegistrations :: GetTransitGatewayRegistrations -> TestTree
requestGetTransitGatewayRegistrations =
  req
    "GetTransitGatewayRegistrations"
    "fixture/GetTransitGatewayRegistrations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterTransitGateway :: RegisterTransitGateway -> TestTree
requestRegisterTransitGateway =
  req
    "RegisterTransitGateway"
    "fixture/RegisterTransitGateway.yaml"

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

requestUpdateConnection :: UpdateConnection -> TestTree
requestUpdateConnection =
  req
    "UpdateConnection"
    "fixture/UpdateConnection.yaml"

requestUpdateDevice :: UpdateDevice -> TestTree
requestUpdateDevice =
  req
    "UpdateDevice"
    "fixture/UpdateDevice.yaml"

requestUpdateGlobalNetwork :: UpdateGlobalNetwork -> TestTree
requestUpdateGlobalNetwork =
  req
    "UpdateGlobalNetwork"
    "fixture/UpdateGlobalNetwork.yaml"

requestUpdateLink :: UpdateLink -> TestTree
requestUpdateLink =
  req
    "UpdateLink"
    "fixture/UpdateLink.yaml"

requestUpdateSite :: UpdateSite -> TestTree
requestUpdateSite =
  req
    "UpdateSite"
    "fixture/UpdateSite.yaml"

-- Responses

responseAssociateCustomerGateway :: AssociateCustomerGatewayResponse -> TestTree
responseAssociateCustomerGateway =
  res
    "AssociateCustomerGatewayResponse"
    "fixture/AssociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateCustomerGateway)

responseAssociateLink :: AssociateLinkResponse -> TestTree
responseAssociateLink =
  res
    "AssociateLinkResponse"
    "fixture/AssociateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLink)

responseAssociateTransitGatewayConnectPeer :: AssociateTransitGatewayConnectPeerResponse -> TestTree
responseAssociateTransitGatewayConnectPeer =
  res
    "AssociateTransitGatewayConnectPeerResponse"
    "fixture/AssociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTransitGatewayConnectPeer)

responseCreateConnection :: CreateConnectionResponse -> TestTree
responseCreateConnection =
  res
    "CreateConnectionResponse"
    "fixture/CreateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnection)

responseCreateDevice :: CreateDeviceResponse -> TestTree
responseCreateDevice =
  res
    "CreateDeviceResponse"
    "fixture/CreateDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDevice)

responseCreateGlobalNetwork :: CreateGlobalNetworkResponse -> TestTree
responseCreateGlobalNetwork =
  res
    "CreateGlobalNetworkResponse"
    "fixture/CreateGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGlobalNetwork)

responseCreateLink :: CreateLinkResponse -> TestTree
responseCreateLink =
  res
    "CreateLinkResponse"
    "fixture/CreateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLink)

responseCreateSite :: CreateSiteResponse -> TestTree
responseCreateSite =
  res
    "CreateSiteResponse"
    "fixture/CreateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSite)

responseDeleteConnection :: DeleteConnectionResponse -> TestTree
responseDeleteConnection =
  res
    "DeleteConnectionResponse"
    "fixture/DeleteConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnection)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevice)

responseDeleteGlobalNetwork :: DeleteGlobalNetworkResponse -> TestTree
responseDeleteGlobalNetwork =
  res
    "DeleteGlobalNetworkResponse"
    "fixture/DeleteGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGlobalNetwork)

responseDeleteLink :: DeleteLinkResponse -> TestTree
responseDeleteLink =
  res
    "DeleteLinkResponse"
    "fixture/DeleteLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLink)

responseDeleteSite :: DeleteSiteResponse -> TestTree
responseDeleteSite =
  res
    "DeleteSiteResponse"
    "fixture/DeleteSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSite)

responseDeregisterTransitGateway :: DeregisterTransitGatewayResponse -> TestTree
responseDeregisterTransitGateway =
  res
    "DeregisterTransitGatewayResponse"
    "fixture/DeregisterTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterTransitGateway)

responseDescribeGlobalNetworks :: DescribeGlobalNetworksResponse -> TestTree
responseDescribeGlobalNetworks =
  res
    "DescribeGlobalNetworksResponse"
    "fixture/DescribeGlobalNetworksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGlobalNetworks)

responseDisassociateCustomerGateway :: DisassociateCustomerGatewayResponse -> TestTree
responseDisassociateCustomerGateway =
  res
    "DisassociateCustomerGatewayResponse"
    "fixture/DisassociateCustomerGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCustomerGateway)

responseDisassociateLink :: DisassociateLinkResponse -> TestTree
responseDisassociateLink =
  res
    "DisassociateLinkResponse"
    "fixture/DisassociateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLink)

responseDisassociateTransitGatewayConnectPeer :: DisassociateTransitGatewayConnectPeerResponse -> TestTree
responseDisassociateTransitGatewayConnectPeer =
  res
    "DisassociateTransitGatewayConnectPeerResponse"
    "fixture/DisassociateTransitGatewayConnectPeerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTransitGatewayConnectPeer)

responseGetConnections :: GetConnectionsResponse -> TestTree
responseGetConnections =
  res
    "GetConnectionsResponse"
    "fixture/GetConnectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnections)

responseGetCustomerGatewayAssociations :: GetCustomerGatewayAssociationsResponse -> TestTree
responseGetCustomerGatewayAssociations =
  res
    "GetCustomerGatewayAssociationsResponse"
    "fixture/GetCustomerGatewayAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCustomerGatewayAssociations)

responseGetDevices :: GetDevicesResponse -> TestTree
responseGetDevices =
  res
    "GetDevicesResponse"
    "fixture/GetDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevices)

responseGetLinkAssociations :: GetLinkAssociationsResponse -> TestTree
responseGetLinkAssociations =
  res
    "GetLinkAssociationsResponse"
    "fixture/GetLinkAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLinkAssociations)

responseGetLinks :: GetLinksResponse -> TestTree
responseGetLinks =
  res
    "GetLinksResponse"
    "fixture/GetLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLinks)

responseGetSites :: GetSitesResponse -> TestTree
responseGetSites =
  res
    "GetSitesResponse"
    "fixture/GetSitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSites)

responseGetTransitGatewayConnectPeerAssociations :: GetTransitGatewayConnectPeerAssociationsResponse -> TestTree
responseGetTransitGatewayConnectPeerAssociations =
  res
    "GetTransitGatewayConnectPeerAssociationsResponse"
    "fixture/GetTransitGatewayConnectPeerAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayConnectPeerAssociations)

responseGetTransitGatewayRegistrations :: GetTransitGatewayRegistrationsResponse -> TestTree
responseGetTransitGatewayRegistrations =
  res
    "GetTransitGatewayRegistrationsResponse"
    "fixture/GetTransitGatewayRegistrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTransitGatewayRegistrations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterTransitGateway :: RegisterTransitGatewayResponse -> TestTree
responseRegisterTransitGateway =
  res
    "RegisterTransitGatewayResponse"
    "fixture/RegisterTransitGatewayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterTransitGateway)

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

responseUpdateConnection :: UpdateConnectionResponse -> TestTree
responseUpdateConnection =
  res
    "UpdateConnectionResponse"
    "fixture/UpdateConnectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnection)

responseUpdateDevice :: UpdateDeviceResponse -> TestTree
responseUpdateDevice =
  res
    "UpdateDeviceResponse"
    "fixture/UpdateDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevice)

responseUpdateGlobalNetwork :: UpdateGlobalNetworkResponse -> TestTree
responseUpdateGlobalNetwork =
  res
    "UpdateGlobalNetworkResponse"
    "fixture/UpdateGlobalNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalNetwork)

responseUpdateLink :: UpdateLinkResponse -> TestTree
responseUpdateLink =
  res
    "UpdateLinkResponse"
    "fixture/UpdateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLink)

responseUpdateSite :: UpdateSiteResponse -> TestTree
responseUpdateSite =
  res
    "UpdateSiteResponse"
    "fixture/UpdateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSite)
