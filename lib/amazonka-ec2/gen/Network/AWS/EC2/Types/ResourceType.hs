{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResourceType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ResourceType
  = RTClientVPNEndpoint
  | RTCustomerGateway
  | RTDHCPOptions
  | RTDedicatedHost
  | RTEgressOnlyInternetGateway
  | RTElasticGpu
  | RTElasticIP
  | RTExportImageTask
  | RTExportInstanceTask
  | RTFleet
  | RTFpgaImage
  | RTHostReservation
  | RTImage
  | RTImportImageTask
  | RTImportSnapshotTask
  | RTInstance
  | RTInternetGateway
  | RTKeyPair
  | RTLaunchTemplate
  | RTLocalGatewayRouteTableVPCAssociation
  | RTNatgateway
  | RTNetworkACL
  | RTNetworkInterface
  | RTPlacementGroup
  | RTReservedInstances
  | RTRouteTable
  | RTSecurityGroup
  | RTSnapshot
  | RTSpotFleetRequest
  | RTSpotInstancesRequest
  | RTSubnet
  | RTTrafficMirrorFilter
  | RTTrafficMirrorSession
  | RTTrafficMirrorTarget
  | RTTransitGateway
  | RTTransitGatewayAttachment
  | RTTransitGatewayMulticastDomain
  | RTTransitGatewayRouteTable
  | RTVPC
  | RTVPCFlowLog
  | RTVPCPeeringConnection
  | RTVPNConnection
  | RTVPNGateway
  | RTVolume
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "client-vpn-endpoint" -> pure RTClientVPNEndpoint
      "customer-gateway" -> pure RTCustomerGateway
      "dhcp-options" -> pure RTDHCPOptions
      "dedicated-host" -> pure RTDedicatedHost
      "egress-only-internet-gateway" -> pure RTEgressOnlyInternetGateway
      "elastic-gpu" -> pure RTElasticGpu
      "elastic-ip" -> pure RTElasticIP
      "export-image-task" -> pure RTExportImageTask
      "export-instance-task" -> pure RTExportInstanceTask
      "fleet" -> pure RTFleet
      "fpga-image" -> pure RTFpgaImage
      "host-reservation" -> pure RTHostReservation
      "image" -> pure RTImage
      "import-image-task" -> pure RTImportImageTask
      "import-snapshot-task" -> pure RTImportSnapshotTask
      "instance" -> pure RTInstance
      "internet-gateway" -> pure RTInternetGateway
      "key-pair" -> pure RTKeyPair
      "launch-template" -> pure RTLaunchTemplate
      "local-gateway-route-table-vpc-association" -> pure RTLocalGatewayRouteTableVPCAssociation
      "natgateway" -> pure RTNatgateway
      "network-acl" -> pure RTNetworkACL
      "network-interface" -> pure RTNetworkInterface
      "placement-group" -> pure RTPlacementGroup
      "reserved-instances" -> pure RTReservedInstances
      "route-table" -> pure RTRouteTable
      "security-group" -> pure RTSecurityGroup
      "snapshot" -> pure RTSnapshot
      "spot-fleet-request" -> pure RTSpotFleetRequest
      "spot-instances-request" -> pure RTSpotInstancesRequest
      "subnet" -> pure RTSubnet
      "traffic-mirror-filter" -> pure RTTrafficMirrorFilter
      "traffic-mirror-session" -> pure RTTrafficMirrorSession
      "traffic-mirror-target" -> pure RTTrafficMirrorTarget
      "transit-gateway" -> pure RTTransitGateway
      "transit-gateway-attachment" -> pure RTTransitGatewayAttachment
      "transit-gateway-multicast-domain" -> pure RTTransitGatewayMulticastDomain
      "transit-gateway-route-table" -> pure RTTransitGatewayRouteTable
      "vpc" -> pure RTVPC
      "vpc-flow-log" -> pure RTVPCFlowLog
      "vpc-peering-connection" -> pure RTVPCPeeringConnection
      "vpn-connection" -> pure RTVPNConnection
      "vpn-gateway" -> pure RTVPNGateway
      "volume" -> pure RTVolume
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: client-vpn-endpoint, customer-gateway, dhcp-options, dedicated-host, egress-only-internet-gateway, elastic-gpu, elastic-ip, export-image-task, export-instance-task, fleet, fpga-image, host-reservation, image, import-image-task, import-snapshot-task, instance, internet-gateway, key-pair, launch-template, local-gateway-route-table-vpc-association, natgateway, network-acl, network-interface, placement-group, reserved-instances, route-table, security-group, snapshot, spot-fleet-request, spot-instances-request, subnet, traffic-mirror-filter, traffic-mirror-session, traffic-mirror-target, transit-gateway, transit-gateway-attachment, transit-gateway-multicast-domain, transit-gateway-route-table, vpc, vpc-flow-log, vpc-peering-connection, vpn-connection, vpn-gateway, volume"

instance ToText ResourceType where
  toText = \case
    RTClientVPNEndpoint -> "client-vpn-endpoint"
    RTCustomerGateway -> "customer-gateway"
    RTDHCPOptions -> "dhcp-options"
    RTDedicatedHost -> "dedicated-host"
    RTEgressOnlyInternetGateway -> "egress-only-internet-gateway"
    RTElasticGpu -> "elastic-gpu"
    RTElasticIP -> "elastic-ip"
    RTExportImageTask -> "export-image-task"
    RTExportInstanceTask -> "export-instance-task"
    RTFleet -> "fleet"
    RTFpgaImage -> "fpga-image"
    RTHostReservation -> "host-reservation"
    RTImage -> "image"
    RTImportImageTask -> "import-image-task"
    RTImportSnapshotTask -> "import-snapshot-task"
    RTInstance -> "instance"
    RTInternetGateway -> "internet-gateway"
    RTKeyPair -> "key-pair"
    RTLaunchTemplate -> "launch-template"
    RTLocalGatewayRouteTableVPCAssociation -> "local-gateway-route-table-vpc-association"
    RTNatgateway -> "natgateway"
    RTNetworkACL -> "network-acl"
    RTNetworkInterface -> "network-interface"
    RTPlacementGroup -> "placement-group"
    RTReservedInstances -> "reserved-instances"
    RTRouteTable -> "route-table"
    RTSecurityGroup -> "security-group"
    RTSnapshot -> "snapshot"
    RTSpotFleetRequest -> "spot-fleet-request"
    RTSpotInstancesRequest -> "spot-instances-request"
    RTSubnet -> "subnet"
    RTTrafficMirrorFilter -> "traffic-mirror-filter"
    RTTrafficMirrorSession -> "traffic-mirror-session"
    RTTrafficMirrorTarget -> "traffic-mirror-target"
    RTTransitGateway -> "transit-gateway"
    RTTransitGatewayAttachment -> "transit-gateway-attachment"
    RTTransitGatewayMulticastDomain -> "transit-gateway-multicast-domain"
    RTTransitGatewayRouteTable -> "transit-gateway-route-table"
    RTVPC -> "vpc"
    RTVPCFlowLog -> "vpc-flow-log"
    RTVPCPeeringConnection -> "vpc-peering-connection"
    RTVPNConnection -> "vpn-connection"
    RTVPNGateway -> "vpn-gateway"
    RTVolume -> "volume"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance FromXML ResourceType where
  parseXML = parseXMLText "ResourceType"
