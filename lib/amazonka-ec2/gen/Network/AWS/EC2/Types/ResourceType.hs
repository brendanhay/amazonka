{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResourceType
  ( ResourceType
      ( ResourceType',
        RTClientVPNEndpoint,
        RTCustomerGateway,
        RTDedicatedHost,
        RTDHCPOptions,
        RTEgressOnlyInternetGateway,
        RTElasticIP,
        RTElasticGpu,
        RTExportImageTask,
        RTExportInstanceTask,
        RTFleet,
        RTFpgaImage,
        RTHostReservation,
        RTImage,
        RTImportImageTask,
        RTImportSnapshotTask,
        RTInstance,
        RTInternetGateway,
        RTKeyPair,
        RTLaunchTemplate,
        RTLocalGatewayRouteTableVPCAssociation,
        RTNatgateway,
        RTNetworkACL,
        RTNetworkInterface,
        RTPlacementGroup,
        RTReservedInstances,
        RTRouteTable,
        RTSecurityGroup,
        RTSnapshot,
        RTSpotFleetRequest,
        RTSpotInstancesRequest,
        RTSubnet,
        RTTrafficMirrorFilter,
        RTTrafficMirrorSession,
        RTTrafficMirrorTarget,
        RTTransitGateway,
        RTTransitGatewayAttachment,
        RTTransitGatewayMulticastDomain,
        RTTransitGatewayRouteTable,
        RTVolume,
        RTVPC,
        RTVPCPeeringConnection,
        RTVPNConnection,
        RTVPNGateway,
        RTVPCFlowLog
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceType = ResourceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RTClientVPNEndpoint :: ResourceType
pattern RTClientVPNEndpoint = ResourceType' "client-vpn-endpoint"

pattern RTCustomerGateway :: ResourceType
pattern RTCustomerGateway = ResourceType' "customer-gateway"

pattern RTDedicatedHost :: ResourceType
pattern RTDedicatedHost = ResourceType' "dedicated-host"

pattern RTDHCPOptions :: ResourceType
pattern RTDHCPOptions = ResourceType' "dhcp-options"

pattern RTEgressOnlyInternetGateway :: ResourceType
pattern RTEgressOnlyInternetGateway = ResourceType' "egress-only-internet-gateway"

pattern RTElasticIP :: ResourceType
pattern RTElasticIP = ResourceType' "elastic-ip"

pattern RTElasticGpu :: ResourceType
pattern RTElasticGpu = ResourceType' "elastic-gpu"

pattern RTExportImageTask :: ResourceType
pattern RTExportImageTask = ResourceType' "export-image-task"

pattern RTExportInstanceTask :: ResourceType
pattern RTExportInstanceTask = ResourceType' "export-instance-task"

pattern RTFleet :: ResourceType
pattern RTFleet = ResourceType' "fleet"

pattern RTFpgaImage :: ResourceType
pattern RTFpgaImage = ResourceType' "fpga-image"

pattern RTHostReservation :: ResourceType
pattern RTHostReservation = ResourceType' "host-reservation"

pattern RTImage :: ResourceType
pattern RTImage = ResourceType' "image"

pattern RTImportImageTask :: ResourceType
pattern RTImportImageTask = ResourceType' "import-image-task"

pattern RTImportSnapshotTask :: ResourceType
pattern RTImportSnapshotTask = ResourceType' "import-snapshot-task"

pattern RTInstance :: ResourceType
pattern RTInstance = ResourceType' "instance"

pattern RTInternetGateway :: ResourceType
pattern RTInternetGateway = ResourceType' "internet-gateway"

pattern RTKeyPair :: ResourceType
pattern RTKeyPair = ResourceType' "key-pair"

pattern RTLaunchTemplate :: ResourceType
pattern RTLaunchTemplate = ResourceType' "launch-template"

pattern RTLocalGatewayRouteTableVPCAssociation :: ResourceType
pattern RTLocalGatewayRouteTableVPCAssociation = ResourceType' "local-gateway-route-table-vpc-association"

pattern RTNatgateway :: ResourceType
pattern RTNatgateway = ResourceType' "natgateway"

pattern RTNetworkACL :: ResourceType
pattern RTNetworkACL = ResourceType' "network-acl"

pattern RTNetworkInterface :: ResourceType
pattern RTNetworkInterface = ResourceType' "network-interface"

pattern RTPlacementGroup :: ResourceType
pattern RTPlacementGroup = ResourceType' "placement-group"

pattern RTReservedInstances :: ResourceType
pattern RTReservedInstances = ResourceType' "reserved-instances"

pattern RTRouteTable :: ResourceType
pattern RTRouteTable = ResourceType' "route-table"

pattern RTSecurityGroup :: ResourceType
pattern RTSecurityGroup = ResourceType' "security-group"

pattern RTSnapshot :: ResourceType
pattern RTSnapshot = ResourceType' "snapshot"

pattern RTSpotFleetRequest :: ResourceType
pattern RTSpotFleetRequest = ResourceType' "spot-fleet-request"

pattern RTSpotInstancesRequest :: ResourceType
pattern RTSpotInstancesRequest = ResourceType' "spot-instances-request"

pattern RTSubnet :: ResourceType
pattern RTSubnet = ResourceType' "subnet"

pattern RTTrafficMirrorFilter :: ResourceType
pattern RTTrafficMirrorFilter = ResourceType' "traffic-mirror-filter"

pattern RTTrafficMirrorSession :: ResourceType
pattern RTTrafficMirrorSession = ResourceType' "traffic-mirror-session"

pattern RTTrafficMirrorTarget :: ResourceType
pattern RTTrafficMirrorTarget = ResourceType' "traffic-mirror-target"

pattern RTTransitGateway :: ResourceType
pattern RTTransitGateway = ResourceType' "transit-gateway"

pattern RTTransitGatewayAttachment :: ResourceType
pattern RTTransitGatewayAttachment = ResourceType' "transit-gateway-attachment"

pattern RTTransitGatewayMulticastDomain :: ResourceType
pattern RTTransitGatewayMulticastDomain = ResourceType' "transit-gateway-multicast-domain"

pattern RTTransitGatewayRouteTable :: ResourceType
pattern RTTransitGatewayRouteTable = ResourceType' "transit-gateway-route-table"

pattern RTVolume :: ResourceType
pattern RTVolume = ResourceType' "volume"

pattern RTVPC :: ResourceType
pattern RTVPC = ResourceType' "vpc"

pattern RTVPCPeeringConnection :: ResourceType
pattern RTVPCPeeringConnection = ResourceType' "vpc-peering-connection"

pattern RTVPNConnection :: ResourceType
pattern RTVPNConnection = ResourceType' "vpn-connection"

pattern RTVPNGateway :: ResourceType
pattern RTVPNGateway = ResourceType' "vpn-gateway"

pattern RTVPCFlowLog :: ResourceType
pattern RTVPCFlowLog = ResourceType' "vpc-flow-log"

{-# COMPLETE
  RTClientVPNEndpoint,
  RTCustomerGateway,
  RTDedicatedHost,
  RTDHCPOptions,
  RTEgressOnlyInternetGateway,
  RTElasticIP,
  RTElasticGpu,
  RTExportImageTask,
  RTExportInstanceTask,
  RTFleet,
  RTFpgaImage,
  RTHostReservation,
  RTImage,
  RTImportImageTask,
  RTImportSnapshotTask,
  RTInstance,
  RTInternetGateway,
  RTKeyPair,
  RTLaunchTemplate,
  RTLocalGatewayRouteTableVPCAssociation,
  RTNatgateway,
  RTNetworkACL,
  RTNetworkInterface,
  RTPlacementGroup,
  RTReservedInstances,
  RTRouteTable,
  RTSecurityGroup,
  RTSnapshot,
  RTSpotFleetRequest,
  RTSpotInstancesRequest,
  RTSubnet,
  RTTrafficMirrorFilter,
  RTTrafficMirrorSession,
  RTTrafficMirrorTarget,
  RTTransitGateway,
  RTTransitGatewayAttachment,
  RTTransitGatewayMulticastDomain,
  RTTransitGatewayRouteTable,
  RTVolume,
  RTVPC,
  RTVPCPeeringConnection,
  RTVPNConnection,
  RTVPNGateway,
  RTVPCFlowLog,
  ResourceType'
  #-}
