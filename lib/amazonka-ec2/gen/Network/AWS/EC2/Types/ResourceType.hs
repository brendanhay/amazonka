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
        ResourceTypeClientVpnEndpoint,
        ResourceTypeCustomerGateway,
        ResourceTypeDedicatedHost,
        ResourceTypeDhcpOptions,
        ResourceTypeEgressOnlyInternetGateway,
        ResourceTypeElasticIp,
        ResourceTypeElasticGpu,
        ResourceTypeExportImageTask,
        ResourceTypeExportInstanceTask,
        ResourceTypeFleet,
        ResourceTypeFpgaImage,
        ResourceTypeHostReservation,
        ResourceTypeImage,
        ResourceTypeImportImageTask,
        ResourceTypeImportSnapshotTask,
        ResourceTypeInstance,
        ResourceTypeInternetGateway,
        ResourceTypeKeyPair,
        ResourceTypeLaunchTemplate,
        ResourceTypeLocalGatewayRouteTableVpcAssociation,
        ResourceTypeNatgateway,
        ResourceTypeNetworkAcl,
        ResourceTypeNetworkInterface,
        ResourceTypePlacementGroup,
        ResourceTypeReservedInstances,
        ResourceTypeRouteTable,
        ResourceTypeSecurityGroup,
        ResourceTypeSnapshot,
        ResourceTypeSpotFleetRequest,
        ResourceTypeSpotInstancesRequest,
        ResourceTypeSubnet,
        ResourceTypeTrafficMirrorFilter,
        ResourceTypeTrafficMirrorSession,
        ResourceTypeTrafficMirrorTarget,
        ResourceTypeTransitGateway,
        ResourceTypeTransitGatewayAttachment,
        ResourceTypeTransitGatewayMulticastDomain,
        ResourceTypeTransitGatewayRouteTable,
        ResourceTypeVolume,
        ResourceTypeVpc,
        ResourceTypeVpcPeeringConnection,
        ResourceTypeVpnConnection,
        ResourceTypeVpnGateway,
        ResourceTypeVpcFlowLog,
        fromResourceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ResourceType = ResourceType' {fromResourceType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ResourceTypeClientVpnEndpoint :: ResourceType
pattern ResourceTypeClientVpnEndpoint = ResourceType' "client-vpn-endpoint"

pattern ResourceTypeCustomerGateway :: ResourceType
pattern ResourceTypeCustomerGateway = ResourceType' "customer-gateway"

pattern ResourceTypeDedicatedHost :: ResourceType
pattern ResourceTypeDedicatedHost = ResourceType' "dedicated-host"

pattern ResourceTypeDhcpOptions :: ResourceType
pattern ResourceTypeDhcpOptions = ResourceType' "dhcp-options"

pattern ResourceTypeEgressOnlyInternetGateway :: ResourceType
pattern ResourceTypeEgressOnlyInternetGateway = ResourceType' "egress-only-internet-gateway"

pattern ResourceTypeElasticIp :: ResourceType
pattern ResourceTypeElasticIp = ResourceType' "elastic-ip"

pattern ResourceTypeElasticGpu :: ResourceType
pattern ResourceTypeElasticGpu = ResourceType' "elastic-gpu"

pattern ResourceTypeExportImageTask :: ResourceType
pattern ResourceTypeExportImageTask = ResourceType' "export-image-task"

pattern ResourceTypeExportInstanceTask :: ResourceType
pattern ResourceTypeExportInstanceTask = ResourceType' "export-instance-task"

pattern ResourceTypeFleet :: ResourceType
pattern ResourceTypeFleet = ResourceType' "fleet"

pattern ResourceTypeFpgaImage :: ResourceType
pattern ResourceTypeFpgaImage = ResourceType' "fpga-image"

pattern ResourceTypeHostReservation :: ResourceType
pattern ResourceTypeHostReservation = ResourceType' "host-reservation"

pattern ResourceTypeImage :: ResourceType
pattern ResourceTypeImage = ResourceType' "image"

pattern ResourceTypeImportImageTask :: ResourceType
pattern ResourceTypeImportImageTask = ResourceType' "import-image-task"

pattern ResourceTypeImportSnapshotTask :: ResourceType
pattern ResourceTypeImportSnapshotTask = ResourceType' "import-snapshot-task"

pattern ResourceTypeInstance :: ResourceType
pattern ResourceTypeInstance = ResourceType' "instance"

pattern ResourceTypeInternetGateway :: ResourceType
pattern ResourceTypeInternetGateway = ResourceType' "internet-gateway"

pattern ResourceTypeKeyPair :: ResourceType
pattern ResourceTypeKeyPair = ResourceType' "key-pair"

pattern ResourceTypeLaunchTemplate :: ResourceType
pattern ResourceTypeLaunchTemplate = ResourceType' "launch-template"

pattern ResourceTypeLocalGatewayRouteTableVpcAssociation :: ResourceType
pattern ResourceTypeLocalGatewayRouteTableVpcAssociation = ResourceType' "local-gateway-route-table-vpc-association"

pattern ResourceTypeNatgateway :: ResourceType
pattern ResourceTypeNatgateway = ResourceType' "natgateway"

pattern ResourceTypeNetworkAcl :: ResourceType
pattern ResourceTypeNetworkAcl = ResourceType' "network-acl"

pattern ResourceTypeNetworkInterface :: ResourceType
pattern ResourceTypeNetworkInterface = ResourceType' "network-interface"

pattern ResourceTypePlacementGroup :: ResourceType
pattern ResourceTypePlacementGroup = ResourceType' "placement-group"

pattern ResourceTypeReservedInstances :: ResourceType
pattern ResourceTypeReservedInstances = ResourceType' "reserved-instances"

pattern ResourceTypeRouteTable :: ResourceType
pattern ResourceTypeRouteTable = ResourceType' "route-table"

pattern ResourceTypeSecurityGroup :: ResourceType
pattern ResourceTypeSecurityGroup = ResourceType' "security-group"

pattern ResourceTypeSnapshot :: ResourceType
pattern ResourceTypeSnapshot = ResourceType' "snapshot"

pattern ResourceTypeSpotFleetRequest :: ResourceType
pattern ResourceTypeSpotFleetRequest = ResourceType' "spot-fleet-request"

pattern ResourceTypeSpotInstancesRequest :: ResourceType
pattern ResourceTypeSpotInstancesRequest = ResourceType' "spot-instances-request"

pattern ResourceTypeSubnet :: ResourceType
pattern ResourceTypeSubnet = ResourceType' "subnet"

pattern ResourceTypeTrafficMirrorFilter :: ResourceType
pattern ResourceTypeTrafficMirrorFilter = ResourceType' "traffic-mirror-filter"

pattern ResourceTypeTrafficMirrorSession :: ResourceType
pattern ResourceTypeTrafficMirrorSession = ResourceType' "traffic-mirror-session"

pattern ResourceTypeTrafficMirrorTarget :: ResourceType
pattern ResourceTypeTrafficMirrorTarget = ResourceType' "traffic-mirror-target"

pattern ResourceTypeTransitGateway :: ResourceType
pattern ResourceTypeTransitGateway = ResourceType' "transit-gateway"

pattern ResourceTypeTransitGatewayAttachment :: ResourceType
pattern ResourceTypeTransitGatewayAttachment = ResourceType' "transit-gateway-attachment"

pattern ResourceTypeTransitGatewayMulticastDomain :: ResourceType
pattern ResourceTypeTransitGatewayMulticastDomain = ResourceType' "transit-gateway-multicast-domain"

pattern ResourceTypeTransitGatewayRouteTable :: ResourceType
pattern ResourceTypeTransitGatewayRouteTable = ResourceType' "transit-gateway-route-table"

pattern ResourceTypeVolume :: ResourceType
pattern ResourceTypeVolume = ResourceType' "volume"

pattern ResourceTypeVpc :: ResourceType
pattern ResourceTypeVpc = ResourceType' "vpc"

pattern ResourceTypeVpcPeeringConnection :: ResourceType
pattern ResourceTypeVpcPeeringConnection = ResourceType' "vpc-peering-connection"

pattern ResourceTypeVpnConnection :: ResourceType
pattern ResourceTypeVpnConnection = ResourceType' "vpn-connection"

pattern ResourceTypeVpnGateway :: ResourceType
pattern ResourceTypeVpnGateway = ResourceType' "vpn-gateway"

pattern ResourceTypeVpcFlowLog :: ResourceType
pattern ResourceTypeVpcFlowLog = ResourceType' "vpc-flow-log"

{-# COMPLETE
  ResourceTypeClientVpnEndpoint,
  ResourceTypeCustomerGateway,
  ResourceTypeDedicatedHost,
  ResourceTypeDhcpOptions,
  ResourceTypeEgressOnlyInternetGateway,
  ResourceTypeElasticIp,
  ResourceTypeElasticGpu,
  ResourceTypeExportImageTask,
  ResourceTypeExportInstanceTask,
  ResourceTypeFleet,
  ResourceTypeFpgaImage,
  ResourceTypeHostReservation,
  ResourceTypeImage,
  ResourceTypeImportImageTask,
  ResourceTypeImportSnapshotTask,
  ResourceTypeInstance,
  ResourceTypeInternetGateway,
  ResourceTypeKeyPair,
  ResourceTypeLaunchTemplate,
  ResourceTypeLocalGatewayRouteTableVpcAssociation,
  ResourceTypeNatgateway,
  ResourceTypeNetworkAcl,
  ResourceTypeNetworkInterface,
  ResourceTypePlacementGroup,
  ResourceTypeReservedInstances,
  ResourceTypeRouteTable,
  ResourceTypeSecurityGroup,
  ResourceTypeSnapshot,
  ResourceTypeSpotFleetRequest,
  ResourceTypeSpotInstancesRequest,
  ResourceTypeSubnet,
  ResourceTypeTrafficMirrorFilter,
  ResourceTypeTrafficMirrorSession,
  ResourceTypeTrafficMirrorTarget,
  ResourceTypeTransitGateway,
  ResourceTypeTransitGatewayAttachment,
  ResourceTypeTransitGatewayMulticastDomain,
  ResourceTypeTransitGatewayRouteTable,
  ResourceTypeVolume,
  ResourceTypeVpc,
  ResourceTypeVpcPeeringConnection,
  ResourceTypeVpnConnection,
  ResourceTypeVpnGateway,
  ResourceTypeVpcFlowLog,
  ResourceType'
  #-}
