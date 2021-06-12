{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_Client_vpn_endpoint,
        ResourceType_Customer_gateway,
        ResourceType_Dedicated_host,
        ResourceType_Dhcp_options,
        ResourceType_Egress_only_internet_gateway,
        ResourceType_Elastic_gpu,
        ResourceType_Elastic_ip,
        ResourceType_Export_image_task,
        ResourceType_Export_instance_task,
        ResourceType_Fleet,
        ResourceType_Fpga_image,
        ResourceType_Host_reservation,
        ResourceType_Image,
        ResourceType_Import_image_task,
        ResourceType_Import_snapshot_task,
        ResourceType_Instance,
        ResourceType_Internet_gateway,
        ResourceType_Key_pair,
        ResourceType_Launch_template,
        ResourceType_Local_gateway_route_table_vpc_association,
        ResourceType_Natgateway,
        ResourceType_Network_acl,
        ResourceType_Network_insights_analysis,
        ResourceType_Network_insights_path,
        ResourceType_Network_interface,
        ResourceType_Placement_group,
        ResourceType_Reserved_instances,
        ResourceType_Route_table,
        ResourceType_Security_group,
        ResourceType_Snapshot,
        ResourceType_Spot_fleet_request,
        ResourceType_Spot_instances_request,
        ResourceType_Subnet,
        ResourceType_Traffic_mirror_filter,
        ResourceType_Traffic_mirror_session,
        ResourceType_Traffic_mirror_target,
        ResourceType_Transit_gateway,
        ResourceType_Transit_gateway_attachment,
        ResourceType_Transit_gateway_connect_peer,
        ResourceType_Transit_gateway_multicast_domain,
        ResourceType_Transit_gateway_route_table,
        ResourceType_Volume,
        ResourceType_Vpc,
        ResourceType_Vpc_flow_log,
        ResourceType_Vpc_peering_connection,
        ResourceType_Vpn_connection,
        ResourceType_Vpn_gateway
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ResourceType = ResourceType'
  { fromResourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ResourceType_Client_vpn_endpoint :: ResourceType
pattern ResourceType_Client_vpn_endpoint = ResourceType' "client-vpn-endpoint"

pattern ResourceType_Customer_gateway :: ResourceType
pattern ResourceType_Customer_gateway = ResourceType' "customer-gateway"

pattern ResourceType_Dedicated_host :: ResourceType
pattern ResourceType_Dedicated_host = ResourceType' "dedicated-host"

pattern ResourceType_Dhcp_options :: ResourceType
pattern ResourceType_Dhcp_options = ResourceType' "dhcp-options"

pattern ResourceType_Egress_only_internet_gateway :: ResourceType
pattern ResourceType_Egress_only_internet_gateway = ResourceType' "egress-only-internet-gateway"

pattern ResourceType_Elastic_gpu :: ResourceType
pattern ResourceType_Elastic_gpu = ResourceType' "elastic-gpu"

pattern ResourceType_Elastic_ip :: ResourceType
pattern ResourceType_Elastic_ip = ResourceType' "elastic-ip"

pattern ResourceType_Export_image_task :: ResourceType
pattern ResourceType_Export_image_task = ResourceType' "export-image-task"

pattern ResourceType_Export_instance_task :: ResourceType
pattern ResourceType_Export_instance_task = ResourceType' "export-instance-task"

pattern ResourceType_Fleet :: ResourceType
pattern ResourceType_Fleet = ResourceType' "fleet"

pattern ResourceType_Fpga_image :: ResourceType
pattern ResourceType_Fpga_image = ResourceType' "fpga-image"

pattern ResourceType_Host_reservation :: ResourceType
pattern ResourceType_Host_reservation = ResourceType' "host-reservation"

pattern ResourceType_Image :: ResourceType
pattern ResourceType_Image = ResourceType' "image"

pattern ResourceType_Import_image_task :: ResourceType
pattern ResourceType_Import_image_task = ResourceType' "import-image-task"

pattern ResourceType_Import_snapshot_task :: ResourceType
pattern ResourceType_Import_snapshot_task = ResourceType' "import-snapshot-task"

pattern ResourceType_Instance :: ResourceType
pattern ResourceType_Instance = ResourceType' "instance"

pattern ResourceType_Internet_gateway :: ResourceType
pattern ResourceType_Internet_gateway = ResourceType' "internet-gateway"

pattern ResourceType_Key_pair :: ResourceType
pattern ResourceType_Key_pair = ResourceType' "key-pair"

pattern ResourceType_Launch_template :: ResourceType
pattern ResourceType_Launch_template = ResourceType' "launch-template"

pattern ResourceType_Local_gateway_route_table_vpc_association :: ResourceType
pattern ResourceType_Local_gateway_route_table_vpc_association = ResourceType' "local-gateway-route-table-vpc-association"

pattern ResourceType_Natgateway :: ResourceType
pattern ResourceType_Natgateway = ResourceType' "natgateway"

pattern ResourceType_Network_acl :: ResourceType
pattern ResourceType_Network_acl = ResourceType' "network-acl"

pattern ResourceType_Network_insights_analysis :: ResourceType
pattern ResourceType_Network_insights_analysis = ResourceType' "network-insights-analysis"

pattern ResourceType_Network_insights_path :: ResourceType
pattern ResourceType_Network_insights_path = ResourceType' "network-insights-path"

pattern ResourceType_Network_interface :: ResourceType
pattern ResourceType_Network_interface = ResourceType' "network-interface"

pattern ResourceType_Placement_group :: ResourceType
pattern ResourceType_Placement_group = ResourceType' "placement-group"

pattern ResourceType_Reserved_instances :: ResourceType
pattern ResourceType_Reserved_instances = ResourceType' "reserved-instances"

pattern ResourceType_Route_table :: ResourceType
pattern ResourceType_Route_table = ResourceType' "route-table"

pattern ResourceType_Security_group :: ResourceType
pattern ResourceType_Security_group = ResourceType' "security-group"

pattern ResourceType_Snapshot :: ResourceType
pattern ResourceType_Snapshot = ResourceType' "snapshot"

pattern ResourceType_Spot_fleet_request :: ResourceType
pattern ResourceType_Spot_fleet_request = ResourceType' "spot-fleet-request"

pattern ResourceType_Spot_instances_request :: ResourceType
pattern ResourceType_Spot_instances_request = ResourceType' "spot-instances-request"

pattern ResourceType_Subnet :: ResourceType
pattern ResourceType_Subnet = ResourceType' "subnet"

pattern ResourceType_Traffic_mirror_filter :: ResourceType
pattern ResourceType_Traffic_mirror_filter = ResourceType' "traffic-mirror-filter"

pattern ResourceType_Traffic_mirror_session :: ResourceType
pattern ResourceType_Traffic_mirror_session = ResourceType' "traffic-mirror-session"

pattern ResourceType_Traffic_mirror_target :: ResourceType
pattern ResourceType_Traffic_mirror_target = ResourceType' "traffic-mirror-target"

pattern ResourceType_Transit_gateway :: ResourceType
pattern ResourceType_Transit_gateway = ResourceType' "transit-gateway"

pattern ResourceType_Transit_gateway_attachment :: ResourceType
pattern ResourceType_Transit_gateway_attachment = ResourceType' "transit-gateway-attachment"

pattern ResourceType_Transit_gateway_connect_peer :: ResourceType
pattern ResourceType_Transit_gateway_connect_peer = ResourceType' "transit-gateway-connect-peer"

pattern ResourceType_Transit_gateway_multicast_domain :: ResourceType
pattern ResourceType_Transit_gateway_multicast_domain = ResourceType' "transit-gateway-multicast-domain"

pattern ResourceType_Transit_gateway_route_table :: ResourceType
pattern ResourceType_Transit_gateway_route_table = ResourceType' "transit-gateway-route-table"

pattern ResourceType_Volume :: ResourceType
pattern ResourceType_Volume = ResourceType' "volume"

pattern ResourceType_Vpc :: ResourceType
pattern ResourceType_Vpc = ResourceType' "vpc"

pattern ResourceType_Vpc_flow_log :: ResourceType
pattern ResourceType_Vpc_flow_log = ResourceType' "vpc-flow-log"

pattern ResourceType_Vpc_peering_connection :: ResourceType
pattern ResourceType_Vpc_peering_connection = ResourceType' "vpc-peering-connection"

pattern ResourceType_Vpn_connection :: ResourceType
pattern ResourceType_Vpn_connection = ResourceType' "vpn-connection"

pattern ResourceType_Vpn_gateway :: ResourceType
pattern ResourceType_Vpn_gateway = ResourceType' "vpn-gateway"

{-# COMPLETE
  ResourceType_Client_vpn_endpoint,
  ResourceType_Customer_gateway,
  ResourceType_Dedicated_host,
  ResourceType_Dhcp_options,
  ResourceType_Egress_only_internet_gateway,
  ResourceType_Elastic_gpu,
  ResourceType_Elastic_ip,
  ResourceType_Export_image_task,
  ResourceType_Export_instance_task,
  ResourceType_Fleet,
  ResourceType_Fpga_image,
  ResourceType_Host_reservation,
  ResourceType_Image,
  ResourceType_Import_image_task,
  ResourceType_Import_snapshot_task,
  ResourceType_Instance,
  ResourceType_Internet_gateway,
  ResourceType_Key_pair,
  ResourceType_Launch_template,
  ResourceType_Local_gateway_route_table_vpc_association,
  ResourceType_Natgateway,
  ResourceType_Network_acl,
  ResourceType_Network_insights_analysis,
  ResourceType_Network_insights_path,
  ResourceType_Network_interface,
  ResourceType_Placement_group,
  ResourceType_Reserved_instances,
  ResourceType_Route_table,
  ResourceType_Security_group,
  ResourceType_Snapshot,
  ResourceType_Spot_fleet_request,
  ResourceType_Spot_instances_request,
  ResourceType_Subnet,
  ResourceType_Traffic_mirror_filter,
  ResourceType_Traffic_mirror_session,
  ResourceType_Traffic_mirror_target,
  ResourceType_Transit_gateway,
  ResourceType_Transit_gateway_attachment,
  ResourceType_Transit_gateway_connect_peer,
  ResourceType_Transit_gateway_multicast_domain,
  ResourceType_Transit_gateway_route_table,
  ResourceType_Volume,
  ResourceType_Vpc,
  ResourceType_Vpc_flow_log,
  ResourceType_Vpc_peering_connection,
  ResourceType_Vpn_connection,
  ResourceType_Vpn_gateway,
  ResourceType'
  #-}
