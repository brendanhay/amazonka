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
-- Module      : Amazonka.EC2.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_Capacity_reservation,
        ResourceType_Capacity_reservation_fleet,
        ResourceType_Carrier_gateway,
        ResourceType_Client_vpn_endpoint,
        ResourceType_Coip_pool,
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
        ResourceType_Instance_event_window,
        ResourceType_Internet_gateway,
        ResourceType_Ipam,
        ResourceType_Ipam_pool,
        ResourceType_Ipam_scope,
        ResourceType_Ipv4pool_ec2,
        ResourceType_Ipv6pool_ec2,
        ResourceType_Key_pair,
        ResourceType_Launch_template,
        ResourceType_Local_gateway,
        ResourceType_Local_gateway_route_table,
        ResourceType_Local_gateway_route_table_virtual_interface_group_association,
        ResourceType_Local_gateway_route_table_vpc_association,
        ResourceType_Local_gateway_virtual_interface,
        ResourceType_Local_gateway_virtual_interface_group,
        ResourceType_Natgateway,
        ResourceType_Network_acl,
        ResourceType_Network_insights_access_scope,
        ResourceType_Network_insights_access_scope_analysis,
        ResourceType_Network_insights_analysis,
        ResourceType_Network_insights_path,
        ResourceType_Network_interface,
        ResourceType_Placement_group,
        ResourceType_Prefix_list,
        ResourceType_Replace_root_volume_task,
        ResourceType_Reserved_instances,
        ResourceType_Route_table,
        ResourceType_Security_group,
        ResourceType_Security_group_rule,
        ResourceType_Snapshot,
        ResourceType_Spot_fleet_request,
        ResourceType_Spot_instances_request,
        ResourceType_Subnet,
        ResourceType_Subnet_cidr_reservation,
        ResourceType_Traffic_mirror_filter,
        ResourceType_Traffic_mirror_filter_rule,
        ResourceType_Traffic_mirror_session,
        ResourceType_Traffic_mirror_target,
        ResourceType_Transit_gateway,
        ResourceType_Transit_gateway_attachment,
        ResourceType_Transit_gateway_connect_peer,
        ResourceType_Transit_gateway_multicast_domain,
        ResourceType_Transit_gateway_policy_table,
        ResourceType_Transit_gateway_route_table,
        ResourceType_Transit_gateway_route_table_announcement,
        ResourceType_Volume,
        ResourceType_Vpc,
        ResourceType_Vpc_endpoint,
        ResourceType_Vpc_endpoint_connection,
        ResourceType_Vpc_endpoint_connection_device_type,
        ResourceType_Vpc_endpoint_service,
        ResourceType_Vpc_endpoint_service_permission,
        ResourceType_Vpc_flow_log,
        ResourceType_Vpc_peering_connection,
        ResourceType_Vpn_connection,
        ResourceType_Vpn_connection_device_type,
        ResourceType_Vpn_gateway
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ResourceType_Capacity_reservation :: ResourceType
pattern ResourceType_Capacity_reservation = ResourceType' "capacity-reservation"

pattern ResourceType_Capacity_reservation_fleet :: ResourceType
pattern ResourceType_Capacity_reservation_fleet = ResourceType' "capacity-reservation-fleet"

pattern ResourceType_Carrier_gateway :: ResourceType
pattern ResourceType_Carrier_gateway = ResourceType' "carrier-gateway"

pattern ResourceType_Client_vpn_endpoint :: ResourceType
pattern ResourceType_Client_vpn_endpoint = ResourceType' "client-vpn-endpoint"

pattern ResourceType_Coip_pool :: ResourceType
pattern ResourceType_Coip_pool = ResourceType' "coip-pool"

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

pattern ResourceType_Instance_event_window :: ResourceType
pattern ResourceType_Instance_event_window = ResourceType' "instance-event-window"

pattern ResourceType_Internet_gateway :: ResourceType
pattern ResourceType_Internet_gateway = ResourceType' "internet-gateway"

pattern ResourceType_Ipam :: ResourceType
pattern ResourceType_Ipam = ResourceType' "ipam"

pattern ResourceType_Ipam_pool :: ResourceType
pattern ResourceType_Ipam_pool = ResourceType' "ipam-pool"

pattern ResourceType_Ipam_scope :: ResourceType
pattern ResourceType_Ipam_scope = ResourceType' "ipam-scope"

pattern ResourceType_Ipv4pool_ec2 :: ResourceType
pattern ResourceType_Ipv4pool_ec2 = ResourceType' "ipv4pool-ec2"

pattern ResourceType_Ipv6pool_ec2 :: ResourceType
pattern ResourceType_Ipv6pool_ec2 = ResourceType' "ipv6pool-ec2"

pattern ResourceType_Key_pair :: ResourceType
pattern ResourceType_Key_pair = ResourceType' "key-pair"

pattern ResourceType_Launch_template :: ResourceType
pattern ResourceType_Launch_template = ResourceType' "launch-template"

pattern ResourceType_Local_gateway :: ResourceType
pattern ResourceType_Local_gateway = ResourceType' "local-gateway"

pattern ResourceType_Local_gateway_route_table :: ResourceType
pattern ResourceType_Local_gateway_route_table = ResourceType' "local-gateway-route-table"

pattern ResourceType_Local_gateway_route_table_virtual_interface_group_association :: ResourceType
pattern ResourceType_Local_gateway_route_table_virtual_interface_group_association = ResourceType' "local-gateway-route-table-virtual-interface-group-association"

pattern ResourceType_Local_gateway_route_table_vpc_association :: ResourceType
pattern ResourceType_Local_gateway_route_table_vpc_association = ResourceType' "local-gateway-route-table-vpc-association"

pattern ResourceType_Local_gateway_virtual_interface :: ResourceType
pattern ResourceType_Local_gateway_virtual_interface = ResourceType' "local-gateway-virtual-interface"

pattern ResourceType_Local_gateway_virtual_interface_group :: ResourceType
pattern ResourceType_Local_gateway_virtual_interface_group = ResourceType' "local-gateway-virtual-interface-group"

pattern ResourceType_Natgateway :: ResourceType
pattern ResourceType_Natgateway = ResourceType' "natgateway"

pattern ResourceType_Network_acl :: ResourceType
pattern ResourceType_Network_acl = ResourceType' "network-acl"

pattern ResourceType_Network_insights_access_scope :: ResourceType
pattern ResourceType_Network_insights_access_scope = ResourceType' "network-insights-access-scope"

pattern ResourceType_Network_insights_access_scope_analysis :: ResourceType
pattern ResourceType_Network_insights_access_scope_analysis = ResourceType' "network-insights-access-scope-analysis"

pattern ResourceType_Network_insights_analysis :: ResourceType
pattern ResourceType_Network_insights_analysis = ResourceType' "network-insights-analysis"

pattern ResourceType_Network_insights_path :: ResourceType
pattern ResourceType_Network_insights_path = ResourceType' "network-insights-path"

pattern ResourceType_Network_interface :: ResourceType
pattern ResourceType_Network_interface = ResourceType' "network-interface"

pattern ResourceType_Placement_group :: ResourceType
pattern ResourceType_Placement_group = ResourceType' "placement-group"

pattern ResourceType_Prefix_list :: ResourceType
pattern ResourceType_Prefix_list = ResourceType' "prefix-list"

pattern ResourceType_Replace_root_volume_task :: ResourceType
pattern ResourceType_Replace_root_volume_task = ResourceType' "replace-root-volume-task"

pattern ResourceType_Reserved_instances :: ResourceType
pattern ResourceType_Reserved_instances = ResourceType' "reserved-instances"

pattern ResourceType_Route_table :: ResourceType
pattern ResourceType_Route_table = ResourceType' "route-table"

pattern ResourceType_Security_group :: ResourceType
pattern ResourceType_Security_group = ResourceType' "security-group"

pattern ResourceType_Security_group_rule :: ResourceType
pattern ResourceType_Security_group_rule = ResourceType' "security-group-rule"

pattern ResourceType_Snapshot :: ResourceType
pattern ResourceType_Snapshot = ResourceType' "snapshot"

pattern ResourceType_Spot_fleet_request :: ResourceType
pattern ResourceType_Spot_fleet_request = ResourceType' "spot-fleet-request"

pattern ResourceType_Spot_instances_request :: ResourceType
pattern ResourceType_Spot_instances_request = ResourceType' "spot-instances-request"

pattern ResourceType_Subnet :: ResourceType
pattern ResourceType_Subnet = ResourceType' "subnet"

pattern ResourceType_Subnet_cidr_reservation :: ResourceType
pattern ResourceType_Subnet_cidr_reservation = ResourceType' "subnet-cidr-reservation"

pattern ResourceType_Traffic_mirror_filter :: ResourceType
pattern ResourceType_Traffic_mirror_filter = ResourceType' "traffic-mirror-filter"

pattern ResourceType_Traffic_mirror_filter_rule :: ResourceType
pattern ResourceType_Traffic_mirror_filter_rule = ResourceType' "traffic-mirror-filter-rule"

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

pattern ResourceType_Transit_gateway_policy_table :: ResourceType
pattern ResourceType_Transit_gateway_policy_table = ResourceType' "transit-gateway-policy-table"

pattern ResourceType_Transit_gateway_route_table :: ResourceType
pattern ResourceType_Transit_gateway_route_table = ResourceType' "transit-gateway-route-table"

pattern ResourceType_Transit_gateway_route_table_announcement :: ResourceType
pattern ResourceType_Transit_gateway_route_table_announcement = ResourceType' "transit-gateway-route-table-announcement"

pattern ResourceType_Volume :: ResourceType
pattern ResourceType_Volume = ResourceType' "volume"

pattern ResourceType_Vpc :: ResourceType
pattern ResourceType_Vpc = ResourceType' "vpc"

pattern ResourceType_Vpc_endpoint :: ResourceType
pattern ResourceType_Vpc_endpoint = ResourceType' "vpc-endpoint"

pattern ResourceType_Vpc_endpoint_connection :: ResourceType
pattern ResourceType_Vpc_endpoint_connection = ResourceType' "vpc-endpoint-connection"

pattern ResourceType_Vpc_endpoint_connection_device_type :: ResourceType
pattern ResourceType_Vpc_endpoint_connection_device_type = ResourceType' "vpc-endpoint-connection-device-type"

pattern ResourceType_Vpc_endpoint_service :: ResourceType
pattern ResourceType_Vpc_endpoint_service = ResourceType' "vpc-endpoint-service"

pattern ResourceType_Vpc_endpoint_service_permission :: ResourceType
pattern ResourceType_Vpc_endpoint_service_permission = ResourceType' "vpc-endpoint-service-permission"

pattern ResourceType_Vpc_flow_log :: ResourceType
pattern ResourceType_Vpc_flow_log = ResourceType' "vpc-flow-log"

pattern ResourceType_Vpc_peering_connection :: ResourceType
pattern ResourceType_Vpc_peering_connection = ResourceType' "vpc-peering-connection"

pattern ResourceType_Vpn_connection :: ResourceType
pattern ResourceType_Vpn_connection = ResourceType' "vpn-connection"

pattern ResourceType_Vpn_connection_device_type :: ResourceType
pattern ResourceType_Vpn_connection_device_type = ResourceType' "vpn-connection-device-type"

pattern ResourceType_Vpn_gateway :: ResourceType
pattern ResourceType_Vpn_gateway = ResourceType' "vpn-gateway"

{-# COMPLETE
  ResourceType_Capacity_reservation,
  ResourceType_Capacity_reservation_fleet,
  ResourceType_Carrier_gateway,
  ResourceType_Client_vpn_endpoint,
  ResourceType_Coip_pool,
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
  ResourceType_Instance_event_window,
  ResourceType_Internet_gateway,
  ResourceType_Ipam,
  ResourceType_Ipam_pool,
  ResourceType_Ipam_scope,
  ResourceType_Ipv4pool_ec2,
  ResourceType_Ipv6pool_ec2,
  ResourceType_Key_pair,
  ResourceType_Launch_template,
  ResourceType_Local_gateway,
  ResourceType_Local_gateway_route_table,
  ResourceType_Local_gateway_route_table_virtual_interface_group_association,
  ResourceType_Local_gateway_route_table_vpc_association,
  ResourceType_Local_gateway_virtual_interface,
  ResourceType_Local_gateway_virtual_interface_group,
  ResourceType_Natgateway,
  ResourceType_Network_acl,
  ResourceType_Network_insights_access_scope,
  ResourceType_Network_insights_access_scope_analysis,
  ResourceType_Network_insights_analysis,
  ResourceType_Network_insights_path,
  ResourceType_Network_interface,
  ResourceType_Placement_group,
  ResourceType_Prefix_list,
  ResourceType_Replace_root_volume_task,
  ResourceType_Reserved_instances,
  ResourceType_Route_table,
  ResourceType_Security_group,
  ResourceType_Security_group_rule,
  ResourceType_Snapshot,
  ResourceType_Spot_fleet_request,
  ResourceType_Spot_instances_request,
  ResourceType_Subnet,
  ResourceType_Subnet_cidr_reservation,
  ResourceType_Traffic_mirror_filter,
  ResourceType_Traffic_mirror_filter_rule,
  ResourceType_Traffic_mirror_session,
  ResourceType_Traffic_mirror_target,
  ResourceType_Transit_gateway,
  ResourceType_Transit_gateway_attachment,
  ResourceType_Transit_gateway_connect_peer,
  ResourceType_Transit_gateway_multicast_domain,
  ResourceType_Transit_gateway_policy_table,
  ResourceType_Transit_gateway_route_table,
  ResourceType_Transit_gateway_route_table_announcement,
  ResourceType_Volume,
  ResourceType_Vpc,
  ResourceType_Vpc_endpoint,
  ResourceType_Vpc_endpoint_connection,
  ResourceType_Vpc_endpoint_connection_device_type,
  ResourceType_Vpc_endpoint_service,
  ResourceType_Vpc_endpoint_service_permission,
  ResourceType_Vpc_flow_log,
  ResourceType_Vpc_peering_connection,
  ResourceType_Vpn_connection,
  ResourceType_Vpn_connection_device_type,
  ResourceType_Vpn_gateway,
  ResourceType'
  #-}
