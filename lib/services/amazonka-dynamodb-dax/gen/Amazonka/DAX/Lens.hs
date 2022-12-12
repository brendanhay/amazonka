{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DAX.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Lens
  ( -- * Operations

    -- ** CreateCluster
    createCluster_availabilityZones,
    createCluster_clusterEndpointEncryptionType,
    createCluster_description,
    createCluster_notificationTopicArn,
    createCluster_parameterGroupName,
    createCluster_preferredMaintenanceWindow,
    createCluster_sSESpecification,
    createCluster_securityGroupIds,
    createCluster_subnetGroupName,
    createCluster_tags,
    createCluster_clusterName,
    createCluster_nodeType,
    createCluster_replicationFactor,
    createCluster_iamRoleArn,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateParameterGroup
    createParameterGroup_description,
    createParameterGroup_parameterGroupName,
    createParameterGroupResponse_parameterGroup,
    createParameterGroupResponse_httpStatus,

    -- ** CreateSubnetGroup
    createSubnetGroup_description,
    createSubnetGroup_subnetGroupName,
    createSubnetGroup_subnetIds,
    createSubnetGroupResponse_subnetGroup,
    createSubnetGroupResponse_httpStatus,

    -- ** DecreaseReplicationFactor
    decreaseReplicationFactor_availabilityZones,
    decreaseReplicationFactor_nodeIdsToRemove,
    decreaseReplicationFactor_clusterName,
    decreaseReplicationFactor_newReplicationFactor,
    decreaseReplicationFactorResponse_cluster,
    decreaseReplicationFactorResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_clusterName,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteParameterGroup
    deleteParameterGroup_parameterGroupName,
    deleteParameterGroupResponse_deletionMessage,
    deleteParameterGroupResponse_httpStatus,

    -- ** DeleteSubnetGroup
    deleteSubnetGroup_subnetGroupName,
    deleteSubnetGroupResponse_deletionMessage,
    deleteSubnetGroupResponse_httpStatus,

    -- ** DescribeClusters
    describeClusters_clusterNames,
    describeClusters_maxResults,
    describeClusters_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_nextToken,
    describeClustersResponse_httpStatus,

    -- ** DescribeDefaultParameters
    describeDefaultParameters_maxResults,
    describeDefaultParameters_nextToken,
    describeDefaultParametersResponse_nextToken,
    describeDefaultParametersResponse_parameters,
    describeDefaultParametersResponse_httpStatus,

    -- ** DescribeEvents
    describeEvents_duration,
    describeEvents_endTime,
    describeEvents_maxResults,
    describeEvents_nextToken,
    describeEvents_sourceName,
    describeEvents_sourceType,
    describeEvents_startTime,
    describeEventsResponse_events,
    describeEventsResponse_nextToken,
    describeEventsResponse_httpStatus,

    -- ** DescribeParameterGroups
    describeParameterGroups_maxResults,
    describeParameterGroups_nextToken,
    describeParameterGroups_parameterGroupNames,
    describeParameterGroupsResponse_nextToken,
    describeParameterGroupsResponse_parameterGroups,
    describeParameterGroupsResponse_httpStatus,

    -- ** DescribeParameters
    describeParameters_maxResults,
    describeParameters_nextToken,
    describeParameters_source,
    describeParameters_parameterGroupName,
    describeParametersResponse_nextToken,
    describeParametersResponse_parameters,
    describeParametersResponse_httpStatus,

    -- ** DescribeSubnetGroups
    describeSubnetGroups_maxResults,
    describeSubnetGroups_nextToken,
    describeSubnetGroups_subnetGroupNames,
    describeSubnetGroupsResponse_nextToken,
    describeSubnetGroupsResponse_subnetGroups,
    describeSubnetGroupsResponse_httpStatus,

    -- ** IncreaseReplicationFactor
    increaseReplicationFactor_availabilityZones,
    increaseReplicationFactor_clusterName,
    increaseReplicationFactor_newReplicationFactor,
    increaseReplicationFactorResponse_cluster,
    increaseReplicationFactorResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_resourceName,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** RebootNode
    rebootNode_clusterName,
    rebootNode_nodeId,
    rebootNodeResponse_cluster,
    rebootNodeResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceName,
    tagResource_tags,
    tagResourceResponse_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceName,
    untagResource_tagKeys,
    untagResourceResponse_tags,
    untagResourceResponse_httpStatus,

    -- ** UpdateCluster
    updateCluster_description,
    updateCluster_notificationTopicArn,
    updateCluster_notificationTopicStatus,
    updateCluster_parameterGroupName,
    updateCluster_preferredMaintenanceWindow,
    updateCluster_securityGroupIds,
    updateCluster_clusterName,
    updateClusterResponse_cluster,
    updateClusterResponse_httpStatus,

    -- ** UpdateParameterGroup
    updateParameterGroup_parameterGroupName,
    updateParameterGroup_parameterNameValues,
    updateParameterGroupResponse_parameterGroup,
    updateParameterGroupResponse_httpStatus,

    -- ** UpdateSubnetGroup
    updateSubnetGroup_description,
    updateSubnetGroup_subnetIds,
    updateSubnetGroup_subnetGroupName,
    updateSubnetGroupResponse_subnetGroup,
    updateSubnetGroupResponse_httpStatus,

    -- * Types

    -- ** Cluster
    cluster_activeNodes,
    cluster_clusterArn,
    cluster_clusterDiscoveryEndpoint,
    cluster_clusterEndpointEncryptionType,
    cluster_clusterName,
    cluster_description,
    cluster_iamRoleArn,
    cluster_nodeIdsToRemove,
    cluster_nodeType,
    cluster_nodes,
    cluster_notificationConfiguration,
    cluster_parameterGroup,
    cluster_preferredMaintenanceWindow,
    cluster_sSEDescription,
    cluster_securityGroups,
    cluster_status,
    cluster_subnetGroup,
    cluster_totalNodes,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,
    endpoint_url,

    -- ** Event
    event_date,
    event_message,
    event_sourceName,
    event_sourceType,

    -- ** Node
    node_availabilityZone,
    node_endpoint,
    node_nodeCreateTime,
    node_nodeId,
    node_nodeStatus,
    node_parameterGroupStatus,

    -- ** NodeTypeSpecificValue
    nodeTypeSpecificValue_nodeType,
    nodeTypeSpecificValue_value,

    -- ** NotificationConfiguration
    notificationConfiguration_topicArn,
    notificationConfiguration_topicStatus,

    -- ** Parameter
    parameter_allowedValues,
    parameter_changeType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_nodeTypeSpecificValues,
    parameter_parameterName,
    parameter_parameterType,
    parameter_parameterValue,
    parameter_source,

    -- ** ParameterGroup
    parameterGroup_description,
    parameterGroup_parameterGroupName,

    -- ** ParameterGroupStatus
    parameterGroupStatus_nodeIdsToReboot,
    parameterGroupStatus_parameterApplyStatus,
    parameterGroupStatus_parameterGroupName,

    -- ** ParameterNameValue
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

    -- ** SSEDescription
    sSEDescription_status,

    -- ** SSESpecification
    sSESpecification_enabled,

    -- ** SecurityGroupMembership
    securityGroupMembership_securityGroupIdentifier,
    securityGroupMembership_status,

    -- ** Subnet
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,

    -- ** SubnetGroup
    subnetGroup_description,
    subnetGroup_subnetGroupName,
    subnetGroup_subnets,
    subnetGroup_vpcId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.DAX.CreateCluster
import Amazonka.DAX.CreateParameterGroup
import Amazonka.DAX.CreateSubnetGroup
import Amazonka.DAX.DecreaseReplicationFactor
import Amazonka.DAX.DeleteCluster
import Amazonka.DAX.DeleteParameterGroup
import Amazonka.DAX.DeleteSubnetGroup
import Amazonka.DAX.DescribeClusters
import Amazonka.DAX.DescribeDefaultParameters
import Amazonka.DAX.DescribeEvents
import Amazonka.DAX.DescribeParameterGroups
import Amazonka.DAX.DescribeParameters
import Amazonka.DAX.DescribeSubnetGroups
import Amazonka.DAX.IncreaseReplicationFactor
import Amazonka.DAX.ListTags
import Amazonka.DAX.RebootNode
import Amazonka.DAX.TagResource
import Amazonka.DAX.Types.Cluster
import Amazonka.DAX.Types.Endpoint
import Amazonka.DAX.Types.Event
import Amazonka.DAX.Types.Node
import Amazonka.DAX.Types.NodeTypeSpecificValue
import Amazonka.DAX.Types.NotificationConfiguration
import Amazonka.DAX.Types.Parameter
import Amazonka.DAX.Types.ParameterGroup
import Amazonka.DAX.Types.ParameterGroupStatus
import Amazonka.DAX.Types.ParameterNameValue
import Amazonka.DAX.Types.SSEDescription
import Amazonka.DAX.Types.SSESpecification
import Amazonka.DAX.Types.SecurityGroupMembership
import Amazonka.DAX.Types.Subnet
import Amazonka.DAX.Types.SubnetGroup
import Amazonka.DAX.Types.Tag
import Amazonka.DAX.UntagResource
import Amazonka.DAX.UpdateCluster
import Amazonka.DAX.UpdateParameterGroup
import Amazonka.DAX.UpdateSubnetGroup
