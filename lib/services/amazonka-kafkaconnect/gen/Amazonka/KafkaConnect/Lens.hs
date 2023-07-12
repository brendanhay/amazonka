{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KafkaConnect.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Lens
  ( -- * Operations

    -- ** CreateConnector
    createConnector_connectorDescription,
    createConnector_logDelivery,
    createConnector_workerConfiguration,
    createConnector_capacity,
    createConnector_connectorConfiguration,
    createConnector_connectorName,
    createConnector_kafkaCluster,
    createConnector_kafkaClusterClientAuthentication,
    createConnector_kafkaClusterEncryptionInTransit,
    createConnector_kafkaConnectVersion,
    createConnector_plugins,
    createConnector_serviceExecutionRoleArn,
    createConnectorResponse_connectorArn,
    createConnectorResponse_connectorName,
    createConnectorResponse_connectorState,
    createConnectorResponse_httpStatus,

    -- ** CreateCustomPlugin
    createCustomPlugin_description,
    createCustomPlugin_contentType,
    createCustomPlugin_location,
    createCustomPlugin_name,
    createCustomPluginResponse_customPluginArn,
    createCustomPluginResponse_customPluginState,
    createCustomPluginResponse_name,
    createCustomPluginResponse_revision,
    createCustomPluginResponse_httpStatus,

    -- ** CreateWorkerConfiguration
    createWorkerConfiguration_description,
    createWorkerConfiguration_name,
    createWorkerConfiguration_propertiesFileContent,
    createWorkerConfigurationResponse_creationTime,
    createWorkerConfigurationResponse_latestRevision,
    createWorkerConfigurationResponse_name,
    createWorkerConfigurationResponse_workerConfigurationArn,
    createWorkerConfigurationResponse_httpStatus,

    -- ** DeleteConnector
    deleteConnector_currentVersion,
    deleteConnector_connectorArn,
    deleteConnectorResponse_connectorArn,
    deleteConnectorResponse_connectorState,
    deleteConnectorResponse_httpStatus,

    -- ** DeleteCustomPlugin
    deleteCustomPlugin_customPluginArn,
    deleteCustomPluginResponse_customPluginArn,
    deleteCustomPluginResponse_customPluginState,
    deleteCustomPluginResponse_httpStatus,

    -- ** DescribeConnector
    describeConnector_connectorArn,
    describeConnectorResponse_capacity,
    describeConnectorResponse_connectorArn,
    describeConnectorResponse_connectorConfiguration,
    describeConnectorResponse_connectorDescription,
    describeConnectorResponse_connectorName,
    describeConnectorResponse_connectorState,
    describeConnectorResponse_creationTime,
    describeConnectorResponse_currentVersion,
    describeConnectorResponse_kafkaCluster,
    describeConnectorResponse_kafkaClusterClientAuthentication,
    describeConnectorResponse_kafkaClusterEncryptionInTransit,
    describeConnectorResponse_kafkaConnectVersion,
    describeConnectorResponse_logDelivery,
    describeConnectorResponse_plugins,
    describeConnectorResponse_serviceExecutionRoleArn,
    describeConnectorResponse_stateDescription,
    describeConnectorResponse_workerConfiguration,
    describeConnectorResponse_httpStatus,

    -- ** DescribeCustomPlugin
    describeCustomPlugin_customPluginArn,
    describeCustomPluginResponse_creationTime,
    describeCustomPluginResponse_customPluginArn,
    describeCustomPluginResponse_customPluginState,
    describeCustomPluginResponse_description,
    describeCustomPluginResponse_latestRevision,
    describeCustomPluginResponse_name,
    describeCustomPluginResponse_stateDescription,
    describeCustomPluginResponse_httpStatus,

    -- ** DescribeWorkerConfiguration
    describeWorkerConfiguration_workerConfigurationArn,
    describeWorkerConfigurationResponse_creationTime,
    describeWorkerConfigurationResponse_description,
    describeWorkerConfigurationResponse_latestRevision,
    describeWorkerConfigurationResponse_name,
    describeWorkerConfigurationResponse_workerConfigurationArn,
    describeWorkerConfigurationResponse_httpStatus,

    -- ** ListConnectors
    listConnectors_connectorNamePrefix,
    listConnectors_maxResults,
    listConnectors_nextToken,
    listConnectorsResponse_connectors,
    listConnectorsResponse_nextToken,
    listConnectorsResponse_httpStatus,

    -- ** ListCustomPlugins
    listCustomPlugins_maxResults,
    listCustomPlugins_nextToken,
    listCustomPluginsResponse_customPlugins,
    listCustomPluginsResponse_nextToken,
    listCustomPluginsResponse_httpStatus,

    -- ** ListWorkerConfigurations
    listWorkerConfigurations_maxResults,
    listWorkerConfigurations_nextToken,
    listWorkerConfigurationsResponse_nextToken,
    listWorkerConfigurationsResponse_workerConfigurations,
    listWorkerConfigurationsResponse_httpStatus,

    -- ** UpdateConnector
    updateConnector_capacity,
    updateConnector_connectorArn,
    updateConnector_currentVersion,
    updateConnectorResponse_connectorArn,
    updateConnectorResponse_connectorState,
    updateConnectorResponse_httpStatus,

    -- * Types

    -- ** ApacheKafkaCluster
    apacheKafkaCluster_bootstrapServers,
    apacheKafkaCluster_vpc,

    -- ** ApacheKafkaClusterDescription
    apacheKafkaClusterDescription_bootstrapServers,
    apacheKafkaClusterDescription_vpc,

    -- ** AutoScaling
    autoScaling_scaleInPolicy,
    autoScaling_scaleOutPolicy,
    autoScaling_maxWorkerCount,
    autoScaling_mcuCount,
    autoScaling_minWorkerCount,

    -- ** AutoScalingDescription
    autoScalingDescription_maxWorkerCount,
    autoScalingDescription_mcuCount,
    autoScalingDescription_minWorkerCount,
    autoScalingDescription_scaleInPolicy,
    autoScalingDescription_scaleOutPolicy,

    -- ** AutoScalingUpdate
    autoScalingUpdate_maxWorkerCount,
    autoScalingUpdate_mcuCount,
    autoScalingUpdate_minWorkerCount,
    autoScalingUpdate_scaleInPolicy,
    autoScalingUpdate_scaleOutPolicy,

    -- ** Capacity
    capacity_autoScaling,
    capacity_provisionedCapacity,

    -- ** CapacityDescription
    capacityDescription_autoScaling,
    capacityDescription_provisionedCapacity,

    -- ** CapacityUpdate
    capacityUpdate_autoScaling,
    capacityUpdate_provisionedCapacity,

    -- ** CloudWatchLogsLogDelivery
    cloudWatchLogsLogDelivery_logGroup,
    cloudWatchLogsLogDelivery_enabled,

    -- ** CloudWatchLogsLogDeliveryDescription
    cloudWatchLogsLogDeliveryDescription_enabled,
    cloudWatchLogsLogDeliveryDescription_logGroup,

    -- ** ConnectorSummary
    connectorSummary_capacity,
    connectorSummary_connectorArn,
    connectorSummary_connectorDescription,
    connectorSummary_connectorName,
    connectorSummary_connectorState,
    connectorSummary_creationTime,
    connectorSummary_currentVersion,
    connectorSummary_kafkaCluster,
    connectorSummary_kafkaClusterClientAuthentication,
    connectorSummary_kafkaClusterEncryptionInTransit,
    connectorSummary_kafkaConnectVersion,
    connectorSummary_logDelivery,
    connectorSummary_plugins,
    connectorSummary_serviceExecutionRoleArn,
    connectorSummary_workerConfiguration,

    -- ** CustomPlugin
    customPlugin_customPluginArn,
    customPlugin_revision,

    -- ** CustomPluginDescription
    customPluginDescription_customPluginArn,
    customPluginDescription_revision,

    -- ** CustomPluginFileDescription
    customPluginFileDescription_fileMd5,
    customPluginFileDescription_fileSize,

    -- ** CustomPluginLocation
    customPluginLocation_s3Location,

    -- ** CustomPluginLocationDescription
    customPluginLocationDescription_s3Location,

    -- ** CustomPluginRevisionSummary
    customPluginRevisionSummary_contentType,
    customPluginRevisionSummary_creationTime,
    customPluginRevisionSummary_description,
    customPluginRevisionSummary_fileDescription,
    customPluginRevisionSummary_location,
    customPluginRevisionSummary_revision,

    -- ** CustomPluginSummary
    customPluginSummary_creationTime,
    customPluginSummary_customPluginArn,
    customPluginSummary_customPluginState,
    customPluginSummary_description,
    customPluginSummary_latestRevision,
    customPluginSummary_name,

    -- ** FirehoseLogDelivery
    firehoseLogDelivery_deliveryStream,
    firehoseLogDelivery_enabled,

    -- ** FirehoseLogDeliveryDescription
    firehoseLogDeliveryDescription_deliveryStream,
    firehoseLogDeliveryDescription_enabled,

    -- ** KafkaCluster
    kafkaCluster_apacheKafkaCluster,

    -- ** KafkaClusterClientAuthentication
    kafkaClusterClientAuthentication_authenticationType,

    -- ** KafkaClusterClientAuthenticationDescription
    kafkaClusterClientAuthenticationDescription_authenticationType,

    -- ** KafkaClusterDescription
    kafkaClusterDescription_apacheKafkaCluster,

    -- ** KafkaClusterEncryptionInTransit
    kafkaClusterEncryptionInTransit_encryptionType,

    -- ** KafkaClusterEncryptionInTransitDescription
    kafkaClusterEncryptionInTransitDescription_encryptionType,

    -- ** LogDelivery
    logDelivery_workerLogDelivery,

    -- ** LogDeliveryDescription
    logDeliveryDescription_workerLogDelivery,

    -- ** Plugin
    plugin_customPlugin,

    -- ** PluginDescription
    pluginDescription_customPlugin,

    -- ** ProvisionedCapacity
    provisionedCapacity_mcuCount,
    provisionedCapacity_workerCount,

    -- ** ProvisionedCapacityDescription
    provisionedCapacityDescription_mcuCount,
    provisionedCapacityDescription_workerCount,

    -- ** ProvisionedCapacityUpdate
    provisionedCapacityUpdate_mcuCount,
    provisionedCapacityUpdate_workerCount,

    -- ** S3Location
    s3Location_objectVersion,
    s3Location_bucketArn,
    s3Location_fileKey,

    -- ** S3LocationDescription
    s3LocationDescription_bucketArn,
    s3LocationDescription_fileKey,
    s3LocationDescription_objectVersion,

    -- ** S3LogDelivery
    s3LogDelivery_bucket,
    s3LogDelivery_prefix,
    s3LogDelivery_enabled,

    -- ** S3LogDeliveryDescription
    s3LogDeliveryDescription_bucket,
    s3LogDeliveryDescription_enabled,
    s3LogDeliveryDescription_prefix,

    -- ** ScaleInPolicy
    scaleInPolicy_cpuUtilizationPercentage,

    -- ** ScaleInPolicyDescription
    scaleInPolicyDescription_cpuUtilizationPercentage,

    -- ** ScaleInPolicyUpdate
    scaleInPolicyUpdate_cpuUtilizationPercentage,

    -- ** ScaleOutPolicy
    scaleOutPolicy_cpuUtilizationPercentage,

    -- ** ScaleOutPolicyDescription
    scaleOutPolicyDescription_cpuUtilizationPercentage,

    -- ** ScaleOutPolicyUpdate
    scaleOutPolicyUpdate_cpuUtilizationPercentage,

    -- ** StateDescription
    stateDescription_code,
    stateDescription_message,

    -- ** Vpc
    vpc_securityGroups,
    vpc_subnets,

    -- ** VpcDescription
    vpcDescription_securityGroups,
    vpcDescription_subnets,

    -- ** WorkerConfiguration
    workerConfiguration_revision,
    workerConfiguration_workerConfigurationArn,

    -- ** WorkerConfigurationDescription
    workerConfigurationDescription_revision,
    workerConfigurationDescription_workerConfigurationArn,

    -- ** WorkerConfigurationRevisionDescription
    workerConfigurationRevisionDescription_creationTime,
    workerConfigurationRevisionDescription_description,
    workerConfigurationRevisionDescription_propertiesFileContent,
    workerConfigurationRevisionDescription_revision,

    -- ** WorkerConfigurationRevisionSummary
    workerConfigurationRevisionSummary_creationTime,
    workerConfigurationRevisionSummary_description,
    workerConfigurationRevisionSummary_revision,

    -- ** WorkerConfigurationSummary
    workerConfigurationSummary_creationTime,
    workerConfigurationSummary_description,
    workerConfigurationSummary_latestRevision,
    workerConfigurationSummary_name,
    workerConfigurationSummary_workerConfigurationArn,

    -- ** WorkerLogDelivery
    workerLogDelivery_cloudWatchLogs,
    workerLogDelivery_firehose,
    workerLogDelivery_s3,

    -- ** WorkerLogDeliveryDescription
    workerLogDeliveryDescription_cloudWatchLogs,
    workerLogDeliveryDescription_firehose,
    workerLogDeliveryDescription_s3,
  )
where

import Amazonka.KafkaConnect.CreateConnector
import Amazonka.KafkaConnect.CreateCustomPlugin
import Amazonka.KafkaConnect.CreateWorkerConfiguration
import Amazonka.KafkaConnect.DeleteConnector
import Amazonka.KafkaConnect.DeleteCustomPlugin
import Amazonka.KafkaConnect.DescribeConnector
import Amazonka.KafkaConnect.DescribeCustomPlugin
import Amazonka.KafkaConnect.DescribeWorkerConfiguration
import Amazonka.KafkaConnect.ListConnectors
import Amazonka.KafkaConnect.ListCustomPlugins
import Amazonka.KafkaConnect.ListWorkerConfigurations
import Amazonka.KafkaConnect.Types.ApacheKafkaCluster
import Amazonka.KafkaConnect.Types.ApacheKafkaClusterDescription
import Amazonka.KafkaConnect.Types.AutoScaling
import Amazonka.KafkaConnect.Types.AutoScalingDescription
import Amazonka.KafkaConnect.Types.AutoScalingUpdate
import Amazonka.KafkaConnect.Types.Capacity
import Amazonka.KafkaConnect.Types.CapacityDescription
import Amazonka.KafkaConnect.Types.CapacityUpdate
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDelivery
import Amazonka.KafkaConnect.Types.CloudWatchLogsLogDeliveryDescription
import Amazonka.KafkaConnect.Types.ConnectorSummary
import Amazonka.KafkaConnect.Types.CustomPlugin
import Amazonka.KafkaConnect.Types.CustomPluginDescription
import Amazonka.KafkaConnect.Types.CustomPluginFileDescription
import Amazonka.KafkaConnect.Types.CustomPluginLocation
import Amazonka.KafkaConnect.Types.CustomPluginLocationDescription
import Amazonka.KafkaConnect.Types.CustomPluginRevisionSummary
import Amazonka.KafkaConnect.Types.CustomPluginSummary
import Amazonka.KafkaConnect.Types.FirehoseLogDelivery
import Amazonka.KafkaConnect.Types.FirehoseLogDeliveryDescription
import Amazonka.KafkaConnect.Types.KafkaCluster
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthentication
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationDescription
import Amazonka.KafkaConnect.Types.KafkaClusterDescription
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransit
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription
import Amazonka.KafkaConnect.Types.LogDelivery
import Amazonka.KafkaConnect.Types.LogDeliveryDescription
import Amazonka.KafkaConnect.Types.Plugin
import Amazonka.KafkaConnect.Types.PluginDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacity
import Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacityUpdate
import Amazonka.KafkaConnect.Types.S3Location
import Amazonka.KafkaConnect.Types.S3LocationDescription
import Amazonka.KafkaConnect.Types.S3LogDelivery
import Amazonka.KafkaConnect.Types.S3LogDeliveryDescription
import Amazonka.KafkaConnect.Types.ScaleInPolicy
import Amazonka.KafkaConnect.Types.ScaleInPolicyDescription
import Amazonka.KafkaConnect.Types.ScaleInPolicyUpdate
import Amazonka.KafkaConnect.Types.ScaleOutPolicy
import Amazonka.KafkaConnect.Types.ScaleOutPolicyDescription
import Amazonka.KafkaConnect.Types.ScaleOutPolicyUpdate
import Amazonka.KafkaConnect.Types.StateDescription
import Amazonka.KafkaConnect.Types.Vpc
import Amazonka.KafkaConnect.Types.VpcDescription
import Amazonka.KafkaConnect.Types.WorkerConfiguration
import Amazonka.KafkaConnect.Types.WorkerConfigurationDescription
import Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionDescription
import Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionSummary
import Amazonka.KafkaConnect.Types.WorkerConfigurationSummary
import Amazonka.KafkaConnect.Types.WorkerLogDelivery
import Amazonka.KafkaConnect.Types.WorkerLogDeliveryDescription
import Amazonka.KafkaConnect.UpdateConnector
