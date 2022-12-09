{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MwAA.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Lens
  ( -- * Operations

    -- ** CreateCliToken
    createCliToken_name,
    createCliTokenResponse_cliToken,
    createCliTokenResponse_webServerHostname,
    createCliTokenResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_airflowConfigurationOptions,
    createEnvironment_airflowVersion,
    createEnvironment_environmentClass,
    createEnvironment_kmsKey,
    createEnvironment_loggingConfiguration,
    createEnvironment_maxWorkers,
    createEnvironment_minWorkers,
    createEnvironment_pluginsS3ObjectVersion,
    createEnvironment_pluginsS3Path,
    createEnvironment_requirementsS3ObjectVersion,
    createEnvironment_requirementsS3Path,
    createEnvironment_schedulers,
    createEnvironment_tags,
    createEnvironment_webserverAccessMode,
    createEnvironment_weeklyMaintenanceWindowStart,
    createEnvironment_dagS3Path,
    createEnvironment_executionRoleArn,
    createEnvironment_name,
    createEnvironment_networkConfiguration,
    createEnvironment_sourceBucketArn,
    createEnvironmentResponse_arn,
    createEnvironmentResponse_httpStatus,

    -- ** CreateWebLoginToken
    createWebLoginToken_name,
    createWebLoginTokenResponse_webServerHostname,
    createWebLoginTokenResponse_webToken,
    createWebLoginTokenResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_name,
    deleteEnvironmentResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_name,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PublishMetrics
    publishMetrics_environmentName,
    publishMetrics_metricData,
    publishMetricsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_airflowConfigurationOptions,
    updateEnvironment_airflowVersion,
    updateEnvironment_dagS3Path,
    updateEnvironment_environmentClass,
    updateEnvironment_executionRoleArn,
    updateEnvironment_loggingConfiguration,
    updateEnvironment_maxWorkers,
    updateEnvironment_minWorkers,
    updateEnvironment_networkConfiguration,
    updateEnvironment_pluginsS3ObjectVersion,
    updateEnvironment_pluginsS3Path,
    updateEnvironment_requirementsS3ObjectVersion,
    updateEnvironment_requirementsS3Path,
    updateEnvironment_schedulers,
    updateEnvironment_sourceBucketArn,
    updateEnvironment_webserverAccessMode,
    updateEnvironment_weeklyMaintenanceWindowStart,
    updateEnvironment_name,
    updateEnvironmentResponse_arn,
    updateEnvironmentResponse_httpStatus,

    -- * Types

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** Environment
    environment_airflowConfigurationOptions,
    environment_airflowVersion,
    environment_arn,
    environment_createdAt,
    environment_dagS3Path,
    environment_environmentClass,
    environment_executionRoleArn,
    environment_kmsKey,
    environment_lastUpdate,
    environment_loggingConfiguration,
    environment_maxWorkers,
    environment_minWorkers,
    environment_name,
    environment_networkConfiguration,
    environment_pluginsS3ObjectVersion,
    environment_pluginsS3Path,
    environment_requirementsS3ObjectVersion,
    environment_requirementsS3Path,
    environment_schedulers,
    environment_serviceRoleArn,
    environment_sourceBucketArn,
    environment_status,
    environment_tags,
    environment_webserverAccessMode,
    environment_webserverUrl,
    environment_weeklyMaintenanceWindowStart,

    -- ** LastUpdate
    lastUpdate_createdAt,
    lastUpdate_error,
    lastUpdate_source,
    lastUpdate_status,

    -- ** LoggingConfiguration
    loggingConfiguration_dagProcessingLogs,
    loggingConfiguration_schedulerLogs,
    loggingConfiguration_taskLogs,
    loggingConfiguration_webserverLogs,
    loggingConfiguration_workerLogs,

    -- ** LoggingConfigurationInput
    loggingConfigurationInput_dagProcessingLogs,
    loggingConfigurationInput_schedulerLogs,
    loggingConfigurationInput_taskLogs,
    loggingConfigurationInput_webserverLogs,
    loggingConfigurationInput_workerLogs,

    -- ** MetricDatum
    metricDatum_dimensions,
    metricDatum_statisticValues,
    metricDatum_unit,
    metricDatum_value,
    metricDatum_metricName,
    metricDatum_timestamp,

    -- ** ModuleLoggingConfiguration
    moduleLoggingConfiguration_cloudWatchLogGroupArn,
    moduleLoggingConfiguration_enabled,
    moduleLoggingConfiguration_logLevel,

    -- ** ModuleLoggingConfigurationInput
    moduleLoggingConfigurationInput_enabled,
    moduleLoggingConfigurationInput_logLevel,

    -- ** NetworkConfiguration
    networkConfiguration_securityGroupIds,
    networkConfiguration_subnetIds,

    -- ** StatisticSet
    statisticSet_maximum,
    statisticSet_minimum,
    statisticSet_sampleCount,
    statisticSet_sum,

    -- ** UpdateError
    updateError_errorCode,
    updateError_errorMessage,

    -- ** UpdateNetworkConfigurationInput
    updateNetworkConfigurationInput_securityGroupIds,
  )
where

import Amazonka.MwAA.CreateCliToken
import Amazonka.MwAA.CreateEnvironment
import Amazonka.MwAA.CreateWebLoginToken
import Amazonka.MwAA.DeleteEnvironment
import Amazonka.MwAA.GetEnvironment
import Amazonka.MwAA.ListEnvironments
import Amazonka.MwAA.ListTagsForResource
import Amazonka.MwAA.PublishMetrics
import Amazonka.MwAA.TagResource
import Amazonka.MwAA.Types.Dimension
import Amazonka.MwAA.Types.Environment
import Amazonka.MwAA.Types.LastUpdate
import Amazonka.MwAA.Types.LoggingConfiguration
import Amazonka.MwAA.Types.LoggingConfigurationInput
import Amazonka.MwAA.Types.MetricDatum
import Amazonka.MwAA.Types.ModuleLoggingConfiguration
import Amazonka.MwAA.Types.ModuleLoggingConfigurationInput
import Amazonka.MwAA.Types.NetworkConfiguration
import Amazonka.MwAA.Types.StatisticSet
import Amazonka.MwAA.Types.UpdateError
import Amazonka.MwAA.Types.UpdateNetworkConfigurationInput
import Amazonka.MwAA.UntagResource
import Amazonka.MwAA.UpdateEnvironment
