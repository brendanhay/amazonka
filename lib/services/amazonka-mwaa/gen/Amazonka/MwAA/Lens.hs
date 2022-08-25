{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MwAA.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Lens
  ( -- * Operations

    -- ** CreateCliToken
    createCliToken_name,
    createCliTokenResponse_webServerHostname,
    createCliTokenResponse_cliToken,
    createCliTokenResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_tags,
    createEnvironment_schedulers,
    createEnvironment_pluginsS3ObjectVersion,
    createEnvironment_requirementsS3Path,
    createEnvironment_pluginsS3Path,
    createEnvironment_airflowVersion,
    createEnvironment_kmsKey,
    createEnvironment_airflowConfigurationOptions,
    createEnvironment_minWorkers,
    createEnvironment_environmentClass,
    createEnvironment_weeklyMaintenanceWindowStart,
    createEnvironment_requirementsS3ObjectVersion,
    createEnvironment_webserverAccessMode,
    createEnvironment_maxWorkers,
    createEnvironment_loggingConfiguration,
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
    listEnvironments_nextToken,
    listEnvironments_maxResults,
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
    updateEnvironment_schedulers,
    updateEnvironment_pluginsS3ObjectVersion,
    updateEnvironment_requirementsS3Path,
    updateEnvironment_pluginsS3Path,
    updateEnvironment_airflowVersion,
    updateEnvironment_dagS3Path,
    updateEnvironment_sourceBucketArn,
    updateEnvironment_airflowConfigurationOptions,
    updateEnvironment_networkConfiguration,
    updateEnvironment_minWorkers,
    updateEnvironment_environmentClass,
    updateEnvironment_weeklyMaintenanceWindowStart,
    updateEnvironment_requirementsS3ObjectVersion,
    updateEnvironment_executionRoleArn,
    updateEnvironment_webserverAccessMode,
    updateEnvironment_maxWorkers,
    updateEnvironment_loggingConfiguration,
    updateEnvironment_name,
    updateEnvironmentResponse_arn,
    updateEnvironmentResponse_httpStatus,

    -- * Types

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** Environment
    environment_tags,
    environment_schedulers,
    environment_pluginsS3ObjectVersion,
    environment_name,
    environment_requirementsS3Path,
    environment_pluginsS3Path,
    environment_airflowVersion,
    environment_serviceRoleArn,
    environment_dagS3Path,
    environment_sourceBucketArn,
    environment_kmsKey,
    environment_arn,
    environment_airflowConfigurationOptions,
    environment_networkConfiguration,
    environment_status,
    environment_minWorkers,
    environment_environmentClass,
    environment_weeklyMaintenanceWindowStart,
    environment_lastUpdate,
    environment_requirementsS3ObjectVersion,
    environment_executionRoleArn,
    environment_webserverAccessMode,
    environment_maxWorkers,
    environment_createdAt,
    environment_webserverUrl,
    environment_loggingConfiguration,

    -- ** LastUpdate
    lastUpdate_status,
    lastUpdate_source,
    lastUpdate_error,
    lastUpdate_createdAt,

    -- ** LoggingConfiguration
    loggingConfiguration_dagProcessingLogs,
    loggingConfiguration_taskLogs,
    loggingConfiguration_workerLogs,
    loggingConfiguration_webserverLogs,
    loggingConfiguration_schedulerLogs,

    -- ** LoggingConfigurationInput
    loggingConfigurationInput_dagProcessingLogs,
    loggingConfigurationInput_taskLogs,
    loggingConfigurationInput_workerLogs,
    loggingConfigurationInput_webserverLogs,
    loggingConfigurationInput_schedulerLogs,

    -- ** MetricDatum
    metricDatum_statisticValues,
    metricDatum_dimensions,
    metricDatum_unit,
    metricDatum_value,
    metricDatum_metricName,
    metricDatum_timestamp,

    -- ** ModuleLoggingConfiguration
    moduleLoggingConfiguration_logLevel,
    moduleLoggingConfiguration_cloudWatchLogGroupArn,
    moduleLoggingConfiguration_enabled,

    -- ** ModuleLoggingConfigurationInput
    moduleLoggingConfigurationInput_enabled,
    moduleLoggingConfigurationInput_logLevel,

    -- ** NetworkConfiguration
    networkConfiguration_securityGroupIds,
    networkConfiguration_subnetIds,

    -- ** StatisticSet
    statisticSet_minimum,
    statisticSet_sampleCount,
    statisticSet_sum,
    statisticSet_maximum,

    -- ** UpdateError
    updateError_errorMessage,
    updateError_errorCode,

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
