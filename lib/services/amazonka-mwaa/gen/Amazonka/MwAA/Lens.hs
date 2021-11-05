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

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,
    listEnvironmentsResponse_environments,

    -- ** UpdateEnvironment
    updateEnvironment_schedulers,
    updateEnvironment_minWorkers,
    updateEnvironment_pluginsS3Path,
    updateEnvironment_webserverAccessMode,
    updateEnvironment_airflowVersion,
    updateEnvironment_weeklyMaintenanceWindowStart,
    updateEnvironment_executionRoleArn,
    updateEnvironment_requirementsS3ObjectVersion,
    updateEnvironment_sourceBucketArn,
    updateEnvironment_dagS3Path,
    updateEnvironment_pluginsS3ObjectVersion,
    updateEnvironment_airflowConfigurationOptions,
    updateEnvironment_loggingConfiguration,
    updateEnvironment_environmentClass,
    updateEnvironment_networkConfiguration,
    updateEnvironment_requirementsS3Path,
    updateEnvironment_maxWorkers,
    updateEnvironment_name,
    updateEnvironmentResponse_arn,
    updateEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_name,
    deleteEnvironmentResponse_httpStatus,

    -- ** CreateWebLoginToken
    createWebLoginToken_name,
    createWebLoginTokenResponse_webServerHostname,
    createWebLoginTokenResponse_webToken,
    createWebLoginTokenResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_name,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** PublishMetrics
    publishMetrics_environmentName,
    publishMetrics_metricData,
    publishMetricsResponse_httpStatus,

    -- ** CreateEnvironment
    createEnvironment_schedulers,
    createEnvironment_minWorkers,
    createEnvironment_pluginsS3Path,
    createEnvironment_webserverAccessMode,
    createEnvironment_airflowVersion,
    createEnvironment_kmsKey,
    createEnvironment_weeklyMaintenanceWindowStart,
    createEnvironment_requirementsS3ObjectVersion,
    createEnvironment_pluginsS3ObjectVersion,
    createEnvironment_airflowConfigurationOptions,
    createEnvironment_loggingConfiguration,
    createEnvironment_environmentClass,
    createEnvironment_tags,
    createEnvironment_requirementsS3Path,
    createEnvironment_maxWorkers,
    createEnvironment_dagS3Path,
    createEnvironment_executionRoleArn,
    createEnvironment_name,
    createEnvironment_networkConfiguration,
    createEnvironment_sourceBucketArn,
    createEnvironmentResponse_arn,
    createEnvironmentResponse_httpStatus,

    -- ** CreateCliToken
    createCliToken_name,
    createCliTokenResponse_webServerHostname,
    createCliTokenResponse_cliToken,
    createCliTokenResponse_httpStatus,

    -- * Types

    -- ** Dimension
    dimension_name,
    dimension_value,

    -- ** Environment
    environment_serviceRoleArn,
    environment_schedulers,
    environment_status,
    environment_minWorkers,
    environment_pluginsS3Path,
    environment_webserverAccessMode,
    environment_airflowVersion,
    environment_kmsKey,
    environment_arn,
    environment_createdAt,
    environment_weeklyMaintenanceWindowStart,
    environment_executionRoleArn,
    environment_requirementsS3ObjectVersion,
    environment_lastUpdate,
    environment_sourceBucketArn,
    environment_webserverUrl,
    environment_dagS3Path,
    environment_name,
    environment_pluginsS3ObjectVersion,
    environment_airflowConfigurationOptions,
    environment_loggingConfiguration,
    environment_environmentClass,
    environment_networkConfiguration,
    environment_tags,
    environment_requirementsS3Path,
    environment_maxWorkers,

    -- ** LastUpdate
    lastUpdate_status,
    lastUpdate_createdAt,
    lastUpdate_error,

    -- ** LoggingConfiguration
    loggingConfiguration_taskLogs,
    loggingConfiguration_webserverLogs,
    loggingConfiguration_schedulerLogs,
    loggingConfiguration_dagProcessingLogs,
    loggingConfiguration_workerLogs,

    -- ** LoggingConfigurationInput
    loggingConfigurationInput_taskLogs,
    loggingConfigurationInput_webserverLogs,
    loggingConfigurationInput_schedulerLogs,
    loggingConfigurationInput_dagProcessingLogs,
    loggingConfigurationInput_workerLogs,

    -- ** MetricDatum
    metricDatum_value,
    metricDatum_dimensions,
    metricDatum_unit,
    metricDatum_statisticValues,
    metricDatum_metricName,
    metricDatum_timestamp,

    -- ** ModuleLoggingConfiguration
    moduleLoggingConfiguration_logLevel,
    moduleLoggingConfiguration_enabled,
    moduleLoggingConfiguration_cloudWatchLogGroupArn,

    -- ** ModuleLoggingConfigurationInput
    moduleLoggingConfigurationInput_enabled,
    moduleLoggingConfigurationInput_logLevel,

    -- ** NetworkConfiguration
    networkConfiguration_securityGroupIds,
    networkConfiguration_subnetIds,

    -- ** StatisticSet
    statisticSet_sampleCount,
    statisticSet_maximum,
    statisticSet_minimum,
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
