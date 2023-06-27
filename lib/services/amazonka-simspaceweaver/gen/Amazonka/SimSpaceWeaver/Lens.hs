{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SimSpaceWeaver.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Lens
  ( -- * Operations

    -- ** CreateSnapshot
    createSnapshot_destination,
    createSnapshot_simulation,
    createSnapshotResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_app,
    deleteApp_domain,
    deleteApp_simulation,
    deleteAppResponse_httpStatus,

    -- ** DeleteSimulation
    deleteSimulation_simulation,
    deleteSimulationResponse_httpStatus,

    -- ** DescribeApp
    describeApp_app,
    describeApp_domain,
    describeApp_simulation,
    describeAppResponse_description,
    describeAppResponse_domain,
    describeAppResponse_endpointInfo,
    describeAppResponse_launchOverrides,
    describeAppResponse_name,
    describeAppResponse_simulation,
    describeAppResponse_status,
    describeAppResponse_targetStatus,
    describeAppResponse_httpStatus,

    -- ** DescribeSimulation
    describeSimulation_simulation,
    describeSimulationResponse_arn,
    describeSimulationResponse_creationTime,
    describeSimulationResponse_description,
    describeSimulationResponse_executionId,
    describeSimulationResponse_liveSimulationState,
    describeSimulationResponse_loggingConfiguration,
    describeSimulationResponse_maximumDuration,
    describeSimulationResponse_name,
    describeSimulationResponse_roleArn,
    describeSimulationResponse_schemaError,
    describeSimulationResponse_schemaS3Location,
    describeSimulationResponse_snapshotS3Location,
    describeSimulationResponse_startError,
    describeSimulationResponse_status,
    describeSimulationResponse_targetStatus,
    describeSimulationResponse_httpStatus,

    -- ** ListApps
    listApps_domain,
    listApps_maxResults,
    listApps_nextToken,
    listApps_simulation,
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,

    -- ** ListSimulations
    listSimulations_maxResults,
    listSimulations_nextToken,
    listSimulationsResponse_nextToken,
    listSimulationsResponse_simulations,
    listSimulationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartApp
    startApp_clientToken,
    startApp_description,
    startApp_launchOverrides,
    startApp_domain,
    startApp_name,
    startApp_simulation,
    startAppResponse_domain,
    startAppResponse_name,
    startAppResponse_simulation,
    startAppResponse_httpStatus,

    -- ** StartClock
    startClock_simulation,
    startClockResponse_httpStatus,

    -- ** StartSimulation
    startSimulation_clientToken,
    startSimulation_description,
    startSimulation_maximumDuration,
    startSimulation_schemaS3Location,
    startSimulation_snapshotS3Location,
    startSimulation_tags,
    startSimulation_name,
    startSimulation_roleArn,
    startSimulationResponse_arn,
    startSimulationResponse_creationTime,
    startSimulationResponse_executionId,
    startSimulationResponse_httpStatus,

    -- ** StopApp
    stopApp_app,
    stopApp_domain,
    stopApp_simulation,
    stopAppResponse_httpStatus,

    -- ** StopClock
    stopClock_simulation,
    stopClockResponse_httpStatus,

    -- ** StopSimulation
    stopSimulation_simulation,
    stopSimulationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** CloudWatchLogsLogGroup
    cloudWatchLogsLogGroup_logGroupArn,

    -- ** Domain
    domain_lifecycle,
    domain_name,

    -- ** LaunchOverrides
    launchOverrides_launchCommands,

    -- ** LiveSimulationState
    liveSimulationState_clocks,
    liveSimulationState_domains,

    -- ** LogDestination
    logDestination_cloudWatchLogsLogGroup,

    -- ** LoggingConfiguration
    loggingConfiguration_destinations,

    -- ** S3Destination
    s3Destination_bucketName,
    s3Destination_objectKeyPrefix,

    -- ** S3Location
    s3Location_bucketName,
    s3Location_objectKey,

    -- ** SimulationAppEndpointInfo
    simulationAppEndpointInfo_address,
    simulationAppEndpointInfo_ingressPortMappings,

    -- ** SimulationAppMetadata
    simulationAppMetadata_domain,
    simulationAppMetadata_name,
    simulationAppMetadata_simulation,
    simulationAppMetadata_status,
    simulationAppMetadata_targetStatus,

    -- ** SimulationAppPortMapping
    simulationAppPortMapping_actual,
    simulationAppPortMapping_declared,

    -- ** SimulationClock
    simulationClock_status,
    simulationClock_targetStatus,

    -- ** SimulationMetadata
    simulationMetadata_arn,
    simulationMetadata_creationTime,
    simulationMetadata_name,
    simulationMetadata_status,
    simulationMetadata_targetStatus,
  )
where

import Amazonka.SimSpaceWeaver.CreateSnapshot
import Amazonka.SimSpaceWeaver.DeleteApp
import Amazonka.SimSpaceWeaver.DeleteSimulation
import Amazonka.SimSpaceWeaver.DescribeApp
import Amazonka.SimSpaceWeaver.DescribeSimulation
import Amazonka.SimSpaceWeaver.ListApps
import Amazonka.SimSpaceWeaver.ListSimulations
import Amazonka.SimSpaceWeaver.ListTagsForResource
import Amazonka.SimSpaceWeaver.StartApp
import Amazonka.SimSpaceWeaver.StartClock
import Amazonka.SimSpaceWeaver.StartSimulation
import Amazonka.SimSpaceWeaver.StopApp
import Amazonka.SimSpaceWeaver.StopClock
import Amazonka.SimSpaceWeaver.StopSimulation
import Amazonka.SimSpaceWeaver.TagResource
import Amazonka.SimSpaceWeaver.Types.CloudWatchLogsLogGroup
import Amazonka.SimSpaceWeaver.Types.Domain
import Amazonka.SimSpaceWeaver.Types.LaunchOverrides
import Amazonka.SimSpaceWeaver.Types.LiveSimulationState
import Amazonka.SimSpaceWeaver.Types.LogDestination
import Amazonka.SimSpaceWeaver.Types.LoggingConfiguration
import Amazonka.SimSpaceWeaver.Types.S3Destination
import Amazonka.SimSpaceWeaver.Types.S3Location
import Amazonka.SimSpaceWeaver.Types.SimulationAppEndpointInfo
import Amazonka.SimSpaceWeaver.Types.SimulationAppMetadata
import Amazonka.SimSpaceWeaver.Types.SimulationAppPortMapping
import Amazonka.SimSpaceWeaver.Types.SimulationClock
import Amazonka.SimSpaceWeaver.Types.SimulationMetadata
import Amazonka.SimSpaceWeaver.UntagResource
