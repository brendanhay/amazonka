{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KafkaConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-09-14@ of the AWS service descriptions, licensed under Apache 2.0.
module Amazonka.KafkaConnect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConnector
    CreateConnector (CreateConnector'),
    newCreateConnector,
    CreateConnectorResponse (CreateConnectorResponse'),
    newCreateConnectorResponse,

    -- ** CreateCustomPlugin
    CreateCustomPlugin (CreateCustomPlugin'),
    newCreateCustomPlugin,
    CreateCustomPluginResponse (CreateCustomPluginResponse'),
    newCreateCustomPluginResponse,

    -- ** CreateWorkerConfiguration
    CreateWorkerConfiguration (CreateWorkerConfiguration'),
    newCreateWorkerConfiguration,
    CreateWorkerConfigurationResponse (CreateWorkerConfigurationResponse'),
    newCreateWorkerConfigurationResponse,

    -- ** DeleteConnector
    DeleteConnector (DeleteConnector'),
    newDeleteConnector,
    DeleteConnectorResponse (DeleteConnectorResponse'),
    newDeleteConnectorResponse,

    -- ** DeleteCustomPlugin
    DeleteCustomPlugin (DeleteCustomPlugin'),
    newDeleteCustomPlugin,
    DeleteCustomPluginResponse (DeleteCustomPluginResponse'),
    newDeleteCustomPluginResponse,

    -- ** DescribeConnector
    DescribeConnector (DescribeConnector'),
    newDescribeConnector,
    DescribeConnectorResponse (DescribeConnectorResponse'),
    newDescribeConnectorResponse,

    -- ** DescribeCustomPlugin
    DescribeCustomPlugin (DescribeCustomPlugin'),
    newDescribeCustomPlugin,
    DescribeCustomPluginResponse (DescribeCustomPluginResponse'),
    newDescribeCustomPluginResponse,

    -- ** DescribeWorkerConfiguration
    DescribeWorkerConfiguration (DescribeWorkerConfiguration'),
    newDescribeWorkerConfiguration,
    DescribeWorkerConfigurationResponse (DescribeWorkerConfigurationResponse'),
    newDescribeWorkerConfigurationResponse,

    -- ** ListConnectors (Paginated)
    ListConnectors (ListConnectors'),
    newListConnectors,
    ListConnectorsResponse (ListConnectorsResponse'),
    newListConnectorsResponse,

    -- ** ListCustomPlugins (Paginated)
    ListCustomPlugins (ListCustomPlugins'),
    newListCustomPlugins,
    ListCustomPluginsResponse (ListCustomPluginsResponse'),
    newListCustomPluginsResponse,

    -- ** ListWorkerConfigurations (Paginated)
    ListWorkerConfigurations (ListWorkerConfigurations'),
    newListWorkerConfigurations,
    ListWorkerConfigurationsResponse (ListWorkerConfigurationsResponse'),
    newListWorkerConfigurationsResponse,

    -- ** UpdateConnector
    UpdateConnector (UpdateConnector'),
    newUpdateConnector,
    UpdateConnectorResponse (UpdateConnectorResponse'),
    newUpdateConnectorResponse,

    -- * Types

    -- ** ConnectorState
    ConnectorState (..),

    -- ** CustomPluginContentType
    CustomPluginContentType (..),

    -- ** CustomPluginState
    CustomPluginState (..),

    -- ** KafkaClusterClientAuthenticationType
    KafkaClusterClientAuthenticationType (..),

    -- ** KafkaClusterEncryptionInTransitType
    KafkaClusterEncryptionInTransitType (..),

    -- ** ApacheKafkaCluster
    ApacheKafkaCluster (ApacheKafkaCluster'),
    newApacheKafkaCluster,

    -- ** ApacheKafkaClusterDescription
    ApacheKafkaClusterDescription (ApacheKafkaClusterDescription'),
    newApacheKafkaClusterDescription,

    -- ** AutoScaling
    AutoScaling (AutoScaling'),
    newAutoScaling,

    -- ** AutoScalingDescription
    AutoScalingDescription (AutoScalingDescription'),
    newAutoScalingDescription,

    -- ** AutoScalingUpdate
    AutoScalingUpdate (AutoScalingUpdate'),
    newAutoScalingUpdate,

    -- ** Capacity
    Capacity (Capacity'),
    newCapacity,

    -- ** CapacityDescription
    CapacityDescription (CapacityDescription'),
    newCapacityDescription,

    -- ** CapacityUpdate
    CapacityUpdate (CapacityUpdate'),
    newCapacityUpdate,

    -- ** CloudWatchLogsLogDelivery
    CloudWatchLogsLogDelivery (CloudWatchLogsLogDelivery'),
    newCloudWatchLogsLogDelivery,

    -- ** CloudWatchLogsLogDeliveryDescription
    CloudWatchLogsLogDeliveryDescription (CloudWatchLogsLogDeliveryDescription'),
    newCloudWatchLogsLogDeliveryDescription,

    -- ** ConnectorSummary
    ConnectorSummary (ConnectorSummary'),
    newConnectorSummary,

    -- ** CustomPlugin
    CustomPlugin (CustomPlugin'),
    newCustomPlugin,

    -- ** CustomPluginDescription
    CustomPluginDescription (CustomPluginDescription'),
    newCustomPluginDescription,

    -- ** CustomPluginFileDescription
    CustomPluginFileDescription (CustomPluginFileDescription'),
    newCustomPluginFileDescription,

    -- ** CustomPluginLocation
    CustomPluginLocation (CustomPluginLocation'),
    newCustomPluginLocation,

    -- ** CustomPluginLocationDescription
    CustomPluginLocationDescription (CustomPluginLocationDescription'),
    newCustomPluginLocationDescription,

    -- ** CustomPluginRevisionSummary
    CustomPluginRevisionSummary (CustomPluginRevisionSummary'),
    newCustomPluginRevisionSummary,

    -- ** CustomPluginSummary
    CustomPluginSummary (CustomPluginSummary'),
    newCustomPluginSummary,

    -- ** FirehoseLogDelivery
    FirehoseLogDelivery (FirehoseLogDelivery'),
    newFirehoseLogDelivery,

    -- ** FirehoseLogDeliveryDescription
    FirehoseLogDeliveryDescription (FirehoseLogDeliveryDescription'),
    newFirehoseLogDeliveryDescription,

    -- ** KafkaCluster
    KafkaCluster (KafkaCluster'),
    newKafkaCluster,

    -- ** KafkaClusterClientAuthentication
    KafkaClusterClientAuthentication (KafkaClusterClientAuthentication'),
    newKafkaClusterClientAuthentication,

    -- ** KafkaClusterClientAuthenticationDescription
    KafkaClusterClientAuthenticationDescription (KafkaClusterClientAuthenticationDescription'),
    newKafkaClusterClientAuthenticationDescription,

    -- ** KafkaClusterDescription
    KafkaClusterDescription (KafkaClusterDescription'),
    newKafkaClusterDescription,

    -- ** KafkaClusterEncryptionInTransit
    KafkaClusterEncryptionInTransit (KafkaClusterEncryptionInTransit'),
    newKafkaClusterEncryptionInTransit,

    -- ** KafkaClusterEncryptionInTransitDescription
    KafkaClusterEncryptionInTransitDescription (KafkaClusterEncryptionInTransitDescription'),
    newKafkaClusterEncryptionInTransitDescription,

    -- ** LogDelivery
    LogDelivery (LogDelivery'),
    newLogDelivery,

    -- ** LogDeliveryDescription
    LogDeliveryDescription (LogDeliveryDescription'),
    newLogDeliveryDescription,

    -- ** Plugin
    Plugin (Plugin'),
    newPlugin,

    -- ** PluginDescription
    PluginDescription (PluginDescription'),
    newPluginDescription,

    -- ** ProvisionedCapacity
    ProvisionedCapacity (ProvisionedCapacity'),
    newProvisionedCapacity,

    -- ** ProvisionedCapacityDescription
    ProvisionedCapacityDescription (ProvisionedCapacityDescription'),
    newProvisionedCapacityDescription,

    -- ** ProvisionedCapacityUpdate
    ProvisionedCapacityUpdate (ProvisionedCapacityUpdate'),
    newProvisionedCapacityUpdate,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** S3LocationDescription
    S3LocationDescription (S3LocationDescription'),
    newS3LocationDescription,

    -- ** S3LogDelivery
    S3LogDelivery (S3LogDelivery'),
    newS3LogDelivery,

    -- ** S3LogDeliveryDescription
    S3LogDeliveryDescription (S3LogDeliveryDescription'),
    newS3LogDeliveryDescription,

    -- ** ScaleInPolicy
    ScaleInPolicy (ScaleInPolicy'),
    newScaleInPolicy,

    -- ** ScaleInPolicyDescription
    ScaleInPolicyDescription (ScaleInPolicyDescription'),
    newScaleInPolicyDescription,

    -- ** ScaleInPolicyUpdate
    ScaleInPolicyUpdate (ScaleInPolicyUpdate'),
    newScaleInPolicyUpdate,

    -- ** ScaleOutPolicy
    ScaleOutPolicy (ScaleOutPolicy'),
    newScaleOutPolicy,

    -- ** ScaleOutPolicyDescription
    ScaleOutPolicyDescription (ScaleOutPolicyDescription'),
    newScaleOutPolicyDescription,

    -- ** ScaleOutPolicyUpdate
    ScaleOutPolicyUpdate (ScaleOutPolicyUpdate'),
    newScaleOutPolicyUpdate,

    -- ** StateDescription
    StateDescription (StateDescription'),
    newStateDescription,

    -- ** Vpc
    Vpc (Vpc'),
    newVpc,

    -- ** VpcDescription
    VpcDescription (VpcDescription'),
    newVpcDescription,

    -- ** WorkerConfiguration
    WorkerConfiguration (WorkerConfiguration'),
    newWorkerConfiguration,

    -- ** WorkerConfigurationDescription
    WorkerConfigurationDescription (WorkerConfigurationDescription'),
    newWorkerConfigurationDescription,

    -- ** WorkerConfigurationRevisionDescription
    WorkerConfigurationRevisionDescription (WorkerConfigurationRevisionDescription'),
    newWorkerConfigurationRevisionDescription,

    -- ** WorkerConfigurationRevisionSummary
    WorkerConfigurationRevisionSummary (WorkerConfigurationRevisionSummary'),
    newWorkerConfigurationRevisionSummary,

    -- ** WorkerConfigurationSummary
    WorkerConfigurationSummary (WorkerConfigurationSummary'),
    newWorkerConfigurationSummary,

    -- ** WorkerLogDelivery
    WorkerLogDelivery (WorkerLogDelivery'),
    newWorkerLogDelivery,

    -- ** WorkerLogDeliveryDescription
    WorkerLogDeliveryDescription (WorkerLogDeliveryDescription'),
    newWorkerLogDeliveryDescription,
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
import Amazonka.KafkaConnect.Lens
import Amazonka.KafkaConnect.ListConnectors
import Amazonka.KafkaConnect.ListCustomPlugins
import Amazonka.KafkaConnect.ListWorkerConfigurations
import Amazonka.KafkaConnect.Types
import Amazonka.KafkaConnect.UpdateConnector
import Amazonka.KafkaConnect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KafkaConnect'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
