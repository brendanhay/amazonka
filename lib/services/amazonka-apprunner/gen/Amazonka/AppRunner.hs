{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppRunner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-05-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- App Runner
--
-- App Runner is an application service that provides a fast, simple, and
-- cost-effective way to go directly from an existing container image or
-- source code to a running service in the Amazon Web Services Cloud in
-- seconds. You don\'t need to learn new technologies, decide which compute
-- service to use, or understand how to provision and configure Amazon Web
-- Services resources.
--
-- App Runner connects directly to your container registry or source code
-- repository. It provides an automatic delivery pipeline with fully
-- managed operations, high performance, scalability, and security.
--
-- For more information about App Runner, see the
-- <https://docs.aws.amazon.com/apprunner/latest/dg/ App Runner Developer Guide>.
-- For release information, see the
-- <https://docs.aws.amazon.com/apprunner/latest/relnotes/ App Runner Release Notes>.
--
-- To install the Software Development Kits (SDKs), Integrated Development
-- Environment (IDE) Toolkits, and command line tools that you can use to
-- access the API, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
--
-- __Endpoints__
--
-- For a list of Region-specific endpoints that App Runner supports, see
-- <https://docs.aws.amazon.com/general/latest/gr/apprunner.html App Runner endpoints and quotas>
-- in the /Amazon Web Services General Reference/.
module Amazonka.AppRunner
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidStateException
    _InvalidStateException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateCustomDomain
    AssociateCustomDomain (AssociateCustomDomain'),
    newAssociateCustomDomain,
    AssociateCustomDomainResponse (AssociateCustomDomainResponse'),
    newAssociateCustomDomainResponse,

    -- ** CreateAutoScalingConfiguration
    CreateAutoScalingConfiguration (CreateAutoScalingConfiguration'),
    newCreateAutoScalingConfiguration,
    CreateAutoScalingConfigurationResponse (CreateAutoScalingConfigurationResponse'),
    newCreateAutoScalingConfigurationResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** CreateObservabilityConfiguration
    CreateObservabilityConfiguration (CreateObservabilityConfiguration'),
    newCreateObservabilityConfiguration,
    CreateObservabilityConfigurationResponse (CreateObservabilityConfigurationResponse'),
    newCreateObservabilityConfigurationResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- ** CreateVpcConnector
    CreateVpcConnector (CreateVpcConnector'),
    newCreateVpcConnector,
    CreateVpcConnectorResponse (CreateVpcConnectorResponse'),
    newCreateVpcConnectorResponse,

    -- ** CreateVpcIngressConnection
    CreateVpcIngressConnection (CreateVpcIngressConnection'),
    newCreateVpcIngressConnection,
    CreateVpcIngressConnectionResponse (CreateVpcIngressConnectionResponse'),
    newCreateVpcIngressConnectionResponse,

    -- ** DeleteAutoScalingConfiguration
    DeleteAutoScalingConfiguration (DeleteAutoScalingConfiguration'),
    newDeleteAutoScalingConfiguration,
    DeleteAutoScalingConfigurationResponse (DeleteAutoScalingConfigurationResponse'),
    newDeleteAutoScalingConfigurationResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DeleteObservabilityConfiguration
    DeleteObservabilityConfiguration (DeleteObservabilityConfiguration'),
    newDeleteObservabilityConfiguration,
    DeleteObservabilityConfigurationResponse (DeleteObservabilityConfigurationResponse'),
    newDeleteObservabilityConfigurationResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** DeleteVpcConnector
    DeleteVpcConnector (DeleteVpcConnector'),
    newDeleteVpcConnector,
    DeleteVpcConnectorResponse (DeleteVpcConnectorResponse'),
    newDeleteVpcConnectorResponse,

    -- ** DeleteVpcIngressConnection
    DeleteVpcIngressConnection (DeleteVpcIngressConnection'),
    newDeleteVpcIngressConnection,
    DeleteVpcIngressConnectionResponse (DeleteVpcIngressConnectionResponse'),
    newDeleteVpcIngressConnectionResponse,

    -- ** DescribeAutoScalingConfiguration
    DescribeAutoScalingConfiguration (DescribeAutoScalingConfiguration'),
    newDescribeAutoScalingConfiguration,
    DescribeAutoScalingConfigurationResponse (DescribeAutoScalingConfigurationResponse'),
    newDescribeAutoScalingConfigurationResponse,

    -- ** DescribeCustomDomains
    DescribeCustomDomains (DescribeCustomDomains'),
    newDescribeCustomDomains,
    DescribeCustomDomainsResponse (DescribeCustomDomainsResponse'),
    newDescribeCustomDomainsResponse,

    -- ** DescribeObservabilityConfiguration
    DescribeObservabilityConfiguration (DescribeObservabilityConfiguration'),
    newDescribeObservabilityConfiguration,
    DescribeObservabilityConfigurationResponse (DescribeObservabilityConfigurationResponse'),
    newDescribeObservabilityConfigurationResponse,

    -- ** DescribeService
    DescribeService (DescribeService'),
    newDescribeService,
    DescribeServiceResponse (DescribeServiceResponse'),
    newDescribeServiceResponse,

    -- ** DescribeVpcConnector
    DescribeVpcConnector (DescribeVpcConnector'),
    newDescribeVpcConnector,
    DescribeVpcConnectorResponse (DescribeVpcConnectorResponse'),
    newDescribeVpcConnectorResponse,

    -- ** DescribeVpcIngressConnection
    DescribeVpcIngressConnection (DescribeVpcIngressConnection'),
    newDescribeVpcIngressConnection,
    DescribeVpcIngressConnectionResponse (DescribeVpcIngressConnectionResponse'),
    newDescribeVpcIngressConnectionResponse,

    -- ** DisassociateCustomDomain
    DisassociateCustomDomain (DisassociateCustomDomain'),
    newDisassociateCustomDomain,
    DisassociateCustomDomainResponse (DisassociateCustomDomainResponse'),
    newDisassociateCustomDomainResponse,

    -- ** ListAutoScalingConfigurations
    ListAutoScalingConfigurations (ListAutoScalingConfigurations'),
    newListAutoScalingConfigurations,
    ListAutoScalingConfigurationsResponse (ListAutoScalingConfigurationsResponse'),
    newListAutoScalingConfigurationsResponse,

    -- ** ListConnections
    ListConnections (ListConnections'),
    newListConnections,
    ListConnectionsResponse (ListConnectionsResponse'),
    newListConnectionsResponse,

    -- ** ListObservabilityConfigurations
    ListObservabilityConfigurations (ListObservabilityConfigurations'),
    newListObservabilityConfigurations,
    ListObservabilityConfigurationsResponse (ListObservabilityConfigurationsResponse'),
    newListObservabilityConfigurationsResponse,

    -- ** ListOperations
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** ListServices
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVpcConnectors
    ListVpcConnectors (ListVpcConnectors'),
    newListVpcConnectors,
    ListVpcConnectorsResponse (ListVpcConnectorsResponse'),
    newListVpcConnectorsResponse,

    -- ** ListVpcIngressConnections
    ListVpcIngressConnections (ListVpcIngressConnections'),
    newListVpcIngressConnections,
    ListVpcIngressConnectionsResponse (ListVpcIngressConnectionsResponse'),
    newListVpcIngressConnectionsResponse,

    -- ** PauseService
    PauseService (PauseService'),
    newPauseService,
    PauseServiceResponse (PauseServiceResponse'),
    newPauseServiceResponse,

    -- ** ResumeService
    ResumeService (ResumeService'),
    newResumeService,
    ResumeServiceResponse (ResumeServiceResponse'),
    newResumeServiceResponse,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    StartDeploymentResponse (StartDeploymentResponse'),
    newStartDeploymentResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** UpdateVpcIngressConnection
    UpdateVpcIngressConnection (UpdateVpcIngressConnection'),
    newUpdateVpcIngressConnection,
    UpdateVpcIngressConnectionResponse (UpdateVpcIngressConnectionResponse'),
    newUpdateVpcIngressConnectionResponse,

    -- * Types

    -- ** AutoScalingConfigurationStatus
    AutoScalingConfigurationStatus (..),

    -- ** CertificateValidationRecordStatus
    CertificateValidationRecordStatus (..),

    -- ** ConfigurationSource
    ConfigurationSource (..),

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** CustomDomainAssociationStatus
    CustomDomainAssociationStatus (..),

    -- ** EgressType
    EgressType (..),

    -- ** HealthCheckProtocol
    HealthCheckProtocol (..),

    -- ** ImageRepositoryType
    ImageRepositoryType (..),

    -- ** ObservabilityConfigurationStatus
    ObservabilityConfigurationStatus (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** OperationType
    OperationType (..),

    -- ** ProviderType
    ProviderType (..),

    -- ** Runtime
    Runtime (..),

    -- ** ServiceStatus
    ServiceStatus (..),

    -- ** SourceCodeVersionType
    SourceCodeVersionType (..),

    -- ** TracingVendor
    TracingVendor (..),

    -- ** VpcConnectorStatus
    VpcConnectorStatus (..),

    -- ** VpcIngressConnectionStatus
    VpcIngressConnectionStatus (..),

    -- ** AuthenticationConfiguration
    AuthenticationConfiguration (AuthenticationConfiguration'),
    newAuthenticationConfiguration,

    -- ** AutoScalingConfiguration
    AutoScalingConfiguration (AutoScalingConfiguration'),
    newAutoScalingConfiguration,

    -- ** AutoScalingConfigurationSummary
    AutoScalingConfigurationSummary (AutoScalingConfigurationSummary'),
    newAutoScalingConfigurationSummary,

    -- ** CertificateValidationRecord
    CertificateValidationRecord (CertificateValidationRecord'),
    newCertificateValidationRecord,

    -- ** CodeConfiguration
    CodeConfiguration (CodeConfiguration'),
    newCodeConfiguration,

    -- ** CodeConfigurationValues
    CodeConfigurationValues (CodeConfigurationValues'),
    newCodeConfigurationValues,

    -- ** CodeRepository
    CodeRepository (CodeRepository'),
    newCodeRepository,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** ConnectionSummary
    ConnectionSummary (ConnectionSummary'),
    newConnectionSummary,

    -- ** CustomDomain
    CustomDomain (CustomDomain'),
    newCustomDomain,

    -- ** EgressConfiguration
    EgressConfiguration (EgressConfiguration'),
    newEgressConfiguration,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** HealthCheckConfiguration
    HealthCheckConfiguration (HealthCheckConfiguration'),
    newHealthCheckConfiguration,

    -- ** ImageConfiguration
    ImageConfiguration (ImageConfiguration'),
    newImageConfiguration,

    -- ** ImageRepository
    ImageRepository (ImageRepository'),
    newImageRepository,

    -- ** IngressConfiguration
    IngressConfiguration (IngressConfiguration'),
    newIngressConfiguration,

    -- ** IngressVpcConfiguration
    IngressVpcConfiguration (IngressVpcConfiguration'),
    newIngressVpcConfiguration,

    -- ** InstanceConfiguration
    InstanceConfiguration (InstanceConfiguration'),
    newInstanceConfiguration,

    -- ** ListVpcIngressConnectionsFilter
    ListVpcIngressConnectionsFilter (ListVpcIngressConnectionsFilter'),
    newListVpcIngressConnectionsFilter,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** ObservabilityConfiguration
    ObservabilityConfiguration (ObservabilityConfiguration'),
    newObservabilityConfiguration,

    -- ** ObservabilityConfigurationSummary
    ObservabilityConfigurationSummary (ObservabilityConfigurationSummary'),
    newObservabilityConfigurationSummary,

    -- ** OperationSummary
    OperationSummary (OperationSummary'),
    newOperationSummary,

    -- ** Service
    Service (Service'),
    newService,

    -- ** ServiceObservabilityConfiguration
    ServiceObservabilityConfiguration (ServiceObservabilityConfiguration'),
    newServiceObservabilityConfiguration,

    -- ** ServiceSummary
    ServiceSummary (ServiceSummary'),
    newServiceSummary,

    -- ** SourceCodeVersion
    SourceCodeVersion (SourceCodeVersion'),
    newSourceCodeVersion,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TraceConfiguration
    TraceConfiguration (TraceConfiguration'),
    newTraceConfiguration,

    -- ** VpcConnector
    VpcConnector (VpcConnector'),
    newVpcConnector,

    -- ** VpcDNSTarget
    VpcDNSTarget (VpcDNSTarget'),
    newVpcDNSTarget,

    -- ** VpcIngressConnection
    VpcIngressConnection (VpcIngressConnection'),
    newVpcIngressConnection,

    -- ** VpcIngressConnectionSummary
    VpcIngressConnectionSummary (VpcIngressConnectionSummary'),
    newVpcIngressConnectionSummary,
  )
where

import Amazonka.AppRunner.AssociateCustomDomain
import Amazonka.AppRunner.CreateAutoScalingConfiguration
import Amazonka.AppRunner.CreateConnection
import Amazonka.AppRunner.CreateObservabilityConfiguration
import Amazonka.AppRunner.CreateService
import Amazonka.AppRunner.CreateVpcConnector
import Amazonka.AppRunner.CreateVpcIngressConnection
import Amazonka.AppRunner.DeleteAutoScalingConfiguration
import Amazonka.AppRunner.DeleteConnection
import Amazonka.AppRunner.DeleteObservabilityConfiguration
import Amazonka.AppRunner.DeleteService
import Amazonka.AppRunner.DeleteVpcConnector
import Amazonka.AppRunner.DeleteVpcIngressConnection
import Amazonka.AppRunner.DescribeAutoScalingConfiguration
import Amazonka.AppRunner.DescribeCustomDomains
import Amazonka.AppRunner.DescribeObservabilityConfiguration
import Amazonka.AppRunner.DescribeService
import Amazonka.AppRunner.DescribeVpcConnector
import Amazonka.AppRunner.DescribeVpcIngressConnection
import Amazonka.AppRunner.DisassociateCustomDomain
import Amazonka.AppRunner.Lens
import Amazonka.AppRunner.ListAutoScalingConfigurations
import Amazonka.AppRunner.ListConnections
import Amazonka.AppRunner.ListObservabilityConfigurations
import Amazonka.AppRunner.ListOperations
import Amazonka.AppRunner.ListServices
import Amazonka.AppRunner.ListTagsForResource
import Amazonka.AppRunner.ListVpcConnectors
import Amazonka.AppRunner.ListVpcIngressConnections
import Amazonka.AppRunner.PauseService
import Amazonka.AppRunner.ResumeService
import Amazonka.AppRunner.StartDeployment
import Amazonka.AppRunner.TagResource
import Amazonka.AppRunner.Types
import Amazonka.AppRunner.UntagResource
import Amazonka.AppRunner.UpdateService
import Amazonka.AppRunner.UpdateVpcIngressConnection
import Amazonka.AppRunner.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppRunner'.

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
