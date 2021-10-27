{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.AppRunner
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.AppRunner
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidStateException
    _InvalidStateException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListServices
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ResumeService
    ResumeService (ResumeService'),
    newResumeService,
    ResumeServiceResponse (ResumeServiceResponse'),
    newResumeServiceResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** ListOperations
    ListOperations (ListOperations'),
    newListOperations,
    ListOperationsResponse (ListOperationsResponse'),
    newListOperationsResponse,

    -- ** AssociateCustomDomain
    AssociateCustomDomain (AssociateCustomDomain'),
    newAssociateCustomDomain,
    AssociateCustomDomainResponse (AssociateCustomDomainResponse'),
    newAssociateCustomDomainResponse,

    -- ** ListConnections
    ListConnections (ListConnections'),
    newListConnections,
    ListConnectionsResponse (ListConnectionsResponse'),
    newListConnectionsResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DescribeAutoScalingConfiguration
    DescribeAutoScalingConfiguration (DescribeAutoScalingConfiguration'),
    newDescribeAutoScalingConfiguration,
    DescribeAutoScalingConfigurationResponse (DescribeAutoScalingConfigurationResponse'),
    newDescribeAutoScalingConfigurationResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** DescribeCustomDomains
    DescribeCustomDomains (DescribeCustomDomains'),
    newDescribeCustomDomains,
    DescribeCustomDomainsResponse (DescribeCustomDomainsResponse'),
    newDescribeCustomDomainsResponse,

    -- ** DescribeService
    DescribeService (DescribeService'),
    newDescribeService,
    DescribeServiceResponse (DescribeServiceResponse'),
    newDescribeServiceResponse,

    -- ** DeleteAutoScalingConfiguration
    DeleteAutoScalingConfiguration (DeleteAutoScalingConfiguration'),
    newDeleteAutoScalingConfiguration,
    DeleteAutoScalingConfigurationResponse (DeleteAutoScalingConfigurationResponse'),
    newDeleteAutoScalingConfigurationResponse,

    -- ** ListAutoScalingConfigurations
    ListAutoScalingConfigurations (ListAutoScalingConfigurations'),
    newListAutoScalingConfigurations,
    ListAutoScalingConfigurationsResponse (ListAutoScalingConfigurationsResponse'),
    newListAutoScalingConfigurationsResponse,

    -- ** DisassociateCustomDomain
    DisassociateCustomDomain (DisassociateCustomDomain'),
    newDisassociateCustomDomain,
    DisassociateCustomDomainResponse (DisassociateCustomDomainResponse'),
    newDisassociateCustomDomainResponse,

    -- ** PauseService
    PauseService (PauseService'),
    newPauseService,
    PauseServiceResponse (PauseServiceResponse'),
    newPauseServiceResponse,

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

    -- ** CreateAutoScalingConfiguration
    CreateAutoScalingConfiguration (CreateAutoScalingConfiguration'),
    newCreateAutoScalingConfiguration,
    CreateAutoScalingConfigurationResponse (CreateAutoScalingConfigurationResponse'),
    newCreateAutoScalingConfigurationResponse,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    StartDeploymentResponse (StartDeploymentResponse'),
    newStartDeploymentResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

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

    -- ** HealthCheckProtocol
    HealthCheckProtocol (..),

    -- ** ImageRepositoryType
    ImageRepositoryType (..),

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

    -- ** InstanceConfiguration
    InstanceConfiguration (InstanceConfiguration'),
    newInstanceConfiguration,

    -- ** OperationSummary
    OperationSummary (OperationSummary'),
    newOperationSummary,

    -- ** Service
    Service (Service'),
    newService,

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
  )
where

import Network.AWS.AppRunner.AssociateCustomDomain
import Network.AWS.AppRunner.CreateAutoScalingConfiguration
import Network.AWS.AppRunner.CreateConnection
import Network.AWS.AppRunner.CreateService
import Network.AWS.AppRunner.DeleteAutoScalingConfiguration
import Network.AWS.AppRunner.DeleteConnection
import Network.AWS.AppRunner.DeleteService
import Network.AWS.AppRunner.DescribeAutoScalingConfiguration
import Network.AWS.AppRunner.DescribeCustomDomains
import Network.AWS.AppRunner.DescribeService
import Network.AWS.AppRunner.DisassociateCustomDomain
import Network.AWS.AppRunner.Lens
import Network.AWS.AppRunner.ListAutoScalingConfigurations
import Network.AWS.AppRunner.ListConnections
import Network.AWS.AppRunner.ListOperations
import Network.AWS.AppRunner.ListServices
import Network.AWS.AppRunner.ListTagsForResource
import Network.AWS.AppRunner.PauseService
import Network.AWS.AppRunner.ResumeService
import Network.AWS.AppRunner.StartDeployment
import Network.AWS.AppRunner.TagResource
import Network.AWS.AppRunner.Types
import Network.AWS.AppRunner.UntagResource
import Network.AWS.AppRunner.UpdateService
import Network.AWS.AppRunner.Waiters

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
