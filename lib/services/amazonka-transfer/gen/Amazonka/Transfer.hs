{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Transfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Transfer Family is a fully managed service that
-- enables the transfer of files over the File Transfer Protocol (FTP),
-- File Transfer Protocol over SSL (FTPS), or Secure Shell (SSH) File
-- Transfer Protocol (SFTP) directly into and out of Amazon Simple Storage
-- Service (Amazon S3). Amazon Web Services helps you seamlessly migrate
-- your file transfer workflows to Amazon Web Services Transfer Family by
-- integrating with existing authentication systems, and providing DNS
-- routing with Amazon Route 53 so nothing changes for your customers and
-- partners, or their applications. With your data in Amazon S3, you can
-- use it with Amazon Web Services services for processing, analytics,
-- machine learning, and archiving. Getting started with Amazon Web
-- Services Transfer Family is easy since there is no infrastructure to buy
-- and set up.
module Amazonka.Transfer
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InternalServiceError
    _InternalServiceError,

    -- ** ConflictException
    _ConflictException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ResourceExistsException
    _ResourceExistsException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAccess
    CreateAccess (CreateAccess'),
    newCreateAccess,
    CreateAccessResponse (CreateAccessResponse'),
    newCreateAccessResponse,

    -- ** CreateServer
    CreateServer (CreateServer'),
    newCreateServer,
    CreateServerResponse (CreateServerResponse'),
    newCreateServerResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** CreateWorkflow
    CreateWorkflow (CreateWorkflow'),
    newCreateWorkflow,
    CreateWorkflowResponse (CreateWorkflowResponse'),
    newCreateWorkflowResponse,

    -- ** DeleteAccess
    DeleteAccess (DeleteAccess'),
    newDeleteAccess,
    DeleteAccessResponse (DeleteAccessResponse'),
    newDeleteAccessResponse,

    -- ** DeleteServer
    DeleteServer (DeleteServer'),
    newDeleteServer,
    DeleteServerResponse (DeleteServerResponse'),
    newDeleteServerResponse,

    -- ** DeleteSshPublicKey
    DeleteSshPublicKey (DeleteSshPublicKey'),
    newDeleteSshPublicKey,
    DeleteSshPublicKeyResponse (DeleteSshPublicKeyResponse'),
    newDeleteSshPublicKeyResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DeleteWorkflow
    DeleteWorkflow (DeleteWorkflow'),
    newDeleteWorkflow,
    DeleteWorkflowResponse (DeleteWorkflowResponse'),
    newDeleteWorkflowResponse,

    -- ** DescribeAccess
    DescribeAccess (DescribeAccess'),
    newDescribeAccess,
    DescribeAccessResponse (DescribeAccessResponse'),
    newDescribeAccessResponse,

    -- ** DescribeExecution
    DescribeExecution (DescribeExecution'),
    newDescribeExecution,
    DescribeExecutionResponse (DescribeExecutionResponse'),
    newDescribeExecutionResponse,

    -- ** DescribeSecurityPolicy
    DescribeSecurityPolicy (DescribeSecurityPolicy'),
    newDescribeSecurityPolicy,
    DescribeSecurityPolicyResponse (DescribeSecurityPolicyResponse'),
    newDescribeSecurityPolicyResponse,

    -- ** DescribeServer
    DescribeServer (DescribeServer'),
    newDescribeServer,
    DescribeServerResponse (DescribeServerResponse'),
    newDescribeServerResponse,

    -- ** DescribeUser
    DescribeUser (DescribeUser'),
    newDescribeUser,
    DescribeUserResponse (DescribeUserResponse'),
    newDescribeUserResponse,

    -- ** DescribeWorkflow
    DescribeWorkflow (DescribeWorkflow'),
    newDescribeWorkflow,
    DescribeWorkflowResponse (DescribeWorkflowResponse'),
    newDescribeWorkflowResponse,

    -- ** ImportSshPublicKey
    ImportSshPublicKey (ImportSshPublicKey'),
    newImportSshPublicKey,
    ImportSshPublicKeyResponse (ImportSshPublicKeyResponse'),
    newImportSshPublicKeyResponse,

    -- ** ListAccesses
    ListAccesses (ListAccesses'),
    newListAccesses,
    ListAccessesResponse (ListAccessesResponse'),
    newListAccessesResponse,

    -- ** ListExecutions
    ListExecutions (ListExecutions'),
    newListExecutions,
    ListExecutionsResponse (ListExecutionsResponse'),
    newListExecutionsResponse,

    -- ** ListSecurityPolicies
    ListSecurityPolicies (ListSecurityPolicies'),
    newListSecurityPolicies,
    ListSecurityPoliciesResponse (ListSecurityPoliciesResponse'),
    newListSecurityPoliciesResponse,

    -- ** ListServers (Paginated)
    ListServers (ListServers'),
    newListServers,
    ListServersResponse (ListServersResponse'),
    newListServersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUsers
    ListUsers (ListUsers'),
    newListUsers,
    ListUsersResponse (ListUsersResponse'),
    newListUsersResponse,

    -- ** ListWorkflows
    ListWorkflows (ListWorkflows'),
    newListWorkflows,
    ListWorkflowsResponse (ListWorkflowsResponse'),
    newListWorkflowsResponse,

    -- ** SendWorkflowStepState
    SendWorkflowStepState (SendWorkflowStepState'),
    newSendWorkflowStepState,
    SendWorkflowStepStateResponse (SendWorkflowStepStateResponse'),
    newSendWorkflowStepStateResponse,

    -- ** StartServer
    StartServer (StartServer'),
    newStartServer,
    StartServerResponse (StartServerResponse'),
    newStartServerResponse,

    -- ** StopServer
    StopServer (StopServer'),
    newStopServer,
    StopServerResponse (StopServerResponse'),
    newStopServerResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestIdentityProvider
    TestIdentityProvider (TestIdentityProvider'),
    newTestIdentityProvider,
    TestIdentityProviderResponse (TestIdentityProviderResponse'),
    newTestIdentityProviderResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccess
    UpdateAccess (UpdateAccess'),
    newUpdateAccess,
    UpdateAccessResponse (UpdateAccessResponse'),
    newUpdateAccessResponse,

    -- ** UpdateServer
    UpdateServer (UpdateServer'),
    newUpdateServer,
    UpdateServerResponse (UpdateServerResponse'),
    newUpdateServerResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- * Types

    -- ** CustomStepStatus
    CustomStepStatus (..),

    -- ** Domain
    Domain (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** ExecutionErrorType
    ExecutionErrorType (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** HomeDirectoryType
    HomeDirectoryType (..),

    -- ** IdentityProviderType
    IdentityProviderType (..),

    -- ** OverwriteExisting
    OverwriteExisting (..),

    -- ** Protocol
    Protocol (..),

    -- ** State
    State (..),

    -- ** WorkflowStepType
    WorkflowStepType (..),

    -- ** CopyStepDetails
    CopyStepDetails (CopyStepDetails'),
    newCopyStepDetails,

    -- ** CustomStepDetails
    CustomStepDetails (CustomStepDetails'),
    newCustomStepDetails,

    -- ** DeleteStepDetails
    DeleteStepDetails (DeleteStepDetails'),
    newDeleteStepDetails,

    -- ** DescribedAccess
    DescribedAccess (DescribedAccess'),
    newDescribedAccess,

    -- ** DescribedExecution
    DescribedExecution (DescribedExecution'),
    newDescribedExecution,

    -- ** DescribedSecurityPolicy
    DescribedSecurityPolicy (DescribedSecurityPolicy'),
    newDescribedSecurityPolicy,

    -- ** DescribedServer
    DescribedServer (DescribedServer'),
    newDescribedServer,

    -- ** DescribedUser
    DescribedUser (DescribedUser'),
    newDescribedUser,

    -- ** DescribedWorkflow
    DescribedWorkflow (DescribedWorkflow'),
    newDescribedWorkflow,

    -- ** EfsFileLocation
    EfsFileLocation (EfsFileLocation'),
    newEfsFileLocation,

    -- ** EndpointDetails
    EndpointDetails (EndpointDetails'),
    newEndpointDetails,

    -- ** ExecutionError
    ExecutionError (ExecutionError'),
    newExecutionError,

    -- ** ExecutionResults
    ExecutionResults (ExecutionResults'),
    newExecutionResults,

    -- ** ExecutionStepResult
    ExecutionStepResult (ExecutionStepResult'),
    newExecutionStepResult,

    -- ** FileLocation
    FileLocation (FileLocation'),
    newFileLocation,

    -- ** HomeDirectoryMapEntry
    HomeDirectoryMapEntry (HomeDirectoryMapEntry'),
    newHomeDirectoryMapEntry,

    -- ** IdentityProviderDetails
    IdentityProviderDetails (IdentityProviderDetails'),
    newIdentityProviderDetails,

    -- ** InputFileLocation
    InputFileLocation (InputFileLocation'),
    newInputFileLocation,

    -- ** ListedAccess
    ListedAccess (ListedAccess'),
    newListedAccess,

    -- ** ListedExecution
    ListedExecution (ListedExecution'),
    newListedExecution,

    -- ** ListedServer
    ListedServer (ListedServer'),
    newListedServer,

    -- ** ListedUser
    ListedUser (ListedUser'),
    newListedUser,

    -- ** ListedWorkflow
    ListedWorkflow (ListedWorkflow'),
    newListedWorkflow,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** PosixProfile
    PosixProfile (PosixProfile'),
    newPosixProfile,

    -- ** ProtocolDetails
    ProtocolDetails (ProtocolDetails'),
    newProtocolDetails,

    -- ** S3FileLocation
    S3FileLocation (S3FileLocation'),
    newS3FileLocation,

    -- ** S3InputFileLocation
    S3InputFileLocation (S3InputFileLocation'),
    newS3InputFileLocation,

    -- ** S3Tag
    S3Tag (S3Tag'),
    newS3Tag,

    -- ** ServiceMetadata
    ServiceMetadata (ServiceMetadata'),
    newServiceMetadata,

    -- ** SshPublicKey
    SshPublicKey (SshPublicKey'),
    newSshPublicKey,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagStepDetails
    TagStepDetails (TagStepDetails'),
    newTagStepDetails,

    -- ** UserDetails
    UserDetails (UserDetails'),
    newUserDetails,

    -- ** WorkflowDetail
    WorkflowDetail (WorkflowDetail'),
    newWorkflowDetail,

    -- ** WorkflowDetails
    WorkflowDetails (WorkflowDetails'),
    newWorkflowDetails,

    -- ** WorkflowStep
    WorkflowStep (WorkflowStep'),
    newWorkflowStep,
  )
where

import Amazonka.Transfer.CreateAccess
import Amazonka.Transfer.CreateServer
import Amazonka.Transfer.CreateUser
import Amazonka.Transfer.CreateWorkflow
import Amazonka.Transfer.DeleteAccess
import Amazonka.Transfer.DeleteServer
import Amazonka.Transfer.DeleteSshPublicKey
import Amazonka.Transfer.DeleteUser
import Amazonka.Transfer.DeleteWorkflow
import Amazonka.Transfer.DescribeAccess
import Amazonka.Transfer.DescribeExecution
import Amazonka.Transfer.DescribeSecurityPolicy
import Amazonka.Transfer.DescribeServer
import Amazonka.Transfer.DescribeUser
import Amazonka.Transfer.DescribeWorkflow
import Amazonka.Transfer.ImportSshPublicKey
import Amazonka.Transfer.Lens
import Amazonka.Transfer.ListAccesses
import Amazonka.Transfer.ListExecutions
import Amazonka.Transfer.ListSecurityPolicies
import Amazonka.Transfer.ListServers
import Amazonka.Transfer.ListTagsForResource
import Amazonka.Transfer.ListUsers
import Amazonka.Transfer.ListWorkflows
import Amazonka.Transfer.SendWorkflowStepState
import Amazonka.Transfer.StartServer
import Amazonka.Transfer.StopServer
import Amazonka.Transfer.TagResource
import Amazonka.Transfer.TestIdentityProvider
import Amazonka.Transfer.Types
import Amazonka.Transfer.UntagResource
import Amazonka.Transfer.UpdateAccess
import Amazonka.Transfer.UpdateServer
import Amazonka.Transfer.UpdateUser
import Amazonka.Transfer.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Transfer'.

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
