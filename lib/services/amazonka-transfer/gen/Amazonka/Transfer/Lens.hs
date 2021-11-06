{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Lens
  ( -- * Operations

    -- ** UpdateServer
    updateServer_protocolDetails,
    updateServer_loggingRole,
    updateServer_hostKey,
    updateServer_protocols,
    updateServer_endpointType,
    updateServer_securityPolicyName,
    updateServer_certificate,
    updateServer_identityProviderDetails,
    updateServer_workflowDetails,
    updateServer_endpointDetails,
    updateServer_serverId,
    updateServerResponse_httpStatus,
    updateServerResponse_serverId,

    -- ** DeleteServer
    deleteServer_serverId,

    -- ** CreateWorkflow
    createWorkflow_onExceptionSteps,
    createWorkflow_description,
    createWorkflow_tags,
    createWorkflow_steps,
    createWorkflowResponse_httpStatus,
    createWorkflowResponse_workflowId,

    -- ** DeleteSshPublicKey
    deleteSshPublicKey_serverId,
    deleteSshPublicKey_sshPublicKeyId,
    deleteSshPublicKey_userName,

    -- ** ListSecurityPolicies
    listSecurityPolicies_nextToken,
    listSecurityPolicies_maxResults,
    listSecurityPoliciesResponse_nextToken,
    listSecurityPoliciesResponse_httpStatus,
    listSecurityPoliciesResponse_securityPolicyNames,

    -- ** DeleteWorkflow
    deleteWorkflow_workflowId,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_arn,
    listTagsForResourceResponse_arn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SendWorkflowStepState
    sendWorkflowStepState_workflowId,
    sendWorkflowStepState_executionId,
    sendWorkflowStepState_token,
    sendWorkflowStepState_status,
    sendWorkflowStepStateResponse_httpStatus,

    -- ** StopServer
    stopServer_serverId,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_serverId,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
    listUsersResponse_serverId,
    listUsersResponse_users,

    -- ** DescribeServer
    describeServer_serverId,
    describeServerResponse_httpStatus,
    describeServerResponse_server,

    -- ** DescribeSecurityPolicy
    describeSecurityPolicy_securityPolicyName,
    describeSecurityPolicyResponse_httpStatus,
    describeSecurityPolicyResponse_securityPolicy,

    -- ** ImportSshPublicKey
    importSshPublicKey_serverId,
    importSshPublicKey_sshPublicKeyBody,
    importSshPublicKey_userName,
    importSshPublicKeyResponse_httpStatus,
    importSshPublicKeyResponse_serverId,
    importSshPublicKeyResponse_sshPublicKeyId,
    importSshPublicKeyResponse_userName,

    -- ** ListExecutions
    listExecutions_nextToken,
    listExecutions_maxResults,
    listExecutions_workflowId,
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,
    listExecutionsResponse_workflowId,
    listExecutionsResponse_executions,

    -- ** CreateServer
    createServer_loggingRole,
    createServer_hostKey,
    createServer_identityProviderType,
    createServer_protocols,
    createServer_domain,
    createServer_endpointType,
    createServer_securityPolicyName,
    createServer_certificate,
    createServer_identityProviderDetails,
    createServer_workflowDetails,
    createServer_tags,
    createServer_endpointDetails,
    createServerResponse_httpStatus,
    createServerResponse_serverId,

    -- ** TestIdentityProvider
    testIdentityProvider_serverProtocol,
    testIdentityProvider_userPassword,
    testIdentityProvider_sourceIp,
    testIdentityProvider_serverId,
    testIdentityProvider_userName,
    testIdentityProviderResponse_response,
    testIdentityProviderResponse_message,
    testIdentityProviderResponse_httpStatus,
    testIdentityProviderResponse_statusCode,
    testIdentityProviderResponse_url,

    -- ** ListServers
    listServers_nextToken,
    listServers_maxResults,
    listServersResponse_nextToken,
    listServersResponse_httpStatus,
    listServersResponse_servers,

    -- ** DescribeUser
    describeUser_serverId,
    describeUser_userName,
    describeUserResponse_httpStatus,
    describeUserResponse_serverId,
    describeUserResponse_user,

    -- ** DescribeExecution
    describeExecution_executionId,
    describeExecution_workflowId,
    describeExecutionResponse_httpStatus,
    describeExecutionResponse_workflowId,
    describeExecutionResponse_execution,

    -- ** ListWorkflows
    listWorkflows_nextToken,
    listWorkflows_maxResults,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
    listWorkflowsResponse_workflows,

    -- ** CreateUser
    createUser_homeDirectoryType,
    createUser_sshPublicKeyBody,
    createUser_posixProfile,
    createUser_homeDirectoryMappings,
    createUser_policy,
    createUser_homeDirectory,
    createUser_tags,
    createUser_role,
    createUser_serverId,
    createUser_userName,
    createUserResponse_httpStatus,
    createUserResponse_serverId,
    createUserResponse_userName,

    -- ** StartServer
    startServer_serverId,

    -- ** UpdateAccess
    updateAccess_homeDirectoryType,
    updateAccess_posixProfile,
    updateAccess_homeDirectoryMappings,
    updateAccess_role,
    updateAccess_policy,
    updateAccess_homeDirectory,
    updateAccess_serverId,
    updateAccess_externalId,
    updateAccessResponse_httpStatus,
    updateAccessResponse_serverId,
    updateAccessResponse_externalId,

    -- ** DeleteAccess
    deleteAccess_serverId,
    deleteAccess_externalId,

    -- ** CreateAccess
    createAccess_homeDirectoryType,
    createAccess_posixProfile,
    createAccess_homeDirectoryMappings,
    createAccess_policy,
    createAccess_homeDirectory,
    createAccess_role,
    createAccess_serverId,
    createAccess_externalId,
    createAccessResponse_httpStatus,
    createAccessResponse_serverId,
    createAccessResponse_externalId,

    -- ** ListAccesses
    listAccesses_nextToken,
    listAccesses_maxResults,
    listAccesses_serverId,
    listAccessesResponse_nextToken,
    listAccessesResponse_httpStatus,
    listAccessesResponse_serverId,
    listAccessesResponse_accesses,

    -- ** UpdateUser
    updateUser_homeDirectoryType,
    updateUser_posixProfile,
    updateUser_homeDirectoryMappings,
    updateUser_role,
    updateUser_policy,
    updateUser_homeDirectory,
    updateUser_serverId,
    updateUser_userName,
    updateUserResponse_httpStatus,
    updateUserResponse_serverId,
    updateUserResponse_userName,

    -- ** DeleteUser
    deleteUser_serverId,
    deleteUser_userName,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,

    -- ** DescribeWorkflow
    describeWorkflow_workflowId,
    describeWorkflowResponse_httpStatus,
    describeWorkflowResponse_workflow,

    -- ** DescribeAccess
    describeAccess_serverId,
    describeAccess_externalId,
    describeAccessResponse_httpStatus,
    describeAccessResponse_serverId,
    describeAccessResponse_access,

    -- * Types

    -- ** CopyStepDetails
    copyStepDetails_destinationFileLocation,
    copyStepDetails_overwriteExisting,
    copyStepDetails_name,

    -- ** CustomStepDetails
    customStepDetails_name,
    customStepDetails_timeoutSeconds,
    customStepDetails_target,

    -- ** DeleteStepDetails
    deleteStepDetails_name,

    -- ** DescribedAccess
    describedAccess_homeDirectoryType,
    describedAccess_posixProfile,
    describedAccess_homeDirectoryMappings,
    describedAccess_role,
    describedAccess_policy,
    describedAccess_externalId,
    describedAccess_homeDirectory,

    -- ** DescribedExecution
    describedExecution_status,
    describedExecution_executionId,
    describedExecution_results,
    describedExecution_initialFileLocation,
    describedExecution_posixProfile,
    describedExecution_serviceMetadata,
    describedExecution_loggingConfiguration,
    describedExecution_executionRole,

    -- ** DescribedSecurityPolicy
    describedSecurityPolicy_fips,
    describedSecurityPolicy_sshMacs,
    describedSecurityPolicy_sshKexs,
    describedSecurityPolicy_tlsCiphers,
    describedSecurityPolicy_sshCiphers,
    describedSecurityPolicy_securityPolicyName,

    -- ** DescribedServer
    describedServer_protocolDetails,
    describedServer_loggingRole,
    describedServer_state,
    describedServer_identityProviderType,
    describedServer_protocols,
    describedServer_serverId,
    describedServer_domain,
    describedServer_endpointType,
    describedServer_securityPolicyName,
    describedServer_hostKeyFingerprint,
    describedServer_userCount,
    describedServer_certificate,
    describedServer_identityProviderDetails,
    describedServer_workflowDetails,
    describedServer_tags,
    describedServer_endpointDetails,
    describedServer_arn,

    -- ** DescribedUser
    describedUser_sshPublicKeys,
    describedUser_homeDirectoryType,
    describedUser_userName,
    describedUser_posixProfile,
    describedUser_homeDirectoryMappings,
    describedUser_role,
    describedUser_policy,
    describedUser_homeDirectory,
    describedUser_tags,
    describedUser_arn,

    -- ** DescribedWorkflow
    describedWorkflow_onExceptionSteps,
    describedWorkflow_steps,
    describedWorkflow_workflowId,
    describedWorkflow_description,
    describedWorkflow_tags,
    describedWorkflow_arn,

    -- ** EfsFileLocation
    efsFileLocation_path,
    efsFileLocation_fileSystemId,

    -- ** EndpointDetails
    endpointDetails_securityGroupIds,
    endpointDetails_subnetIds,
    endpointDetails_vpcId,
    endpointDetails_addressAllocationIds,
    endpointDetails_vpcEndpointId,

    -- ** ExecutionError
    executionError_type,
    executionError_message,

    -- ** ExecutionResults
    executionResults_onExceptionSteps,
    executionResults_steps,

    -- ** ExecutionStepResult
    executionStepResult_stepType,
    executionStepResult_error,
    executionStepResult_outputs,

    -- ** FileLocation
    fileLocation_efsFileLocation,
    fileLocation_s3FileLocation,

    -- ** HomeDirectoryMapEntry
    homeDirectoryMapEntry_entry,
    homeDirectoryMapEntry_target,

    -- ** IdentityProviderDetails
    identityProviderDetails_invocationRole,
    identityProviderDetails_directoryId,
    identityProviderDetails_url,

    -- ** InputFileLocation
    inputFileLocation_efsFileLocation,
    inputFileLocation_s3FileLocation,

    -- ** ListedAccess
    listedAccess_homeDirectoryType,
    listedAccess_role,
    listedAccess_externalId,
    listedAccess_homeDirectory,

    -- ** ListedExecution
    listedExecution_status,
    listedExecution_executionId,
    listedExecution_initialFileLocation,
    listedExecution_serviceMetadata,

    -- ** ListedServer
    listedServer_loggingRole,
    listedServer_state,
    listedServer_identityProviderType,
    listedServer_serverId,
    listedServer_domain,
    listedServer_endpointType,
    listedServer_userCount,
    listedServer_arn,

    -- ** ListedUser
    listedUser_homeDirectoryType,
    listedUser_userName,
    listedUser_role,
    listedUser_sshPublicKeyCount,
    listedUser_homeDirectory,
    listedUser_arn,

    -- ** ListedWorkflow
    listedWorkflow_arn,
    listedWorkflow_workflowId,
    listedWorkflow_description,

    -- ** LoggingConfiguration
    loggingConfiguration_loggingRole,
    loggingConfiguration_logGroupName,

    -- ** PosixProfile
    posixProfile_secondaryGids,
    posixProfile_uid,
    posixProfile_gid,

    -- ** ProtocolDetails
    protocolDetails_passiveIp,

    -- ** S3FileLocation
    s3FileLocation_versionId,
    s3FileLocation_etag,
    s3FileLocation_bucket,
    s3FileLocation_key,

    -- ** S3InputFileLocation
    s3InputFileLocation_bucket,
    s3InputFileLocation_key,

    -- ** S3Tag
    s3Tag_key,
    s3Tag_value,

    -- ** ServiceMetadata
    serviceMetadata_userDetails,

    -- ** SshPublicKey
    sshPublicKey_dateImported,
    sshPublicKey_sshPublicKeyBody,
    sshPublicKey_sshPublicKeyId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagStepDetails
    tagStepDetails_name,
    tagStepDetails_tags,

    -- ** UserDetails
    userDetails_sessionId,
    userDetails_userName,
    userDetails_serverId,

    -- ** WorkflowDetail
    workflowDetail_workflowId,
    workflowDetail_executionRole,

    -- ** WorkflowDetails
    workflowDetails_onUpload,

    -- ** WorkflowStep
    workflowStep_tagStepDetails,
    workflowStep_deleteStepDetails,
    workflowStep_copyStepDetails,
    workflowStep_type,
    workflowStep_customStepDetails,
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
import Amazonka.Transfer.Types.CopyStepDetails
import Amazonka.Transfer.Types.CustomStepDetails
import Amazonka.Transfer.Types.DeleteStepDetails
import Amazonka.Transfer.Types.DescribedAccess
import Amazonka.Transfer.Types.DescribedExecution
import Amazonka.Transfer.Types.DescribedSecurityPolicy
import Amazonka.Transfer.Types.DescribedServer
import Amazonka.Transfer.Types.DescribedUser
import Amazonka.Transfer.Types.DescribedWorkflow
import Amazonka.Transfer.Types.EfsFileLocation
import Amazonka.Transfer.Types.EndpointDetails
import Amazonka.Transfer.Types.ExecutionError
import Amazonka.Transfer.Types.ExecutionResults
import Amazonka.Transfer.Types.ExecutionStepResult
import Amazonka.Transfer.Types.FileLocation
import Amazonka.Transfer.Types.HomeDirectoryMapEntry
import Amazonka.Transfer.Types.IdentityProviderDetails
import Amazonka.Transfer.Types.InputFileLocation
import Amazonka.Transfer.Types.ListedAccess
import Amazonka.Transfer.Types.ListedExecution
import Amazonka.Transfer.Types.ListedServer
import Amazonka.Transfer.Types.ListedUser
import Amazonka.Transfer.Types.ListedWorkflow
import Amazonka.Transfer.Types.LoggingConfiguration
import Amazonka.Transfer.Types.PosixProfile
import Amazonka.Transfer.Types.ProtocolDetails
import Amazonka.Transfer.Types.S3FileLocation
import Amazonka.Transfer.Types.S3InputFileLocation
import Amazonka.Transfer.Types.S3Tag
import Amazonka.Transfer.Types.ServiceMetadata
import Amazonka.Transfer.Types.SshPublicKey
import Amazonka.Transfer.Types.Tag
import Amazonka.Transfer.Types.TagStepDetails
import Amazonka.Transfer.Types.UserDetails
import Amazonka.Transfer.Types.WorkflowDetail
import Amazonka.Transfer.Types.WorkflowDetails
import Amazonka.Transfer.Types.WorkflowStep
import Amazonka.Transfer.UntagResource
import Amazonka.Transfer.UpdateAccess
import Amazonka.Transfer.UpdateServer
import Amazonka.Transfer.UpdateUser
