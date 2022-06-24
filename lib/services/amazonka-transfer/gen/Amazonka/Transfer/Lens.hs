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

    -- ** CreateAccess
    createAccess_homeDirectory,
    createAccess_policy,
    createAccess_posixProfile,
    createAccess_homeDirectoryType,
    createAccess_homeDirectoryMappings,
    createAccess_role,
    createAccess_serverId,
    createAccess_externalId,
    createAccessResponse_httpStatus,
    createAccessResponse_serverId,
    createAccessResponse_externalId,

    -- ** CreateServer
    createServer_tags,
    createServer_identityProviderDetails,
    createServer_domain,
    createServer_identityProviderType,
    createServer_securityPolicyName,
    createServer_endpointDetails,
    createServer_certificate,
    createServer_protocols,
    createServer_endpointType,
    createServer_loggingRole,
    createServer_workflowDetails,
    createServer_hostKey,
    createServerResponse_httpStatus,
    createServerResponse_serverId,

    -- ** CreateUser
    createUser_tags,
    createUser_homeDirectory,
    createUser_policy,
    createUser_sshPublicKeyBody,
    createUser_posixProfile,
    createUser_homeDirectoryType,
    createUser_homeDirectoryMappings,
    createUser_role,
    createUser_serverId,
    createUser_userName,
    createUserResponse_httpStatus,
    createUserResponse_serverId,
    createUserResponse_userName,

    -- ** CreateWorkflow
    createWorkflow_tags,
    createWorkflow_description,
    createWorkflow_onExceptionSteps,
    createWorkflow_steps,
    createWorkflowResponse_httpStatus,
    createWorkflowResponse_workflowId,

    -- ** DeleteAccess
    deleteAccess_serverId,
    deleteAccess_externalId,

    -- ** DeleteServer
    deleteServer_serverId,

    -- ** DeleteSshPublicKey
    deleteSshPublicKey_serverId,
    deleteSshPublicKey_sshPublicKeyId,
    deleteSshPublicKey_userName,

    -- ** DeleteUser
    deleteUser_serverId,
    deleteUser_userName,

    -- ** DeleteWorkflow
    deleteWorkflow_workflowId,

    -- ** DescribeAccess
    describeAccess_serverId,
    describeAccess_externalId,
    describeAccessResponse_httpStatus,
    describeAccessResponse_serverId,
    describeAccessResponse_access,

    -- ** DescribeExecution
    describeExecution_executionId,
    describeExecution_workflowId,
    describeExecutionResponse_httpStatus,
    describeExecutionResponse_workflowId,
    describeExecutionResponse_execution,

    -- ** DescribeSecurityPolicy
    describeSecurityPolicy_securityPolicyName,
    describeSecurityPolicyResponse_httpStatus,
    describeSecurityPolicyResponse_securityPolicy,

    -- ** DescribeServer
    describeServer_serverId,
    describeServerResponse_httpStatus,
    describeServerResponse_server,

    -- ** DescribeUser
    describeUser_serverId,
    describeUser_userName,
    describeUserResponse_httpStatus,
    describeUserResponse_serverId,
    describeUserResponse_user,

    -- ** DescribeWorkflow
    describeWorkflow_workflowId,
    describeWorkflowResponse_httpStatus,
    describeWorkflowResponse_workflow,

    -- ** ImportSshPublicKey
    importSshPublicKey_serverId,
    importSshPublicKey_sshPublicKeyBody,
    importSshPublicKey_userName,
    importSshPublicKeyResponse_httpStatus,
    importSshPublicKeyResponse_serverId,
    importSshPublicKeyResponse_sshPublicKeyId,
    importSshPublicKeyResponse_userName,

    -- ** ListAccesses
    listAccesses_nextToken,
    listAccesses_maxResults,
    listAccesses_serverId,
    listAccessesResponse_nextToken,
    listAccessesResponse_httpStatus,
    listAccessesResponse_serverId,
    listAccessesResponse_accesses,

    -- ** ListExecutions
    listExecutions_nextToken,
    listExecutions_maxResults,
    listExecutions_workflowId,
    listExecutionsResponse_nextToken,
    listExecutionsResponse_httpStatus,
    listExecutionsResponse_workflowId,
    listExecutionsResponse_executions,

    -- ** ListSecurityPolicies
    listSecurityPolicies_nextToken,
    listSecurityPolicies_maxResults,
    listSecurityPoliciesResponse_nextToken,
    listSecurityPoliciesResponse_httpStatus,
    listSecurityPoliciesResponse_securityPolicyNames,

    -- ** ListServers
    listServers_nextToken,
    listServers_maxResults,
    listServersResponse_nextToken,
    listServersResponse_httpStatus,
    listServersResponse_servers,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_arn,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsers
    listUsers_nextToken,
    listUsers_maxResults,
    listUsers_serverId,
    listUsersResponse_nextToken,
    listUsersResponse_httpStatus,
    listUsersResponse_serverId,
    listUsersResponse_users,

    -- ** ListWorkflows
    listWorkflows_nextToken,
    listWorkflows_maxResults,
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
    listWorkflowsResponse_workflows,

    -- ** SendWorkflowStepState
    sendWorkflowStepState_workflowId,
    sendWorkflowStepState_executionId,
    sendWorkflowStepState_token,
    sendWorkflowStepState_status,
    sendWorkflowStepStateResponse_httpStatus,

    -- ** StartServer
    startServer_serverId,

    -- ** StopServer
    stopServer_serverId,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,

    -- ** TestIdentityProvider
    testIdentityProvider_serverProtocol,
    testIdentityProvider_sourceIp,
    testIdentityProvider_userPassword,
    testIdentityProvider_serverId,
    testIdentityProvider_userName,
    testIdentityProviderResponse_message,
    testIdentityProviderResponse_response,
    testIdentityProviderResponse_httpStatus,
    testIdentityProviderResponse_statusCode,
    testIdentityProviderResponse_url,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,

    -- ** UpdateAccess
    updateAccess_homeDirectory,
    updateAccess_policy,
    updateAccess_posixProfile,
    updateAccess_role,
    updateAccess_homeDirectoryType,
    updateAccess_homeDirectoryMappings,
    updateAccess_serverId,
    updateAccess_externalId,
    updateAccessResponse_httpStatus,
    updateAccessResponse_serverId,
    updateAccessResponse_externalId,

    -- ** UpdateServer
    updateServer_protocolDetails,
    updateServer_identityProviderDetails,
    updateServer_securityPolicyName,
    updateServer_endpointDetails,
    updateServer_certificate,
    updateServer_protocols,
    updateServer_endpointType,
    updateServer_loggingRole,
    updateServer_workflowDetails,
    updateServer_hostKey,
    updateServer_serverId,
    updateServerResponse_httpStatus,
    updateServerResponse_serverId,

    -- ** UpdateUser
    updateUser_homeDirectory,
    updateUser_policy,
    updateUser_posixProfile,
    updateUser_role,
    updateUser_homeDirectoryType,
    updateUser_homeDirectoryMappings,
    updateUser_serverId,
    updateUser_userName,
    updateUserResponse_httpStatus,
    updateUserResponse_serverId,
    updateUserResponse_userName,

    -- * Types

    -- ** CopyStepDetails
    copyStepDetails_name,
    copyStepDetails_overwriteExisting,
    copyStepDetails_destinationFileLocation,

    -- ** CustomStepDetails
    customStepDetails_name,
    customStepDetails_timeoutSeconds,
    customStepDetails_target,

    -- ** DeleteStepDetails
    deleteStepDetails_name,

    -- ** DescribedAccess
    describedAccess_homeDirectory,
    describedAccess_policy,
    describedAccess_posixProfile,
    describedAccess_externalId,
    describedAccess_role,
    describedAccess_homeDirectoryType,
    describedAccess_homeDirectoryMappings,

    -- ** DescribedExecution
    describedExecution_executionRole,
    describedExecution_serviceMetadata,
    describedExecution_initialFileLocation,
    describedExecution_posixProfile,
    describedExecution_status,
    describedExecution_executionId,
    describedExecution_results,
    describedExecution_loggingConfiguration,

    -- ** DescribedSecurityPolicy
    describedSecurityPolicy_tlsCiphers,
    describedSecurityPolicy_sshKexs,
    describedSecurityPolicy_fips,
    describedSecurityPolicy_sshCiphers,
    describedSecurityPolicy_sshMacs,
    describedSecurityPolicy_securityPolicyName,

    -- ** DescribedServer
    describedServer_tags,
    describedServer_userCount,
    describedServer_protocolDetails,
    describedServer_identityProviderDetails,
    describedServer_domain,
    describedServer_identityProviderType,
    describedServer_securityPolicyName,
    describedServer_endpointDetails,
    describedServer_state,
    describedServer_certificate,
    describedServer_protocols,
    describedServer_endpointType,
    describedServer_hostKeyFingerprint,
    describedServer_loggingRole,
    describedServer_serverId,
    describedServer_workflowDetails,
    describedServer_arn,

    -- ** DescribedUser
    describedUser_tags,
    describedUser_homeDirectory,
    describedUser_policy,
    describedUser_userName,
    describedUser_posixProfile,
    describedUser_sshPublicKeys,
    describedUser_role,
    describedUser_homeDirectoryType,
    describedUser_homeDirectoryMappings,
    describedUser_arn,

    -- ** DescribedWorkflow
    describedWorkflow_tags,
    describedWorkflow_workflowId,
    describedWorkflow_steps,
    describedWorkflow_description,
    describedWorkflow_onExceptionSteps,
    describedWorkflow_arn,

    -- ** EfsFileLocation
    efsFileLocation_path,
    efsFileLocation_fileSystemId,

    -- ** EndpointDetails
    endpointDetails_securityGroupIds,
    endpointDetails_vpcEndpointId,
    endpointDetails_addressAllocationIds,
    endpointDetails_vpcId,
    endpointDetails_subnetIds,

    -- ** ExecutionError
    executionError_type,
    executionError_message,

    -- ** ExecutionResults
    executionResults_steps,
    executionResults_onExceptionSteps,

    -- ** ExecutionStepResult
    executionStepResult_outputs,
    executionStepResult_stepType,
    executionStepResult_error,

    -- ** FileLocation
    fileLocation_s3FileLocation,
    fileLocation_efsFileLocation,

    -- ** HomeDirectoryMapEntry
    homeDirectoryMapEntry_entry,
    homeDirectoryMapEntry_target,

    -- ** IdentityProviderDetails
    identityProviderDetails_directoryId,
    identityProviderDetails_url,
    identityProviderDetails_invocationRole,

    -- ** InputFileLocation
    inputFileLocation_s3FileLocation,
    inputFileLocation_efsFileLocation,

    -- ** ListedAccess
    listedAccess_homeDirectory,
    listedAccess_externalId,
    listedAccess_role,
    listedAccess_homeDirectoryType,

    -- ** ListedExecution
    listedExecution_serviceMetadata,
    listedExecution_initialFileLocation,
    listedExecution_status,
    listedExecution_executionId,

    -- ** ListedServer
    listedServer_userCount,
    listedServer_domain,
    listedServer_identityProviderType,
    listedServer_state,
    listedServer_endpointType,
    listedServer_loggingRole,
    listedServer_serverId,
    listedServer_arn,

    -- ** ListedUser
    listedUser_homeDirectory,
    listedUser_userName,
    listedUser_role,
    listedUser_homeDirectoryType,
    listedUser_sshPublicKeyCount,
    listedUser_arn,

    -- ** ListedWorkflow
    listedWorkflow_workflowId,
    listedWorkflow_arn,
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
    s3FileLocation_key,
    s3FileLocation_bucket,
    s3FileLocation_etag,
    s3FileLocation_versionId,

    -- ** S3InputFileLocation
    s3InputFileLocation_key,
    s3InputFileLocation_bucket,

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
    tagStepDetails_tags,
    tagStepDetails_name,

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
    workflowStep_type,
    workflowStep_tagStepDetails,
    workflowStep_customStepDetails,
    workflowStep_deleteStepDetails,
    workflowStep_copyStepDetails,
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
