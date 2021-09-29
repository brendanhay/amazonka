{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Lens
  ( -- * Operations

    -- ** DeleteSecret
    deleteSecret_recoveryWindowInDays,
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_secretId,
    deleteSecretResponse_arn,
    deleteSecretResponse_name,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_httpStatus,

    -- ** UpdateSecret
    updateSecret_secretBinary,
    updateSecret_kmsKeyId,
    updateSecret_description,
    updateSecret_secretString,
    updateSecret_clientRequestToken,
    updateSecret_secretId,
    updateSecretResponse_arn,
    updateSecretResponse_name,
    updateSecretResponse_versionId,
    updateSecretResponse_httpStatus,

    -- ** RotateSecret
    rotateSecret_rotationRules,
    rotateSecret_rotationLambdaARN,
    rotateSecret_clientRequestToken,
    rotateSecret_secretId,
    rotateSecretResponse_arn,
    rotateSecretResponse_name,
    rotateSecretResponse_versionId,
    rotateSecretResponse_httpStatus,

    -- ** RemoveRegionsFromReplication
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_secretId,
    getResourcePolicyResponse_arn,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_httpStatus,

    -- ** ValidateResourcePolicy
    validateResourcePolicy_secretId,
    validateResourcePolicy_resourcePolicy,
    validateResourcePolicyResponse_policyValidationPassed,
    validateResourcePolicyResponse_validationErrors,
    validateResourcePolicyResponse_httpStatus,

    -- ** CancelRotateSecret
    cancelRotateSecret_secretId,
    cancelRotateSecretResponse_arn,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_httpStatus,

    -- ** RestoreSecret
    restoreSecret_secretId,
    restoreSecretResponse_arn,
    restoreSecretResponse_name,
    restoreSecretResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_blockPublicPolicy,
    putResourcePolicy_secretId,
    putResourcePolicy_resourcePolicy,
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_httpStatus,

    -- ** DescribeSecret
    describeSecret_secretId,
    describeSecretResponse_createdDate,
    describeSecretResponse_owningService,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_arn,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_name,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_deletedDate,
    describeSecretResponse_tags,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_rotationRules,
    describeSecretResponse_description,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_httpStatus,

    -- ** GetSecretValue
    getSecretValue_versionId,
    getSecretValue_versionStage,
    getSecretValue_secretId,
    getSecretValueResponse_createdDate,
    getSecretValueResponse_secretBinary,
    getSecretValueResponse_versionStages,
    getSecretValueResponse_arn,
    getSecretValueResponse_name,
    getSecretValueResponse_versionId,
    getSecretValueResponse_secretString,
    getSecretValueResponse_httpStatus,

    -- ** UntagResource
    untagResource_secretId,
    untagResource_tagKeys,

    -- ** TagResource
    tagResource_secretId,
    tagResource_tags,

    -- ** CreateSecret
    createSecret_secretBinary,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_kmsKeyId,
    createSecret_tags,
    createSecret_description,
    createSecret_secretString,
    createSecret_clientRequestToken,
    createSecret_addReplicaRegions,
    createSecret_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_arn,
    createSecretResponse_name,
    createSecretResponse_versionId,
    createSecretResponse_httpStatus,

    -- ** GetRandomPassword
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludeLowercase,
    getRandomPassword_includeSpace,
    getRandomPassword_requireEachIncludedType,
    getRandomPassword_excludeNumbers,
    getRandomPassword_passwordLength,
    getRandomPassword_excludeUppercase,
    getRandomPassword_excludePunctuation,
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,

    -- ** ReplicateSecretToRegions
    replicateSecretToRegions_forceOverwriteReplicaSecret,
    replicateSecretToRegions_secretId,
    replicateSecretToRegions_addReplicaRegions,
    replicateSecretToRegionsResponse_replicationStatus,
    replicateSecretToRegionsResponse_arn,
    replicateSecretToRegionsResponse_httpStatus,

    -- ** StopReplicationToReplica
    stopReplicationToReplica_secretId,
    stopReplicationToReplicaResponse_arn,
    stopReplicationToReplicaResponse_httpStatus,

    -- ** ListSecrets
    listSecrets_nextToken,
    listSecrets_sortOrder,
    listSecrets_maxResults,
    listSecrets_filters,
    listSecretsResponse_nextToken,
    listSecretsResponse_secretList,
    listSecretsResponse_httpStatus,

    -- ** PutSecretValue
    putSecretValue_secretBinary,
    putSecretValue_versionStages,
    putSecretValue_secretString,
    putSecretValue_clientRequestToken,
    putSecretValue_secretId,
    putSecretValueResponse_versionStages,
    putSecretValueResponse_arn,
    putSecretValueResponse_name,
    putSecretValueResponse_versionId,
    putSecretValueResponse_httpStatus,

    -- ** UpdateSecretVersionStage
    updateSecretVersionStage_removeFromVersionId,
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_secretId,
    updateSecretVersionStage_versionStage,
    updateSecretVersionStageResponse_arn,
    updateSecretVersionStageResponse_name,
    updateSecretVersionStageResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_secretId,
    deleteResourcePolicyResponse_arn,
    deleteResourcePolicyResponse_name,
    deleteResourcePolicyResponse_httpStatus,

    -- ** ListSecretVersionIds
    listSecretVersionIds_nextToken,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_secretId,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_httpStatus,

    -- * Types

    -- ** Filter
    filter_key,
    filter_values,

    -- ** ReplicaRegionType
    replicaRegionType_kmsKeyId,
    replicaRegionType_region,

    -- ** ReplicationStatusType
    replicationStatusType_statusMessage,
    replicationStatusType_status,
    replicationStatusType_kmsKeyId,
    replicationStatusType_lastAccessedDate,
    replicationStatusType_region,

    -- ** RotationRulesType
    rotationRulesType_automaticallyAfterDays,

    -- ** SecretListEntry
    secretListEntry_createdDate,
    secretListEntry_owningService,
    secretListEntry_secretVersionsToStages,
    secretListEntry_lastRotatedDate,
    secretListEntry_arn,
    secretListEntry_kmsKeyId,
    secretListEntry_name,
    secretListEntry_primaryRegion,
    secretListEntry_lastChangedDate,
    secretListEntry_deletedDate,
    secretListEntry_tags,
    secretListEntry_rotationEnabled,
    secretListEntry_rotationRules,
    secretListEntry_description,
    secretListEntry_rotationLambdaARN,
    secretListEntry_lastAccessedDate,

    -- ** SecretVersionsListEntry
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_kmsKeyIds,
    secretVersionsListEntry_versionStages,
    secretVersionsListEntry_versionId,
    secretVersionsListEntry_lastAccessedDate,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ValidationErrorsEntry
    validationErrorsEntry_checkName,
    validationErrorsEntry_errorMessage,
  )
where

import Network.AWS.SecretsManager.CancelRotateSecret
import Network.AWS.SecretsManager.CreateSecret
import Network.AWS.SecretsManager.DeleteResourcePolicy
import Network.AWS.SecretsManager.DeleteSecret
import Network.AWS.SecretsManager.DescribeSecret
import Network.AWS.SecretsManager.GetRandomPassword
import Network.AWS.SecretsManager.GetResourcePolicy
import Network.AWS.SecretsManager.GetSecretValue
import Network.AWS.SecretsManager.ListSecretVersionIds
import Network.AWS.SecretsManager.ListSecrets
import Network.AWS.SecretsManager.PutResourcePolicy
import Network.AWS.SecretsManager.PutSecretValue
import Network.AWS.SecretsManager.RemoveRegionsFromReplication
import Network.AWS.SecretsManager.ReplicateSecretToRegions
import Network.AWS.SecretsManager.RestoreSecret
import Network.AWS.SecretsManager.RotateSecret
import Network.AWS.SecretsManager.StopReplicationToReplica
import Network.AWS.SecretsManager.TagResource
import Network.AWS.SecretsManager.Types.Filter
import Network.AWS.SecretsManager.Types.ReplicaRegionType
import Network.AWS.SecretsManager.Types.ReplicationStatusType
import Network.AWS.SecretsManager.Types.RotationRulesType
import Network.AWS.SecretsManager.Types.SecretListEntry
import Network.AWS.SecretsManager.Types.SecretVersionsListEntry
import Network.AWS.SecretsManager.Types.Tag
import Network.AWS.SecretsManager.Types.ValidationErrorsEntry
import Network.AWS.SecretsManager.UntagResource
import Network.AWS.SecretsManager.UpdateSecret
import Network.AWS.SecretsManager.UpdateSecretVersionStage
import Network.AWS.SecretsManager.ValidateResourcePolicy
