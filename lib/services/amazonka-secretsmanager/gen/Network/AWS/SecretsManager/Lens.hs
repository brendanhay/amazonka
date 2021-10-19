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

    -- ** ValidateResourcePolicy
    validateResourcePolicy_secretId,
    validateResourcePolicy_resourcePolicy,
    validateResourcePolicyResponse_validationErrors,
    validateResourcePolicyResponse_policyValidationPassed,
    validateResourcePolicyResponse_httpStatus,

    -- ** DeleteSecret
    deleteSecret_recoveryWindowInDays,
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_secretId,
    deleteSecretResponse_arn,
    deleteSecretResponse_name,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_httpStatus,

    -- ** ListSecrets
    listSecrets_filters,
    listSecrets_nextToken,
    listSecrets_sortOrder,
    listSecrets_maxResults,
    listSecretsResponse_nextToken,
    listSecretsResponse_secretList,
    listSecretsResponse_httpStatus,

    -- ** UpdateSecret
    updateSecret_secretBinary,
    updateSecret_kmsKeyId,
    updateSecret_secretString,
    updateSecret_clientRequestToken,
    updateSecret_description,
    updateSecret_secretId,
    updateSecretResponse_versionId,
    updateSecretResponse_arn,
    updateSecretResponse_name,
    updateSecretResponse_httpStatus,

    -- ** RemoveRegionsFromReplication
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_httpStatus,

    -- ** RotateSecret
    rotateSecret_rotationRules,
    rotateSecret_clientRequestToken,
    rotateSecret_rotationLambdaARN,
    rotateSecret_secretId,
    rotateSecretResponse_versionId,
    rotateSecretResponse_arn,
    rotateSecretResponse_name,
    rotateSecretResponse_httpStatus,

    -- ** CreateSecret
    createSecret_addReplicaRegions,
    createSecret_secretBinary,
    createSecret_kmsKeyId,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_secretString,
    createSecret_clientRequestToken,
    createSecret_description,
    createSecret_tags,
    createSecret_name,
    createSecretResponse_versionId,
    createSecretResponse_arn,
    createSecretResponse_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_httpStatus,

    -- ** GetSecretValue
    getSecretValue_versionId,
    getSecretValue_versionStage,
    getSecretValue_secretId,
    getSecretValueResponse_versionId,
    getSecretValueResponse_arn,
    getSecretValueResponse_versionStages,
    getSecretValueResponse_secretBinary,
    getSecretValueResponse_createdDate,
    getSecretValueResponse_name,
    getSecretValueResponse_secretString,
    getSecretValueResponse_httpStatus,

    -- ** DescribeSecret
    describeSecret_secretId,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_arn,
    describeSecretResponse_rotationRules,
    describeSecretResponse_deletedDate,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_createdDate,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_name,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_owningService,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_description,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_tags,
    describeSecretResponse_httpStatus,

    -- ** RestoreSecret
    restoreSecret_secretId,
    restoreSecretResponse_arn,
    restoreSecretResponse_name,
    restoreSecretResponse_httpStatus,

    -- ** CancelRotateSecret
    cancelRotateSecret_secretId,
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_arn,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_secretId,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_arn,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_httpStatus,

    -- ** PutSecretValue
    putSecretValue_versionStages,
    putSecretValue_secretBinary,
    putSecretValue_secretString,
    putSecretValue_clientRequestToken,
    putSecretValue_secretId,
    putSecretValueResponse_versionId,
    putSecretValueResponse_arn,
    putSecretValueResponse_versionStages,
    putSecretValueResponse_name,
    putSecretValueResponse_httpStatus,

    -- ** ReplicateSecretToRegions
    replicateSecretToRegions_forceOverwriteReplicaSecret,
    replicateSecretToRegions_secretId,
    replicateSecretToRegions_addReplicaRegions,
    replicateSecretToRegionsResponse_arn,
    replicateSecretToRegionsResponse_replicationStatus,
    replicateSecretToRegionsResponse_httpStatus,

    -- ** StopReplicationToReplica
    stopReplicationToReplica_secretId,
    stopReplicationToReplicaResponse_arn,
    stopReplicationToReplicaResponse_httpStatus,

    -- ** GetRandomPassword
    getRandomPassword_includeSpace,
    getRandomPassword_excludeNumbers,
    getRandomPassword_excludeLowercase,
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludePunctuation,
    getRandomPassword_requireEachIncludedType,
    getRandomPassword_excludeUppercase,
    getRandomPassword_passwordLength,
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,

    -- ** ListSecretVersionIds
    listSecretVersionIds_nextToken,
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_secretId,
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_httpStatus,

    -- ** TagResource
    tagResource_secretId,
    tagResource_tags,

    -- ** PutResourcePolicy
    putResourcePolicy_blockPublicPolicy,
    putResourcePolicy_secretId,
    putResourcePolicy_resourcePolicy,
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_secretId,
    deleteResourcePolicyResponse_arn,
    deleteResourcePolicyResponse_name,
    deleteResourcePolicyResponse_httpStatus,

    -- ** UntagResource
    untagResource_secretId,
    untagResource_tagKeys,

    -- ** UpdateSecretVersionStage
    updateSecretVersionStage_removeFromVersionId,
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_secretId,
    updateSecretVersionStage_versionStage,
    updateSecretVersionStageResponse_arn,
    updateSecretVersionStageResponse_name,
    updateSecretVersionStageResponse_httpStatus,

    -- * Types

    -- ** Filter
    filter_values,
    filter_key,

    -- ** ReplicaRegionType
    replicaRegionType_kmsKeyId,
    replicaRegionType_region,

    -- ** ReplicationStatusType
    replicationStatusType_status,
    replicationStatusType_kmsKeyId,
    replicationStatusType_statusMessage,
    replicationStatusType_region,
    replicationStatusType_lastAccessedDate,

    -- ** RotationRulesType
    rotationRulesType_automaticallyAfterDays,

    -- ** SecretListEntry
    secretListEntry_lastChangedDate,
    secretListEntry_primaryRegion,
    secretListEntry_arn,
    secretListEntry_secretVersionsToStages,
    secretListEntry_rotationRules,
    secretListEntry_deletedDate,
    secretListEntry_rotationEnabled,
    secretListEntry_createdDate,
    secretListEntry_kmsKeyId,
    secretListEntry_name,
    secretListEntry_owningService,
    secretListEntry_lastRotatedDate,
    secretListEntry_lastAccessedDate,
    secretListEntry_description,
    secretListEntry_rotationLambdaARN,
    secretListEntry_tags,

    -- ** SecretVersionsListEntry
    secretVersionsListEntry_versionId,
    secretVersionsListEntry_versionStages,
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_kmsKeyIds,
    secretVersionsListEntry_lastAccessedDate,

    -- ** Tag
    tag_value,
    tag_key,

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
