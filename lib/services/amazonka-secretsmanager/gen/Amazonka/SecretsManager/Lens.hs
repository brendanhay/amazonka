{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecretsManager.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Lens
  ( -- * Operations

    -- ** CancelRotateSecret
    cancelRotateSecret_secretId,
    cancelRotateSecretResponse_arn,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_httpStatus,

    -- ** CreateSecret
    createSecret_addReplicaRegions,
    createSecret_clientRequestToken,
    createSecret_description,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_kmsKeyId,
    createSecret_secretBinary,
    createSecret_secretString,
    createSecret_tags,
    createSecret_name,
    createSecretResponse_arn,
    createSecretResponse_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_versionId,
    createSecretResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_secretId,
    deleteResourcePolicyResponse_arn,
    deleteResourcePolicyResponse_name,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSecret
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_recoveryWindowInDays,
    deleteSecret_secretId,
    deleteSecretResponse_arn,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_name,
    deleteSecretResponse_httpStatus,

    -- ** DescribeSecret
    describeSecret_secretId,
    describeSecretResponse_arn,
    describeSecretResponse_createdDate,
    describeSecretResponse_deletedDate,
    describeSecretResponse_description,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_name,
    describeSecretResponse_nextRotationDate,
    describeSecretResponse_owningService,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_rotationRules,
    describeSecretResponse_tags,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_httpStatus,

    -- ** GetRandomPassword
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludeLowercase,
    getRandomPassword_excludeNumbers,
    getRandomPassword_excludePunctuation,
    getRandomPassword_excludeUppercase,
    getRandomPassword_includeSpace,
    getRandomPassword_passwordLength,
    getRandomPassword_requireEachIncludedType,
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_secretId,
    getResourcePolicyResponse_arn,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetSecretValue
    getSecretValue_versionId,
    getSecretValue_versionStage,
    getSecretValue_secretId,
    getSecretValueResponse_arn,
    getSecretValueResponse_createdDate,
    getSecretValueResponse_name,
    getSecretValueResponse_secretBinary,
    getSecretValueResponse_secretString,
    getSecretValueResponse_versionId,
    getSecretValueResponse_versionStages,
    getSecretValueResponse_httpStatus,

    -- ** ListSecretVersionIds
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_nextToken,
    listSecretVersionIds_secretId,
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_httpStatus,

    -- ** ListSecrets
    listSecrets_filters,
    listSecrets_includePlannedDeletion,
    listSecrets_maxResults,
    listSecrets_nextToken,
    listSecrets_sortOrder,
    listSecretsResponse_nextToken,
    listSecretsResponse_secretList,
    listSecretsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_blockPublicPolicy,
    putResourcePolicy_secretId,
    putResourcePolicy_resourcePolicy,
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_httpStatus,

    -- ** PutSecretValue
    putSecretValue_clientRequestToken,
    putSecretValue_secretBinary,
    putSecretValue_secretString,
    putSecretValue_versionStages,
    putSecretValue_secretId,
    putSecretValueResponse_arn,
    putSecretValueResponse_name,
    putSecretValueResponse_versionId,
    putSecretValueResponse_versionStages,
    putSecretValueResponse_httpStatus,

    -- ** RemoveRegionsFromReplication
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_httpStatus,

    -- ** ReplicateSecretToRegions
    replicateSecretToRegions_forceOverwriteReplicaSecret,
    replicateSecretToRegions_secretId,
    replicateSecretToRegions_addReplicaRegions,
    replicateSecretToRegionsResponse_arn,
    replicateSecretToRegionsResponse_replicationStatus,
    replicateSecretToRegionsResponse_httpStatus,

    -- ** RestoreSecret
    restoreSecret_secretId,
    restoreSecretResponse_arn,
    restoreSecretResponse_name,
    restoreSecretResponse_httpStatus,

    -- ** RotateSecret
    rotateSecret_clientRequestToken,
    rotateSecret_rotateImmediately,
    rotateSecret_rotationLambdaARN,
    rotateSecret_rotationRules,
    rotateSecret_secretId,
    rotateSecretResponse_arn,
    rotateSecretResponse_name,
    rotateSecretResponse_versionId,
    rotateSecretResponse_httpStatus,

    -- ** StopReplicationToReplica
    stopReplicationToReplica_secretId,
    stopReplicationToReplicaResponse_arn,
    stopReplicationToReplicaResponse_httpStatus,

    -- ** TagResource
    tagResource_secretId,
    tagResource_tags,

    -- ** UntagResource
    untagResource_secretId,
    untagResource_tagKeys,

    -- ** UpdateSecret
    updateSecret_clientRequestToken,
    updateSecret_description,
    updateSecret_kmsKeyId,
    updateSecret_secretBinary,
    updateSecret_secretString,
    updateSecret_secretId,
    updateSecretResponse_arn,
    updateSecretResponse_name,
    updateSecretResponse_versionId,
    updateSecretResponse_httpStatus,

    -- ** UpdateSecretVersionStage
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_removeFromVersionId,
    updateSecretVersionStage_secretId,
    updateSecretVersionStage_versionStage,
    updateSecretVersionStageResponse_arn,
    updateSecretVersionStageResponse_name,
    updateSecretVersionStageResponse_httpStatus,

    -- ** ValidateResourcePolicy
    validateResourcePolicy_secretId,
    validateResourcePolicy_resourcePolicy,
    validateResourcePolicyResponse_policyValidationPassed,
    validateResourcePolicyResponse_validationErrors,
    validateResourcePolicyResponse_httpStatus,

    -- * Types

    -- ** Filter
    filter_key,
    filter_values,

    -- ** ReplicaRegionType
    replicaRegionType_kmsKeyId,
    replicaRegionType_region,

    -- ** ReplicationStatusType
    replicationStatusType_kmsKeyId,
    replicationStatusType_lastAccessedDate,
    replicationStatusType_region,
    replicationStatusType_status,
    replicationStatusType_statusMessage,

    -- ** RotationRulesType
    rotationRulesType_automaticallyAfterDays,
    rotationRulesType_duration,
    rotationRulesType_scheduleExpression,

    -- ** SecretListEntry
    secretListEntry_arn,
    secretListEntry_createdDate,
    secretListEntry_deletedDate,
    secretListEntry_description,
    secretListEntry_kmsKeyId,
    secretListEntry_lastAccessedDate,
    secretListEntry_lastChangedDate,
    secretListEntry_lastRotatedDate,
    secretListEntry_name,
    secretListEntry_nextRotationDate,
    secretListEntry_owningService,
    secretListEntry_primaryRegion,
    secretListEntry_rotationEnabled,
    secretListEntry_rotationLambdaARN,
    secretListEntry_rotationRules,
    secretListEntry_secretVersionsToStages,
    secretListEntry_tags,

    -- ** SecretVersionsListEntry
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_kmsKeyIds,
    secretVersionsListEntry_lastAccessedDate,
    secretVersionsListEntry_versionId,
    secretVersionsListEntry_versionStages,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ValidationErrorsEntry
    validationErrorsEntry_checkName,
    validationErrorsEntry_errorMessage,
  )
where

import Amazonka.SecretsManager.CancelRotateSecret
import Amazonka.SecretsManager.CreateSecret
import Amazonka.SecretsManager.DeleteResourcePolicy
import Amazonka.SecretsManager.DeleteSecret
import Amazonka.SecretsManager.DescribeSecret
import Amazonka.SecretsManager.GetRandomPassword
import Amazonka.SecretsManager.GetResourcePolicy
import Amazonka.SecretsManager.GetSecretValue
import Amazonka.SecretsManager.ListSecretVersionIds
import Amazonka.SecretsManager.ListSecrets
import Amazonka.SecretsManager.PutResourcePolicy
import Amazonka.SecretsManager.PutSecretValue
import Amazonka.SecretsManager.RemoveRegionsFromReplication
import Amazonka.SecretsManager.ReplicateSecretToRegions
import Amazonka.SecretsManager.RestoreSecret
import Amazonka.SecretsManager.RotateSecret
import Amazonka.SecretsManager.StopReplicationToReplica
import Amazonka.SecretsManager.TagResource
import Amazonka.SecretsManager.Types.Filter
import Amazonka.SecretsManager.Types.ReplicaRegionType
import Amazonka.SecretsManager.Types.ReplicationStatusType
import Amazonka.SecretsManager.Types.RotationRulesType
import Amazonka.SecretsManager.Types.SecretListEntry
import Amazonka.SecretsManager.Types.SecretVersionsListEntry
import Amazonka.SecretsManager.Types.Tag
import Amazonka.SecretsManager.Types.ValidationErrorsEntry
import Amazonka.SecretsManager.UntagResource
import Amazonka.SecretsManager.UpdateSecret
import Amazonka.SecretsManager.UpdateSecretVersionStage
import Amazonka.SecretsManager.ValidateResourcePolicy
