{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecretsManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Lens
  ( -- * Operations

    -- ** CancelRotateSecret
    cancelRotateSecret_secretId,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_arn,
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_httpStatus,

    -- ** CreateSecret
    createSecret_tags,
    createSecret_addReplicaRegions,
    createSecret_clientRequestToken,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_description,
    createSecret_secretBinary,
    createSecret_kmsKeyId,
    createSecret_secretString,
    createSecret_name,
    createSecretResponse_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_arn,
    createSecretResponse_versionId,
    createSecretResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_secretId,
    deleteResourcePolicyResponse_name,
    deleteResourcePolicyResponse_arn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSecret
    deleteSecret_recoveryWindowInDays,
    deleteSecret_forceDeleteWithoutRecovery,
    deleteSecret_secretId,
    deleteSecretResponse_name,
    deleteSecretResponse_arn,
    deleteSecretResponse_deletionDate,
    deleteSecretResponse_httpStatus,

    -- ** DescribeSecret
    describeSecret_secretId,
    describeSecretResponse_tags,
    describeSecretResponse_name,
    describeSecretResponse_lastAccessedDate,
    describeSecretResponse_rotationLambdaARN,
    describeSecretResponse_rotationRules,
    describeSecretResponse_versionIdsToStages,
    describeSecretResponse_replicationStatus,
    describeSecretResponse_arn,
    describeSecretResponse_primaryRegion,
    describeSecretResponse_description,
    describeSecretResponse_rotationEnabled,
    describeSecretResponse_lastChangedDate,
    describeSecretResponse_kmsKeyId,
    describeSecretResponse_deletedDate,
    describeSecretResponse_createdDate,
    describeSecretResponse_lastRotatedDate,
    describeSecretResponse_owningService,
    describeSecretResponse_httpStatus,

    -- ** GetRandomPassword
    getRandomPassword_excludeUppercase,
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludeLowercase,
    getRandomPassword_includeSpace,
    getRandomPassword_excludePunctuation,
    getRandomPassword_passwordLength,
    getRandomPassword_requireEachIncludedType,
    getRandomPassword_excludeNumbers,
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_secretId,
    getResourcePolicyResponse_name,
    getResourcePolicyResponse_arn,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetSecretValue
    getSecretValue_versionStage,
    getSecretValue_versionId,
    getSecretValue_secretId,
    getSecretValueResponse_versionStages,
    getSecretValueResponse_name,
    getSecretValueResponse_arn,
    getSecretValueResponse_secretBinary,
    getSecretValueResponse_secretString,
    getSecretValueResponse_createdDate,
    getSecretValueResponse_versionId,
    getSecretValueResponse_httpStatus,

    -- ** ListSecretVersionIds
    listSecretVersionIds_nextToken,
    listSecretVersionIds_maxResults,
    listSecretVersionIds_includeDeprecated,
    listSecretVersionIds_secretId,
    listSecretVersionIdsResponse_name,
    listSecretVersionIdsResponse_nextToken,
    listSecretVersionIdsResponse_arn,
    listSecretVersionIdsResponse_versions,
    listSecretVersionIdsResponse_httpStatus,

    -- ** ListSecrets
    listSecrets_sortOrder,
    listSecrets_nextToken,
    listSecrets_filters,
    listSecrets_maxResults,
    listSecretsResponse_nextToken,
    listSecretsResponse_secretList,
    listSecretsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_blockPublicPolicy,
    putResourcePolicy_secretId,
    putResourcePolicy_resourcePolicy,
    putResourcePolicyResponse_name,
    putResourcePolicyResponse_arn,
    putResourcePolicyResponse_httpStatus,

    -- ** PutSecretValue
    putSecretValue_versionStages,
    putSecretValue_clientRequestToken,
    putSecretValue_secretBinary,
    putSecretValue_secretString,
    putSecretValue_secretId,
    putSecretValueResponse_versionStages,
    putSecretValueResponse_name,
    putSecretValueResponse_arn,
    putSecretValueResponse_versionId,
    putSecretValueResponse_httpStatus,

    -- ** RemoveRegionsFromReplication
    removeRegionsFromReplication_secretId,
    removeRegionsFromReplication_removeReplicaRegions,
    removeRegionsFromReplicationResponse_replicationStatus,
    removeRegionsFromReplicationResponse_arn,
    removeRegionsFromReplicationResponse_httpStatus,

    -- ** ReplicateSecretToRegions
    replicateSecretToRegions_forceOverwriteReplicaSecret,
    replicateSecretToRegions_secretId,
    replicateSecretToRegions_addReplicaRegions,
    replicateSecretToRegionsResponse_replicationStatus,
    replicateSecretToRegionsResponse_arn,
    replicateSecretToRegionsResponse_httpStatus,

    -- ** RestoreSecret
    restoreSecret_secretId,
    restoreSecretResponse_name,
    restoreSecretResponse_arn,
    restoreSecretResponse_httpStatus,

    -- ** RotateSecret
    rotateSecret_rotationLambdaARN,
    rotateSecret_clientRequestToken,
    rotateSecret_rotationRules,
    rotateSecret_secretId,
    rotateSecretResponse_name,
    rotateSecretResponse_arn,
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
    updateSecret_secretBinary,
    updateSecret_kmsKeyId,
    updateSecret_secretString,
    updateSecret_secretId,
    updateSecretResponse_name,
    updateSecretResponse_arn,
    updateSecretResponse_versionId,
    updateSecretResponse_httpStatus,

    -- ** UpdateSecretVersionStage
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_removeFromVersionId,
    updateSecretVersionStage_secretId,
    updateSecretVersionStage_versionStage,
    updateSecretVersionStageResponse_name,
    updateSecretVersionStageResponse_arn,
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
    replicaRegionType_region,
    replicaRegionType_kmsKeyId,

    -- ** ReplicationStatusType
    replicationStatusType_lastAccessedDate,
    replicationStatusType_status,
    replicationStatusType_region,
    replicationStatusType_kmsKeyId,
    replicationStatusType_statusMessage,

    -- ** RotationRulesType
    rotationRulesType_automaticallyAfterDays,

    -- ** SecretListEntry
    secretListEntry_tags,
    secretListEntry_name,
    secretListEntry_lastAccessedDate,
    secretListEntry_rotationLambdaARN,
    secretListEntry_rotationRules,
    secretListEntry_arn,
    secretListEntry_primaryRegion,
    secretListEntry_description,
    secretListEntry_rotationEnabled,
    secretListEntry_lastChangedDate,
    secretListEntry_kmsKeyId,
    secretListEntry_deletedDate,
    secretListEntry_createdDate,
    secretListEntry_secretVersionsToStages,
    secretListEntry_lastRotatedDate,
    secretListEntry_owningService,

    -- ** SecretVersionsListEntry
    secretVersionsListEntry_versionStages,
    secretVersionsListEntry_lastAccessedDate,
    secretVersionsListEntry_kmsKeyIds,
    secretVersionsListEntry_createdDate,
    secretVersionsListEntry_versionId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ValidationErrorsEntry
    validationErrorsEntry_errorMessage,
    validationErrorsEntry_checkName,
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
