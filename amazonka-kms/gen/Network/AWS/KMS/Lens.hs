{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Lens
  ( -- * Operations

    -- ** DisableKeyRotation
    disableKeyRotation_keyId,

    -- ** DeleteAlias
    deleteAlias_aliasName,

    -- ** ListGrants
    listGrants_granteePrincipal,
    listGrants_grantId,
    listGrants_limit,
    listGrants_marker,
    listGrants_keyId,
    listGrantsResponse_nextMarker,
    listGrantsResponse_grants,
    listGrantsResponse_truncated,

    -- ** Verify
    verify_grantTokens,
    verify_messageType,
    verify_keyId,
    verify_message,
    verify_signature,
    verify_signingAlgorithm,
    verifyResponse_signingAlgorithm,
    verifyResponse_signatureValid,
    verifyResponse_keyId,
    verifyResponse_httpStatus,

    -- ** CreateCustomKeyStore
    createCustomKeyStore_customKeyStoreName,
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,

    -- ** UpdateAlias
    updateAlias_aliasName,
    updateAlias_targetKeyId,

    -- ** GenerateDataKey
    generateDataKey_grantTokens,
    generateDataKey_numberOfBytes,
    generateDataKey_encryptionContext,
    generateDataKey_keySpec,
    generateDataKey_keyId,
    generateDataKeyResponse_httpStatus,
    generateDataKeyResponse_keyId,
    generateDataKeyResponse_plaintext,
    generateDataKeyResponse_ciphertextBlob,

    -- ** DeleteImportedKeyMaterial
    deleteImportedKeyMaterial_keyId,

    -- ** ImportKeyMaterial
    importKeyMaterial_validTo,
    importKeyMaterial_expirationModel,
    importKeyMaterial_keyId,
    importKeyMaterial_importToken,
    importKeyMaterial_encryptedKeyMaterial,
    importKeyMaterialResponse_httpStatus,

    -- ** GetKeyRotationStatus
    getKeyRotationStatus_keyId,
    getKeyRotationStatusResponse_keyRotationEnabled,
    getKeyRotationStatusResponse_httpStatus,

    -- ** ListResourceTags
    listResourceTags_limit,
    listResourceTags_marker,
    listResourceTags_keyId,
    listResourceTagsResponse_nextMarker,
    listResourceTagsResponse_tags,
    listResourceTagsResponse_truncated,
    listResourceTagsResponse_httpStatus,

    -- ** PutKeyPolicy
    putKeyPolicy_bypassPolicyLockoutSafetyCheck,
    putKeyPolicy_keyId,
    putKeyPolicy_policyName,
    putKeyPolicy_policy,

    -- ** ListKeyPolicies
    listKeyPolicies_limit,
    listKeyPolicies_marker,
    listKeyPolicies_keyId,
    listKeyPoliciesResponse_nextMarker,
    listKeyPoliciesResponse_policyNames,
    listKeyPoliciesResponse_truncated,
    listKeyPoliciesResponse_httpStatus,

    -- ** DisableKey
    disableKey_keyId,

    -- ** UntagResource
    untagResource_keyId,
    untagResource_tagKeys,

    -- ** DisconnectCustomKeyStore
    disconnectCustomKeyStore_customKeyStoreId,
    disconnectCustomKeyStoreResponse_httpStatus,

    -- ** GenerateRandom
    generateRandom_customKeyStoreId,
    generateRandom_numberOfBytes,
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,

    -- ** GetPublicKey
    getPublicKey_grantTokens,
    getPublicKey_keyId,
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_httpStatus,

    -- ** ReEncrypt
    reEncrypt_destinationEncryptionContext,
    reEncrypt_grantTokens,
    reEncrypt_sourceEncryptionContext,
    reEncrypt_sourceKeyId,
    reEncrypt_destinationEncryptionAlgorithm,
    reEncrypt_sourceEncryptionAlgorithm,
    reEncrypt_ciphertextBlob,
    reEncrypt_destinationKeyId,
    reEncryptResponse_sourceKeyId,
    reEncryptResponse_destinationEncryptionAlgorithm,
    reEncryptResponse_ciphertextBlob,
    reEncryptResponse_sourceEncryptionAlgorithm,
    reEncryptResponse_keyId,
    reEncryptResponse_httpStatus,

    -- ** TagResource
    tagResource_keyId,
    tagResource_tags,

    -- ** ListRetirableGrants
    listRetirableGrants_limit,
    listRetirableGrants_marker,
    listRetirableGrants_retiringPrincipal,
    listGrantsResponse_nextMarker,
    listGrantsResponse_grants,
    listGrantsResponse_truncated,

    -- ** ConnectCustomKeyStore
    connectCustomKeyStore_customKeyStoreId,
    connectCustomKeyStoreResponse_httpStatus,

    -- ** GetParametersForImport
    getParametersForImport_keyId,
    getParametersForImport_wrappingAlgorithm,
    getParametersForImport_wrappingKeySpec,
    getParametersForImportResponse_parametersValidTo,
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_httpStatus,

    -- ** DescribeKey
    describeKey_grantTokens,
    describeKey_keyId,
    describeKeyResponse_keyMetadata,
    describeKeyResponse_httpStatus,

    -- ** DeleteCustomKeyStore
    deleteCustomKeyStore_customKeyStoreId,
    deleteCustomKeyStoreResponse_httpStatus,

    -- ** UpdateCustomKeyStore
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_customKeyStoreId,
    updateCustomKeyStoreResponse_httpStatus,

    -- ** GenerateDataKeyWithoutPlaintext
    generateDataKeyWithoutPlaintext_grantTokens,
    generateDataKeyWithoutPlaintext_numberOfBytes,
    generateDataKeyWithoutPlaintext_encryptionContext,
    generateDataKeyWithoutPlaintext_keySpec,
    generateDataKeyWithoutPlaintext_keyId,
    generateDataKeyWithoutPlaintextResponse_ciphertextBlob,
    generateDataKeyWithoutPlaintextResponse_keyId,
    generateDataKeyWithoutPlaintextResponse_httpStatus,

    -- ** Encrypt
    encrypt_grantTokens,
    encrypt_encryptionAlgorithm,
    encrypt_encryptionContext,
    encrypt_keyId,
    encrypt_plaintext,
    encryptResponse_encryptionAlgorithm,
    encryptResponse_ciphertextBlob,
    encryptResponse_keyId,
    encryptResponse_httpStatus,

    -- ** GetKeyPolicy
    getKeyPolicy_keyId,
    getKeyPolicy_policyName,
    getKeyPolicyResponse_policy,
    getKeyPolicyResponse_httpStatus,

    -- ** ListKeys
    listKeys_limit,
    listKeys_marker,
    listKeysResponse_nextMarker,
    listKeysResponse_keys,
    listKeysResponse_truncated,
    listKeysResponse_httpStatus,

    -- ** RevokeGrant
    revokeGrant_keyId,
    revokeGrant_grantId,

    -- ** ScheduleKeyDeletion
    scheduleKeyDeletion_pendingWindowInDays,
    scheduleKeyDeletion_keyId,
    scheduleKeyDeletionResponse_deletionDate,
    scheduleKeyDeletionResponse_keyId,
    scheduleKeyDeletionResponse_httpStatus,

    -- ** EnableKey
    enableKey_keyId,

    -- ** GenerateDataKeyPair
    generateDataKeyPair_grantTokens,
    generateDataKeyPair_encryptionContext,
    generateDataKeyPair_keyId,
    generateDataKeyPair_keyPairSpec,
    generateDataKeyPairResponse_publicKey,
    generateDataKeyPairResponse_keyPairSpec,
    generateDataKeyPairResponse_privateKeyCiphertextBlob,
    generateDataKeyPairResponse_privateKeyPlaintext,
    generateDataKeyPairResponse_keyId,
    generateDataKeyPairResponse_httpStatus,

    -- ** RetireGrant
    retireGrant_grantToken,
    retireGrant_grantId,
    retireGrant_keyId,

    -- ** CreateKey
    createKey_origin,
    createKey_customKeyStoreId,
    createKey_bypassPolicyLockoutSafetyCheck,
    createKey_tags,
    createKey_description,
    createKey_policy,
    createKey_keyUsage,
    createKey_customerMasterKeySpec,
    createKeyResponse_keyMetadata,
    createKeyResponse_httpStatus,

    -- ** Sign
    sign_grantTokens,
    sign_messageType,
    sign_keyId,
    sign_message,
    sign_signingAlgorithm,
    signResponse_signingAlgorithm,
    signResponse_signature,
    signResponse_keyId,
    signResponse_httpStatus,

    -- ** UpdateKeyDescription
    updateKeyDescription_keyId,
    updateKeyDescription_description,

    -- ** CancelKeyDeletion
    cancelKeyDeletion_keyId,
    cancelKeyDeletionResponse_keyId,
    cancelKeyDeletionResponse_httpStatus,

    -- ** GenerateDataKeyPairWithoutPlaintext
    generateDataKeyPairWithoutPlaintext_grantTokens,
    generateDataKeyPairWithoutPlaintext_encryptionContext,
    generateDataKeyPairWithoutPlaintext_keyId,
    generateDataKeyPairWithoutPlaintext_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_publicKey,
    generateDataKeyPairWithoutPlaintextResponse_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_privateKeyCiphertextBlob,
    generateDataKeyPairWithoutPlaintextResponse_keyId,
    generateDataKeyPairWithoutPlaintextResponse_httpStatus,

    -- ** DescribeCustomKeyStores
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_marker,
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_httpStatus,

    -- ** Decrypt
    decrypt_grantTokens,
    decrypt_encryptionAlgorithm,
    decrypt_encryptionContext,
    decrypt_keyId,
    decrypt_ciphertextBlob,
    decryptResponse_plaintext,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_keyId,
    decryptResponse_httpStatus,

    -- ** CreateAlias
    createAlias_aliasName,
    createAlias_targetKeyId,

    -- ** EnableKeyRotation
    enableKeyRotation_keyId,

    -- ** ListAliases
    listAliases_limit,
    listAliases_keyId,
    listAliases_marker,
    listAliasesResponse_nextMarker,
    listAliasesResponse_aliases,
    listAliasesResponse_truncated,
    listAliasesResponse_httpStatus,

    -- ** CreateGrant
    createGrant_constraints,
    createGrant_grantTokens,
    createGrant_name,
    createGrant_retiringPrincipal,
    createGrant_keyId,
    createGrant_granteePrincipal,
    createGrant_operations,
    createGrantResponse_grantToken,
    createGrantResponse_grantId,
    createGrantResponse_httpStatus,

    -- * Types

    -- ** AliasListEntry
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_creationDate,
    aliasListEntry_aliasName,
    aliasListEntry_aliasArn,
    aliasListEntry_targetKeyId,

    -- ** CustomKeyStoresListEntry
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_connectionState,
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_connectionErrorCode,

    -- ** GrantConstraints
    grantConstraints_encryptionContextEquals,
    grantConstraints_encryptionContextSubset,

    -- ** GrantListEntry
    grantListEntry_constraints,
    grantListEntry_operations,
    grantListEntry_creationDate,
    grantListEntry_name,
    grantListEntry_granteePrincipal,
    grantListEntry_grantId,
    grantListEntry_issuingAccount,
    grantListEntry_retiringPrincipal,
    grantListEntry_keyId,

    -- ** KeyListEntry
    keyListEntry_keyArn,
    keyListEntry_keyId,

    -- ** KeyMetadata
    keyMetadata_signingAlgorithms,
    keyMetadata_keyManager,
    keyMetadata_origin,
    keyMetadata_aWSAccountId,
    keyMetadata_customKeyStoreId,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_keyState,
    keyMetadata_arn,
    keyMetadata_creationDate,
    keyMetadata_validTo,
    keyMetadata_enabled,
    keyMetadata_expirationModel,
    keyMetadata_description,
    keyMetadata_deletionDate,
    keyMetadata_keyUsage,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_keyId,

    -- ** ListGrantsResponse
    listGrantsResponse_nextMarker,
    listGrantsResponse_grants,
    listGrantsResponse_truncated,

    -- ** Tag
    tag_tagKey,
    tag_tagValue,
  )
where

import Network.AWS.KMS.CancelKeyDeletion
import Network.AWS.KMS.ConnectCustomKeyStore
import Network.AWS.KMS.CreateAlias
import Network.AWS.KMS.CreateCustomKeyStore
import Network.AWS.KMS.CreateGrant
import Network.AWS.KMS.CreateKey
import Network.AWS.KMS.Decrypt
import Network.AWS.KMS.DeleteAlias
import Network.AWS.KMS.DeleteCustomKeyStore
import Network.AWS.KMS.DeleteImportedKeyMaterial
import Network.AWS.KMS.DescribeCustomKeyStores
import Network.AWS.KMS.DescribeKey
import Network.AWS.KMS.DisableKey
import Network.AWS.KMS.DisableKeyRotation
import Network.AWS.KMS.DisconnectCustomKeyStore
import Network.AWS.KMS.EnableKey
import Network.AWS.KMS.EnableKeyRotation
import Network.AWS.KMS.Encrypt
import Network.AWS.KMS.GenerateDataKey
import Network.AWS.KMS.GenerateDataKeyPair
import Network.AWS.KMS.GenerateDataKeyPairWithoutPlaintext
import Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
import Network.AWS.KMS.GenerateRandom
import Network.AWS.KMS.GetKeyPolicy
import Network.AWS.KMS.GetKeyRotationStatus
import Network.AWS.KMS.GetParametersForImport
import Network.AWS.KMS.GetPublicKey
import Network.AWS.KMS.ImportKeyMaterial
import Network.AWS.KMS.ListAliases
import Network.AWS.KMS.ListGrants
import Network.AWS.KMS.ListKeyPolicies
import Network.AWS.KMS.ListKeys
import Network.AWS.KMS.ListResourceTags
import Network.AWS.KMS.ListRetirableGrants
import Network.AWS.KMS.PutKeyPolicy
import Network.AWS.KMS.ReEncrypt
import Network.AWS.KMS.RetireGrant
import Network.AWS.KMS.RevokeGrant
import Network.AWS.KMS.ScheduleKeyDeletion
import Network.AWS.KMS.Sign
import Network.AWS.KMS.TagResource
import Network.AWS.KMS.Types.AliasListEntry
import Network.AWS.KMS.Types.CustomKeyStoresListEntry
import Network.AWS.KMS.Types.GrantConstraints
import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.KMS.Types.KeyListEntry
import Network.AWS.KMS.Types.KeyMetadata
import Network.AWS.KMS.Types.ListGrantsResponse
import Network.AWS.KMS.Types.Tag
import Network.AWS.KMS.UntagResource
import Network.AWS.KMS.UpdateAlias
import Network.AWS.KMS.UpdateCustomKeyStore
import Network.AWS.KMS.UpdateKeyDescription
import Network.AWS.KMS.Verify
