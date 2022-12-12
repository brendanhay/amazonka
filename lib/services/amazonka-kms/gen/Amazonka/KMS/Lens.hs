{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KMS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Lens
  ( -- * Operations

    -- ** CancelKeyDeletion
    cancelKeyDeletion_keyId,
    cancelKeyDeletionResponse_keyId,
    cancelKeyDeletionResponse_httpStatus,

    -- ** ConnectCustomKeyStore
    connectCustomKeyStore_customKeyStoreId,
    connectCustomKeyStoreResponse_httpStatus,

    -- ** CreateAlias
    createAlias_aliasName,
    createAlias_targetKeyId,

    -- ** CreateCustomKeyStore
    createCustomKeyStore_cloudHsmClusterId,
    createCustomKeyStore_customKeyStoreType,
    createCustomKeyStore_keyStorePassword,
    createCustomKeyStore_trustAnchorCertificate,
    createCustomKeyStore_xksProxyAuthenticationCredential,
    createCustomKeyStore_xksProxyConnectivity,
    createCustomKeyStore_xksProxyUriEndpoint,
    createCustomKeyStore_xksProxyUriPath,
    createCustomKeyStore_xksProxyVpcEndpointServiceName,
    createCustomKeyStore_customKeyStoreName,
    createCustomKeyStoreResponse_customKeyStoreId,
    createCustomKeyStoreResponse_httpStatus,

    -- ** CreateGrant
    createGrant_constraints,
    createGrant_grantTokens,
    createGrant_name,
    createGrant_retiringPrincipal,
    createGrant_keyId,
    createGrant_granteePrincipal,
    createGrant_operations,
    createGrantResponse_grantId,
    createGrantResponse_grantToken,
    createGrantResponse_httpStatus,

    -- ** CreateKey
    createKey_bypassPolicyLockoutSafetyCheck,
    createKey_customKeyStoreId,
    createKey_customerMasterKeySpec,
    createKey_description,
    createKey_keySpec,
    createKey_keyUsage,
    createKey_multiRegion,
    createKey_origin,
    createKey_policy,
    createKey_tags,
    createKey_xksKeyId,
    createKeyResponse_keyMetadata,
    createKeyResponse_httpStatus,

    -- ** Decrypt
    decrypt_encryptionAlgorithm,
    decrypt_encryptionContext,
    decrypt_grantTokens,
    decrypt_keyId,
    decrypt_ciphertextBlob,
    decryptResponse_encryptionAlgorithm,
    decryptResponse_keyId,
    decryptResponse_plaintext,
    decryptResponse_httpStatus,

    -- ** DeleteAlias
    deleteAlias_aliasName,

    -- ** DeleteCustomKeyStore
    deleteCustomKeyStore_customKeyStoreId,
    deleteCustomKeyStoreResponse_httpStatus,

    -- ** DeleteImportedKeyMaterial
    deleteImportedKeyMaterial_keyId,

    -- ** DescribeCustomKeyStores
    describeCustomKeyStores_customKeyStoreId,
    describeCustomKeyStores_customKeyStoreName,
    describeCustomKeyStores_limit,
    describeCustomKeyStores_marker,
    describeCustomKeyStoresResponse_customKeyStores,
    describeCustomKeyStoresResponse_nextMarker,
    describeCustomKeyStoresResponse_truncated,
    describeCustomKeyStoresResponse_httpStatus,

    -- ** DescribeKey
    describeKey_grantTokens,
    describeKey_keyId,
    describeKeyResponse_keyMetadata,
    describeKeyResponse_httpStatus,

    -- ** DisableKey
    disableKey_keyId,

    -- ** DisableKeyRotation
    disableKeyRotation_keyId,

    -- ** DisconnectCustomKeyStore
    disconnectCustomKeyStore_customKeyStoreId,
    disconnectCustomKeyStoreResponse_httpStatus,

    -- ** EnableKey
    enableKey_keyId,

    -- ** EnableKeyRotation
    enableKeyRotation_keyId,

    -- ** Encrypt
    encrypt_encryptionAlgorithm,
    encrypt_encryptionContext,
    encrypt_grantTokens,
    encrypt_keyId,
    encrypt_plaintext,
    encryptResponse_ciphertextBlob,
    encryptResponse_encryptionAlgorithm,
    encryptResponse_keyId,
    encryptResponse_httpStatus,

    -- ** GenerateDataKey
    generateDataKey_encryptionContext,
    generateDataKey_grantTokens,
    generateDataKey_keySpec,
    generateDataKey_numberOfBytes,
    generateDataKey_keyId,
    generateDataKeyResponse_httpStatus,
    generateDataKeyResponse_keyId,
    generateDataKeyResponse_plaintext,
    generateDataKeyResponse_ciphertextBlob,

    -- ** GenerateDataKeyPair
    generateDataKeyPair_encryptionContext,
    generateDataKeyPair_grantTokens,
    generateDataKeyPair_keyId,
    generateDataKeyPair_keyPairSpec,
    generateDataKeyPairResponse_keyId,
    generateDataKeyPairResponse_keyPairSpec,
    generateDataKeyPairResponse_privateKeyCiphertextBlob,
    generateDataKeyPairResponse_privateKeyPlaintext,
    generateDataKeyPairResponse_publicKey,
    generateDataKeyPairResponse_httpStatus,

    -- ** GenerateDataKeyPairWithoutPlaintext
    generateDataKeyPairWithoutPlaintext_encryptionContext,
    generateDataKeyPairWithoutPlaintext_grantTokens,
    generateDataKeyPairWithoutPlaintext_keyId,
    generateDataKeyPairWithoutPlaintext_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_keyId,
    generateDataKeyPairWithoutPlaintextResponse_keyPairSpec,
    generateDataKeyPairWithoutPlaintextResponse_privateKeyCiphertextBlob,
    generateDataKeyPairWithoutPlaintextResponse_publicKey,
    generateDataKeyPairWithoutPlaintextResponse_httpStatus,

    -- ** GenerateDataKeyWithoutPlaintext
    generateDataKeyWithoutPlaintext_encryptionContext,
    generateDataKeyWithoutPlaintext_grantTokens,
    generateDataKeyWithoutPlaintext_keySpec,
    generateDataKeyWithoutPlaintext_numberOfBytes,
    generateDataKeyWithoutPlaintext_keyId,
    generateDataKeyWithoutPlaintextResponse_ciphertextBlob,
    generateDataKeyWithoutPlaintextResponse_keyId,
    generateDataKeyWithoutPlaintextResponse_httpStatus,

    -- ** GenerateMac
    generateMac_grantTokens,
    generateMac_message,
    generateMac_keyId,
    generateMac_macAlgorithm,
    generateMacResponse_keyId,
    generateMacResponse_mac,
    generateMacResponse_macAlgorithm,
    generateMacResponse_httpStatus,

    -- ** GenerateRandom
    generateRandom_customKeyStoreId,
    generateRandom_numberOfBytes,
    generateRandomResponse_plaintext,
    generateRandomResponse_httpStatus,

    -- ** GetKeyPolicy
    getKeyPolicy_keyId,
    getKeyPolicy_policyName,
    getKeyPolicyResponse_policy,
    getKeyPolicyResponse_httpStatus,

    -- ** GetKeyRotationStatus
    getKeyRotationStatus_keyId,
    getKeyRotationStatusResponse_keyRotationEnabled,
    getKeyRotationStatusResponse_httpStatus,

    -- ** GetParametersForImport
    getParametersForImport_keyId,
    getParametersForImport_wrappingAlgorithm,
    getParametersForImport_wrappingKeySpec,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_keyId,
    getParametersForImportResponse_parametersValidTo,
    getParametersForImportResponse_publicKey,
    getParametersForImportResponse_httpStatus,

    -- ** GetPublicKey
    getPublicKey_grantTokens,
    getPublicKey_keyId,
    getPublicKeyResponse_customerMasterKeySpec,
    getPublicKeyResponse_encryptionAlgorithms,
    getPublicKeyResponse_keyId,
    getPublicKeyResponse_keySpec,
    getPublicKeyResponse_keyUsage,
    getPublicKeyResponse_publicKey,
    getPublicKeyResponse_signingAlgorithms,
    getPublicKeyResponse_httpStatus,

    -- ** ImportKeyMaterial
    importKeyMaterial_expirationModel,
    importKeyMaterial_validTo,
    importKeyMaterial_keyId,
    importKeyMaterial_importToken,
    importKeyMaterial_encryptedKeyMaterial,
    importKeyMaterialResponse_httpStatus,

    -- ** ListAliases
    listAliases_keyId,
    listAliases_limit,
    listAliases_marker,
    listAliasesResponse_aliases,
    listAliasesResponse_nextMarker,
    listAliasesResponse_truncated,
    listAliasesResponse_httpStatus,

    -- ** ListGrants
    listGrants_grantId,
    listGrants_granteePrincipal,
    listGrants_limit,
    listGrants_marker,
    listGrants_keyId,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
    listGrantsResponse_truncated,

    -- ** ListKeyPolicies
    listKeyPolicies_limit,
    listKeyPolicies_marker,
    listKeyPolicies_keyId,
    listKeyPoliciesResponse_nextMarker,
    listKeyPoliciesResponse_policyNames,
    listKeyPoliciesResponse_truncated,
    listKeyPoliciesResponse_httpStatus,

    -- ** ListKeys
    listKeys_limit,
    listKeys_marker,
    listKeysResponse_keys,
    listKeysResponse_nextMarker,
    listKeysResponse_truncated,
    listKeysResponse_httpStatus,

    -- ** ListResourceTags
    listResourceTags_limit,
    listResourceTags_marker,
    listResourceTags_keyId,
    listResourceTagsResponse_nextMarker,
    listResourceTagsResponse_tags,
    listResourceTagsResponse_truncated,
    listResourceTagsResponse_httpStatus,

    -- ** ListRetirableGrants
    listRetirableGrants_limit,
    listRetirableGrants_marker,
    listRetirableGrants_retiringPrincipal,
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
    listGrantsResponse_truncated,

    -- ** PutKeyPolicy
    putKeyPolicy_bypassPolicyLockoutSafetyCheck,
    putKeyPolicy_keyId,
    putKeyPolicy_policyName,
    putKeyPolicy_policy,

    -- ** ReEncrypt
    reEncrypt_destinationEncryptionAlgorithm,
    reEncrypt_destinationEncryptionContext,
    reEncrypt_grantTokens,
    reEncrypt_sourceEncryptionAlgorithm,
    reEncrypt_sourceEncryptionContext,
    reEncrypt_sourceKeyId,
    reEncrypt_ciphertextBlob,
    reEncrypt_destinationKeyId,
    reEncryptResponse_ciphertextBlob,
    reEncryptResponse_destinationEncryptionAlgorithm,
    reEncryptResponse_keyId,
    reEncryptResponse_sourceEncryptionAlgorithm,
    reEncryptResponse_sourceKeyId,
    reEncryptResponse_httpStatus,

    -- ** ReplicateKey
    replicateKey_bypassPolicyLockoutSafetyCheck,
    replicateKey_description,
    replicateKey_policy,
    replicateKey_tags,
    replicateKey_keyId,
    replicateKey_replicaRegion,
    replicateKeyResponse_replicaKeyMetadata,
    replicateKeyResponse_replicaPolicy,
    replicateKeyResponse_replicaTags,
    replicateKeyResponse_httpStatus,

    -- ** RetireGrant
    retireGrant_grantId,
    retireGrant_grantToken,
    retireGrant_keyId,

    -- ** RevokeGrant
    revokeGrant_keyId,
    revokeGrant_grantId,

    -- ** ScheduleKeyDeletion
    scheduleKeyDeletion_pendingWindowInDays,
    scheduleKeyDeletion_keyId,
    scheduleKeyDeletionResponse_deletionDate,
    scheduleKeyDeletionResponse_keyId,
    scheduleKeyDeletionResponse_keyState,
    scheduleKeyDeletionResponse_pendingWindowInDays,
    scheduleKeyDeletionResponse_httpStatus,

    -- ** Sign
    sign_grantTokens,
    sign_messageType,
    sign_keyId,
    sign_message,
    sign_signingAlgorithm,
    signResponse_keyId,
    signResponse_signature,
    signResponse_signingAlgorithm,
    signResponse_httpStatus,

    -- ** TagResource
    tagResource_keyId,
    tagResource_tags,

    -- ** UntagResource
    untagResource_keyId,
    untagResource_tagKeys,

    -- ** UpdateAlias
    updateAlias_aliasName,
    updateAlias_targetKeyId,

    -- ** UpdateCustomKeyStore
    updateCustomKeyStore_cloudHsmClusterId,
    updateCustomKeyStore_keyStorePassword,
    updateCustomKeyStore_newCustomKeyStoreName,
    updateCustomKeyStore_xksProxyAuthenticationCredential,
    updateCustomKeyStore_xksProxyConnectivity,
    updateCustomKeyStore_xksProxyUriEndpoint,
    updateCustomKeyStore_xksProxyUriPath,
    updateCustomKeyStore_xksProxyVpcEndpointServiceName,
    updateCustomKeyStore_customKeyStoreId,
    updateCustomKeyStoreResponse_httpStatus,

    -- ** UpdateKeyDescription
    updateKeyDescription_keyId,
    updateKeyDescription_description,

    -- ** UpdatePrimaryRegion
    updatePrimaryRegion_keyId,
    updatePrimaryRegion_primaryRegion,

    -- ** Verify
    verify_grantTokens,
    verify_messageType,
    verify_keyId,
    verify_message,
    verify_signature,
    verify_signingAlgorithm,
    verifyResponse_keyId,
    verifyResponse_signatureValid,
    verifyResponse_signingAlgorithm,
    verifyResponse_httpStatus,

    -- ** VerifyMac
    verifyMac_grantTokens,
    verifyMac_message,
    verifyMac_keyId,
    verifyMac_macAlgorithm,
    verifyMac_mac,
    verifyMacResponse_keyId,
    verifyMacResponse_macAlgorithm,
    verifyMacResponse_macValid,
    verifyMacResponse_httpStatus,

    -- * Types

    -- ** AliasListEntry
    aliasListEntry_aliasArn,
    aliasListEntry_aliasName,
    aliasListEntry_creationDate,
    aliasListEntry_lastUpdatedDate,
    aliasListEntry_targetKeyId,

    -- ** CustomKeyStoresListEntry
    customKeyStoresListEntry_cloudHsmClusterId,
    customKeyStoresListEntry_connectionErrorCode,
    customKeyStoresListEntry_connectionState,
    customKeyStoresListEntry_creationDate,
    customKeyStoresListEntry_customKeyStoreId,
    customKeyStoresListEntry_customKeyStoreName,
    customKeyStoresListEntry_customKeyStoreType,
    customKeyStoresListEntry_trustAnchorCertificate,
    customKeyStoresListEntry_xksProxyConfiguration,

    -- ** GrantConstraints
    grantConstraints_encryptionContextEquals,
    grantConstraints_encryptionContextSubset,

    -- ** GrantListEntry
    grantListEntry_constraints,
    grantListEntry_creationDate,
    grantListEntry_grantId,
    grantListEntry_granteePrincipal,
    grantListEntry_issuingAccount,
    grantListEntry_keyId,
    grantListEntry_name,
    grantListEntry_operations,
    grantListEntry_retiringPrincipal,

    -- ** KeyListEntry
    keyListEntry_keyArn,
    keyListEntry_keyId,

    -- ** KeyMetadata
    keyMetadata_aWSAccountId,
    keyMetadata_arn,
    keyMetadata_cloudHsmClusterId,
    keyMetadata_creationDate,
    keyMetadata_customKeyStoreId,
    keyMetadata_customerMasterKeySpec,
    keyMetadata_deletionDate,
    keyMetadata_description,
    keyMetadata_enabled,
    keyMetadata_encryptionAlgorithms,
    keyMetadata_expirationModel,
    keyMetadata_keyManager,
    keyMetadata_keySpec,
    keyMetadata_keyState,
    keyMetadata_keyUsage,
    keyMetadata_macAlgorithms,
    keyMetadata_multiRegion,
    keyMetadata_multiRegionConfiguration,
    keyMetadata_origin,
    keyMetadata_pendingDeletionWindowInDays,
    keyMetadata_signingAlgorithms,
    keyMetadata_validTo,
    keyMetadata_xksKeyConfiguration,
    keyMetadata_keyId,

    -- ** ListGrantsResponse
    listGrantsResponse_grants,
    listGrantsResponse_nextMarker,
    listGrantsResponse_truncated,

    -- ** MultiRegionConfiguration
    multiRegionConfiguration_multiRegionKeyType,
    multiRegionConfiguration_primaryKey,
    multiRegionConfiguration_replicaKeys,

    -- ** MultiRegionKey
    multiRegionKey_arn,
    multiRegionKey_region,

    -- ** Tag
    tag_tagKey,
    tag_tagValue,

    -- ** XksKeyConfigurationType
    xksKeyConfigurationType_id,

    -- ** XksProxyAuthenticationCredentialType
    xksProxyAuthenticationCredentialType_accessKeyId,
    xksProxyAuthenticationCredentialType_rawSecretAccessKey,

    -- ** XksProxyConfigurationType
    xksProxyConfigurationType_accessKeyId,
    xksProxyConfigurationType_connectivity,
    xksProxyConfigurationType_uriEndpoint,
    xksProxyConfigurationType_uriPath,
    xksProxyConfigurationType_vpcEndpointServiceName,
  )
where

import Amazonka.KMS.CancelKeyDeletion
import Amazonka.KMS.ConnectCustomKeyStore
import Amazonka.KMS.CreateAlias
import Amazonka.KMS.CreateCustomKeyStore
import Amazonka.KMS.CreateGrant
import Amazonka.KMS.CreateKey
import Amazonka.KMS.Decrypt
import Amazonka.KMS.DeleteAlias
import Amazonka.KMS.DeleteCustomKeyStore
import Amazonka.KMS.DeleteImportedKeyMaterial
import Amazonka.KMS.DescribeCustomKeyStores
import Amazonka.KMS.DescribeKey
import Amazonka.KMS.DisableKey
import Amazonka.KMS.DisableKeyRotation
import Amazonka.KMS.DisconnectCustomKeyStore
import Amazonka.KMS.EnableKey
import Amazonka.KMS.EnableKeyRotation
import Amazonka.KMS.Encrypt
import Amazonka.KMS.GenerateDataKey
import Amazonka.KMS.GenerateDataKeyPair
import Amazonka.KMS.GenerateDataKeyPairWithoutPlaintext
import Amazonka.KMS.GenerateDataKeyWithoutPlaintext
import Amazonka.KMS.GenerateMac
import Amazonka.KMS.GenerateRandom
import Amazonka.KMS.GetKeyPolicy
import Amazonka.KMS.GetKeyRotationStatus
import Amazonka.KMS.GetParametersForImport
import Amazonka.KMS.GetPublicKey
import Amazonka.KMS.ImportKeyMaterial
import Amazonka.KMS.ListAliases
import Amazonka.KMS.ListGrants
import Amazonka.KMS.ListKeyPolicies
import Amazonka.KMS.ListKeys
import Amazonka.KMS.ListResourceTags
import Amazonka.KMS.ListRetirableGrants
import Amazonka.KMS.PutKeyPolicy
import Amazonka.KMS.ReEncrypt
import Amazonka.KMS.ReplicateKey
import Amazonka.KMS.RetireGrant
import Amazonka.KMS.RevokeGrant
import Amazonka.KMS.ScheduleKeyDeletion
import Amazonka.KMS.Sign
import Amazonka.KMS.TagResource
import Amazonka.KMS.Types.AliasListEntry
import Amazonka.KMS.Types.CustomKeyStoresListEntry
import Amazonka.KMS.Types.GrantConstraints
import Amazonka.KMS.Types.GrantListEntry
import Amazonka.KMS.Types.KeyListEntry
import Amazonka.KMS.Types.KeyMetadata
import Amazonka.KMS.Types.ListGrantsResponse
import Amazonka.KMS.Types.MultiRegionConfiguration
import Amazonka.KMS.Types.MultiRegionKey
import Amazonka.KMS.Types.Tag
import Amazonka.KMS.Types.XksKeyConfigurationType
import Amazonka.KMS.Types.XksProxyAuthenticationCredentialType
import Amazonka.KMS.Types.XksProxyConfigurationType
import Amazonka.KMS.UntagResource
import Amazonka.KMS.UpdateAlias
import Amazonka.KMS.UpdateCustomKeyStore
import Amazonka.KMS.UpdateKeyDescription
import Amazonka.KMS.UpdatePrimaryRegion
import Amazonka.KMS.Verify
import Amazonka.KMS.VerifyMac
