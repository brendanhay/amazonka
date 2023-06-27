{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PaymentCryptography.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Lens
  ( -- * Operations

    -- ** CreateAlias
    createAlias_keyArn,
    createAlias_aliasName,
    createAliasResponse_httpStatus,
    createAliasResponse_alias,

    -- ** CreateKey
    createKey_enabled,
    createKey_keyCheckValueAlgorithm,
    createKey_tags,
    createKey_exportable,
    createKey_keyAttributes,
    createKeyResponse_httpStatus,
    createKeyResponse_key,

    -- ** DeleteAlias
    deleteAlias_aliasName,
    deleteAliasResponse_httpStatus,

    -- ** DeleteKey
    deleteKey_deleteKeyInDays,
    deleteKey_keyIdentifier,
    deleteKeyResponse_httpStatus,
    deleteKeyResponse_key,

    -- ** ExportKey
    exportKey_exportKeyIdentifier,
    exportKey_keyMaterial,
    exportKeyResponse_wrappedKey,
    exportKeyResponse_httpStatus,

    -- ** GetAlias
    getAlias_aliasName,
    getAliasResponse_httpStatus,
    getAliasResponse_alias,

    -- ** GetKey
    getKey_keyIdentifier,
    getKeyResponse_httpStatus,
    getKeyResponse_key,

    -- ** GetParametersForExport
    getParametersForExport_keyMaterialType,
    getParametersForExport_signingKeyAlgorithm,
    getParametersForExportResponse_httpStatus,
    getParametersForExportResponse_exportToken,
    getParametersForExportResponse_parametersValidUntilTimestamp,
    getParametersForExportResponse_signingKeyAlgorithm,
    getParametersForExportResponse_signingKeyCertificate,
    getParametersForExportResponse_signingKeyCertificateChain,

    -- ** GetParametersForImport
    getParametersForImport_keyMaterialType,
    getParametersForImport_wrappingKeyAlgorithm,
    getParametersForImportResponse_httpStatus,
    getParametersForImportResponse_importToken,
    getParametersForImportResponse_parametersValidUntilTimestamp,
    getParametersForImportResponse_wrappingKeyAlgorithm,
    getParametersForImportResponse_wrappingKeyCertificate,
    getParametersForImportResponse_wrappingKeyCertificateChain,

    -- ** GetPublicKeyCertificate
    getPublicKeyCertificate_keyIdentifier,
    getPublicKeyCertificateResponse_httpStatus,
    getPublicKeyCertificateResponse_keyCertificate,
    getPublicKeyCertificateResponse_keyCertificateChain,

    -- ** ImportKey
    importKey_enabled,
    importKey_keyCheckValueAlgorithm,
    importKey_tags,
    importKey_keyMaterial,
    importKeyResponse_httpStatus,
    importKeyResponse_key,

    -- ** ListAliases
    listAliases_maxResults,
    listAliases_nextToken,
    listAliasesResponse_nextToken,
    listAliasesResponse_httpStatus,
    listAliasesResponse_aliases,

    -- ** ListKeys
    listKeys_keyState,
    listKeys_maxResults,
    listKeys_nextToken,
    listKeysResponse_nextToken,
    listKeysResponse_httpStatus,
    listKeysResponse_keys,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** RestoreKey
    restoreKey_keyIdentifier,
    restoreKeyResponse_httpStatus,
    restoreKeyResponse_key,

    -- ** StartKeyUsage
    startKeyUsage_keyIdentifier,
    startKeyUsageResponse_httpStatus,
    startKeyUsageResponse_key,

    -- ** StopKeyUsage
    stopKeyUsage_keyIdentifier,
    stopKeyUsageResponse_httpStatus,
    stopKeyUsageResponse_key,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAlias
    updateAlias_keyArn,
    updateAlias_aliasName,
    updateAliasResponse_httpStatus,
    updateAliasResponse_alias,

    -- * Types

    -- ** Alias
    alias_keyArn,
    alias_aliasName,

    -- ** ExportKeyMaterial
    exportKeyMaterial_tr31KeyBlock,
    exportKeyMaterial_tr34KeyBlock,

    -- ** ExportTr31KeyBlock
    exportTr31KeyBlock_wrappingKeyIdentifier,

    -- ** ExportTr34KeyBlock
    exportTr34KeyBlock_randomNonce,
    exportTr34KeyBlock_certificateAuthorityPublicKeyIdentifier,
    exportTr34KeyBlock_exportToken,
    exportTr34KeyBlock_keyBlockFormat,
    exportTr34KeyBlock_wrappingKeyCertificate,

    -- ** ImportKeyMaterial
    importKeyMaterial_rootCertificatePublicKey,
    importKeyMaterial_tr31KeyBlock,
    importKeyMaterial_tr34KeyBlock,
    importKeyMaterial_trustedCertificatePublicKey,

    -- ** ImportTr31KeyBlock
    importTr31KeyBlock_wrappedKeyBlock,
    importTr31KeyBlock_wrappingKeyIdentifier,

    -- ** ImportTr34KeyBlock
    importTr34KeyBlock_randomNonce,
    importTr34KeyBlock_certificateAuthorityPublicKeyIdentifier,
    importTr34KeyBlock_importToken,
    importTr34KeyBlock_keyBlockFormat,
    importTr34KeyBlock_signingKeyCertificate,
    importTr34KeyBlock_wrappedKeyBlock,

    -- ** Key
    key_deletePendingTimestamp,
    key_deleteTimestamp,
    key_usageStartTimestamp,
    key_usageStopTimestamp,
    key_createTimestamp,
    key_enabled,
    key_exportable,
    key_keyArn,
    key_keyAttributes,
    key_keyCheckValue,
    key_keyCheckValueAlgorithm,
    key_keyOrigin,
    key_keyState,

    -- ** KeyAttributes
    keyAttributes_keyAlgorithm,
    keyAttributes_keyClass,
    keyAttributes_keyModesOfUse,
    keyAttributes_keyUsage,

    -- ** KeyModesOfUse
    keyModesOfUse_decrypt,
    keyModesOfUse_deriveKey,
    keyModesOfUse_encrypt,
    keyModesOfUse_generate,
    keyModesOfUse_noRestrictions,
    keyModesOfUse_sign,
    keyModesOfUse_unwrap,
    keyModesOfUse_verify,
    keyModesOfUse_wrap,

    -- ** KeySummary
    keySummary_enabled,
    keySummary_exportable,
    keySummary_keyArn,
    keySummary_keyAttributes,
    keySummary_keyCheckValue,
    keySummary_keyState,

    -- ** RootCertificatePublicKey
    rootCertificatePublicKey_keyAttributes,
    rootCertificatePublicKey_publicKeyCertificate,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TrustedCertificatePublicKey
    trustedCertificatePublicKey_certificateAuthorityPublicKeyIdentifier,
    trustedCertificatePublicKey_keyAttributes,
    trustedCertificatePublicKey_publicKeyCertificate,

    -- ** WrappedKey
    wrappedKey_keyMaterial,
    wrappedKey_wrappedKeyMaterialFormat,
    wrappedKey_wrappingKeyArn,
  )
where

import Amazonka.PaymentCryptography.CreateAlias
import Amazonka.PaymentCryptography.CreateKey
import Amazonka.PaymentCryptography.DeleteAlias
import Amazonka.PaymentCryptography.DeleteKey
import Amazonka.PaymentCryptography.ExportKey
import Amazonka.PaymentCryptography.GetAlias
import Amazonka.PaymentCryptography.GetKey
import Amazonka.PaymentCryptography.GetParametersForExport
import Amazonka.PaymentCryptography.GetParametersForImport
import Amazonka.PaymentCryptography.GetPublicKeyCertificate
import Amazonka.PaymentCryptography.ImportKey
import Amazonka.PaymentCryptography.ListAliases
import Amazonka.PaymentCryptography.ListKeys
import Amazonka.PaymentCryptography.ListTagsForResource
import Amazonka.PaymentCryptography.RestoreKey
import Amazonka.PaymentCryptography.StartKeyUsage
import Amazonka.PaymentCryptography.StopKeyUsage
import Amazonka.PaymentCryptography.TagResource
import Amazonka.PaymentCryptography.Types.Alias
import Amazonka.PaymentCryptography.Types.ExportKeyMaterial
import Amazonka.PaymentCryptography.Types.ExportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ExportTr34KeyBlock
import Amazonka.PaymentCryptography.Types.ImportKeyMaterial
import Amazonka.PaymentCryptography.Types.ImportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ImportTr34KeyBlock
import Amazonka.PaymentCryptography.Types.Key
import Amazonka.PaymentCryptography.Types.KeyAttributes
import Amazonka.PaymentCryptography.Types.KeyModesOfUse
import Amazonka.PaymentCryptography.Types.KeySummary
import Amazonka.PaymentCryptography.Types.RootCertificatePublicKey
import Amazonka.PaymentCryptography.Types.Tag
import Amazonka.PaymentCryptography.Types.TrustedCertificatePublicKey
import Amazonka.PaymentCryptography.Types.WrappedKey
import Amazonka.PaymentCryptography.UntagResource
import Amazonka.PaymentCryptography.UpdateAlias
