{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KMS where

import Data.Proxy
import Network.AWS.KMS
import Test.AWS.Fixture
import Test.AWS.KMS.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestEncrypt $
--             mkEncrypt
--
--         , requestCreateCustomKeyStore $
--             mkCreateCustomKeyStore
--
--         , requestListGrants $
--             mkListGrants
--
--         , requestDisableKeyRotation $
--             mkDisableKeyRotation
--
--         , requestVerify $
--             mkVerify
--
--         , requestGenerateDataKeyWithoutPlaintext $
--             mkGenerateDataKeyWithoutPlaintext
--
--         , requestUpdateCustomKeyStore $
--             mkUpdateCustomKeyStore
--
--         , requestGetParametersForImport $
--             mkGetParametersForImport
--
--         , requestEnableKeyRotation $
--             mkEnableKeyRotation
--
--         , requestDeleteCustomKeyStore $
--             mkDeleteCustomKeyStore
--
--         , requestCreateAlias $
--             mkCreateAlias
--
--         , requestCreateGrant $
--             mkCreateGrant
--
--         , requestListAliases $
--             mkListAliases
--
--         , requestConnectCustomKeyStore $
--             mkConnectCustomKeyStore
--
--         , requestListRetirableGrants $
--             mkListRetirableGrants
--
--         , requestGetPublicKey $
--             mkGetPublicKey
--
--         , requestGenerateRandom $
--             mkGenerateRandom
--
--         , requestCreateKey $
--             mkCreateKey
--
--         , requestDisableKey $
--             mkDisableKey
--
--         , requestDisconnectCustomKeyStore $
--             mkDisconnectCustomKeyStore
--
--         , requestRetireGrant $
--             mkRetireGrant
--
--         , requestListKeys $
--             mkListKeys
--
--         , requestListResourceTags $
--             mkListResourceTags
--
--         , requestGetKeyRotationStatus $
--             mkGetKeyRotationStatus
--
--         , requestGenerateDataKey $
--             mkGenerateDataKey
--
--         , requestDeleteAlias $
--             mkDeleteAlias
--
--         , requestUpdateAlias $
--             mkUpdateAlias
--
--         , requestDescribeKey $
--             mkDescribeKey
--
--         , requestDescribeCustomKeyStores $
--             mkDescribeCustomKeyStores
--
--         , requestCancelKeyDeletion $
--             mkCancelKeyDeletion
--
--         , requestDecrypt $
--             mkDecrypt
--
--         , requestGenerateDataKeyPairWithoutPlaintext $
--             mkGenerateDataKeyPairWithoutPlaintext
--
--         , requestUpdateKeyDescription $
--             mkUpdateKeyDescription
--
--         , requestReEncrypt $
--             mkReEncrypt
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListKeyPolicies $
--             mkListKeyPolicies
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestSign $
--             mkSign
--
--         , requestScheduleKeyDeletion $
--             mkScheduleKeyDeletion
--
--         , requestGenerateDataKeyPair $
--             mkGenerateDataKeyPair
--
--         , requestPutKeyPolicy $
--             mkPutKeyPolicy
--
--         , requestEnableKey $
--             mkEnableKey
--
--         , requestRevokeGrant $
--             mkRevokeGrant
--
--         , requestGetKeyPolicy $
--             mkGetKeyPolicy
--
--         , requestImportKeyMaterial $
--             mkImportKeyMaterial
--
--         , requestDeleteImportedKeyMaterial $
--             mkDeleteImportedKeyMaterial
--
--           ]

--     , testGroup "response"
--         [ responseEncrypt $
--             mkEncryptResponse
--
--         , responseCreateCustomKeyStore $
--             mkCreateCustomKeyStoreResponse
--
--         , responseListGrants $
--             mkListGrantsResponse
--
--         , responseDisableKeyRotation $
--             mkDisableKeyRotationResponse
--
--         , responseVerify $
--             mkVerifyResponse
--
--         , responseGenerateDataKeyWithoutPlaintext $
--             mkGenerateDataKeyWithoutPlaintextResponse
--
--         , responseUpdateCustomKeyStore $
--             mkUpdateCustomKeyStoreResponse
--
--         , responseGetParametersForImport $
--             mkGetParametersForImportResponse
--
--         , responseEnableKeyRotation $
--             mkEnableKeyRotationResponse
--
--         , responseDeleteCustomKeyStore $
--             mkDeleteCustomKeyStoreResponse
--
--         , responseCreateAlias $
--             mkCreateAliasResponse
--
--         , responseCreateGrant $
--             mkCreateGrantResponse
--
--         , responseListAliases $
--             mkListAliasesResponse
--
--         , responseConnectCustomKeyStore $
--             mkConnectCustomKeyStoreResponse
--
--         , responseListRetirableGrants $
--             mkListGrantsResponse
--
--         , responseGetPublicKey $
--             mkGetPublicKeyResponse
--
--         , responseGenerateRandom $
--             mkGenerateRandomResponse
--
--         , responseCreateKey $
--             mkCreateKeyResponse
--
--         , responseDisableKey $
--             mkDisableKeyResponse
--
--         , responseDisconnectCustomKeyStore $
--             mkDisconnectCustomKeyStoreResponse
--
--         , responseRetireGrant $
--             mkRetireGrantResponse
--
--         , responseListKeys $
--             mkListKeysResponse
--
--         , responseListResourceTags $
--             mkListResourceTagsResponse
--
--         , responseGetKeyRotationStatus $
--             mkGetKeyRotationStatusResponse
--
--         , responseGenerateDataKey $
--             mkGenerateDataKeyResponse
--
--         , responseDeleteAlias $
--             mkDeleteAliasResponse
--
--         , responseUpdateAlias $
--             mkUpdateAliasResponse
--
--         , responseDescribeKey $
--             mkDescribeKeyResponse
--
--         , responseDescribeCustomKeyStores $
--             mkDescribeCustomKeyStoresResponse
--
--         , responseCancelKeyDeletion $
--             mkCancelKeyDeletionResponse
--
--         , responseDecrypt $
--             mkDecryptResponse
--
--         , responseGenerateDataKeyPairWithoutPlaintext $
--             mkGenerateDataKeyPairWithoutPlaintextResponse
--
--         , responseUpdateKeyDescription $
--             mkUpdateKeyDescriptionResponse
--
--         , responseReEncrypt $
--             mkReEncryptResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListKeyPolicies $
--             mkListKeyPoliciesResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseSign $
--             mkSignResponse
--
--         , responseScheduleKeyDeletion $
--             mkScheduleKeyDeletionResponse
--
--         , responseGenerateDataKeyPair $
--             mkGenerateDataKeyPairResponse
--
--         , responsePutKeyPolicy $
--             mkPutKeyPolicyResponse
--
--         , responseEnableKey $
--             mkEnableKeyResponse
--
--         , responseRevokeGrant $
--             mkRevokeGrantResponse
--
--         , responseGetKeyPolicy $
--             mkGetKeyPolicyResponse
--
--         , responseImportKeyMaterial $
--             mkImportKeyMaterialResponse
--
--         , responseDeleteImportedKeyMaterial $
--             mkDeleteImportedKeyMaterialResponse
--
--           ]
--     ]

-- Requests

requestEncrypt :: Encrypt -> TestTree
requestEncrypt =
  req
    "Encrypt"
    "fixture/Encrypt.yaml"

requestCreateCustomKeyStore :: CreateCustomKeyStore -> TestTree
requestCreateCustomKeyStore =
  req
    "CreateCustomKeyStore"
    "fixture/CreateCustomKeyStore.yaml"

requestListGrants :: ListGrants -> TestTree
requestListGrants =
  req
    "ListGrants"
    "fixture/ListGrants.yaml"

requestDisableKeyRotation :: DisableKeyRotation -> TestTree
requestDisableKeyRotation =
  req
    "DisableKeyRotation"
    "fixture/DisableKeyRotation.yaml"

requestVerify :: Verify -> TestTree
requestVerify =
  req
    "Verify"
    "fixture/Verify.yaml"

requestGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintext -> TestTree
requestGenerateDataKeyWithoutPlaintext =
  req
    "GenerateDataKeyWithoutPlaintext"
    "fixture/GenerateDataKeyWithoutPlaintext.yaml"

requestUpdateCustomKeyStore :: UpdateCustomKeyStore -> TestTree
requestUpdateCustomKeyStore =
  req
    "UpdateCustomKeyStore"
    "fixture/UpdateCustomKeyStore.yaml"

requestGetParametersForImport :: GetParametersForImport -> TestTree
requestGetParametersForImport =
  req
    "GetParametersForImport"
    "fixture/GetParametersForImport.yaml"

requestEnableKeyRotation :: EnableKeyRotation -> TestTree
requestEnableKeyRotation =
  req
    "EnableKeyRotation"
    "fixture/EnableKeyRotation.yaml"

requestDeleteCustomKeyStore :: DeleteCustomKeyStore -> TestTree
requestDeleteCustomKeyStore =
  req
    "DeleteCustomKeyStore"
    "fixture/DeleteCustomKeyStore.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateGrant :: CreateGrant -> TestTree
requestCreateGrant =
  req
    "CreateGrant"
    "fixture/CreateGrant.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestConnectCustomKeyStore :: ConnectCustomKeyStore -> TestTree
requestConnectCustomKeyStore =
  req
    "ConnectCustomKeyStore"
    "fixture/ConnectCustomKeyStore.yaml"

requestListRetirableGrants :: ListRetirableGrants -> TestTree
requestListRetirableGrants =
  req
    "ListRetirableGrants"
    "fixture/ListRetirableGrants.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestGenerateRandom :: GenerateRandom -> TestTree
requestGenerateRandom =
  req
    "GenerateRandom"
    "fixture/GenerateRandom.yaml"

requestCreateKey :: CreateKey -> TestTree
requestCreateKey =
  req
    "CreateKey"
    "fixture/CreateKey.yaml"

requestDisableKey :: DisableKey -> TestTree
requestDisableKey =
  req
    "DisableKey"
    "fixture/DisableKey.yaml"

requestDisconnectCustomKeyStore :: DisconnectCustomKeyStore -> TestTree
requestDisconnectCustomKeyStore =
  req
    "DisconnectCustomKeyStore"
    "fixture/DisconnectCustomKeyStore.yaml"

requestRetireGrant :: RetireGrant -> TestTree
requestRetireGrant =
  req
    "RetireGrant"
    "fixture/RetireGrant.yaml"

requestListKeys :: ListKeys -> TestTree
requestListKeys =
  req
    "ListKeys"
    "fixture/ListKeys.yaml"

requestListResourceTags :: ListResourceTags -> TestTree
requestListResourceTags =
  req
    "ListResourceTags"
    "fixture/ListResourceTags.yaml"

requestGetKeyRotationStatus :: GetKeyRotationStatus -> TestTree
requestGetKeyRotationStatus =
  req
    "GetKeyRotationStatus"
    "fixture/GetKeyRotationStatus.yaml"

requestGenerateDataKey :: GenerateDataKey -> TestTree
requestGenerateDataKey =
  req
    "GenerateDataKey"
    "fixture/GenerateDataKey.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestDescribeKey :: DescribeKey -> TestTree
requestDescribeKey =
  req
    "DescribeKey"
    "fixture/DescribeKey.yaml"

requestDescribeCustomKeyStores :: DescribeCustomKeyStores -> TestTree
requestDescribeCustomKeyStores =
  req
    "DescribeCustomKeyStores"
    "fixture/DescribeCustomKeyStores.yaml"

requestCancelKeyDeletion :: CancelKeyDeletion -> TestTree
requestCancelKeyDeletion =
  req
    "CancelKeyDeletion"
    "fixture/CancelKeyDeletion.yaml"

requestDecrypt :: Decrypt -> TestTree
requestDecrypt =
  req
    "Decrypt"
    "fixture/Decrypt.yaml"

requestGenerateDataKeyPairWithoutPlaintext :: GenerateDataKeyPairWithoutPlaintext -> TestTree
requestGenerateDataKeyPairWithoutPlaintext =
  req
    "GenerateDataKeyPairWithoutPlaintext"
    "fixture/GenerateDataKeyPairWithoutPlaintext.yaml"

requestUpdateKeyDescription :: UpdateKeyDescription -> TestTree
requestUpdateKeyDescription =
  req
    "UpdateKeyDescription"
    "fixture/UpdateKeyDescription.yaml"

requestReEncrypt :: ReEncrypt -> TestTree
requestReEncrypt =
  req
    "ReEncrypt"
    "fixture/ReEncrypt.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListKeyPolicies :: ListKeyPolicies -> TestTree
requestListKeyPolicies =
  req
    "ListKeyPolicies"
    "fixture/ListKeyPolicies.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSign :: Sign -> TestTree
requestSign =
  req
    "Sign"
    "fixture/Sign.yaml"

requestScheduleKeyDeletion :: ScheduleKeyDeletion -> TestTree
requestScheduleKeyDeletion =
  req
    "ScheduleKeyDeletion"
    "fixture/ScheduleKeyDeletion.yaml"

requestGenerateDataKeyPair :: GenerateDataKeyPair -> TestTree
requestGenerateDataKeyPair =
  req
    "GenerateDataKeyPair"
    "fixture/GenerateDataKeyPair.yaml"

requestPutKeyPolicy :: PutKeyPolicy -> TestTree
requestPutKeyPolicy =
  req
    "PutKeyPolicy"
    "fixture/PutKeyPolicy.yaml"

requestEnableKey :: EnableKey -> TestTree
requestEnableKey =
  req
    "EnableKey"
    "fixture/EnableKey.yaml"

requestRevokeGrant :: RevokeGrant -> TestTree
requestRevokeGrant =
  req
    "RevokeGrant"
    "fixture/RevokeGrant.yaml"

requestGetKeyPolicy :: GetKeyPolicy -> TestTree
requestGetKeyPolicy =
  req
    "GetKeyPolicy"
    "fixture/GetKeyPolicy.yaml"

requestImportKeyMaterial :: ImportKeyMaterial -> TestTree
requestImportKeyMaterial =
  req
    "ImportKeyMaterial"
    "fixture/ImportKeyMaterial.yaml"

requestDeleteImportedKeyMaterial :: DeleteImportedKeyMaterial -> TestTree
requestDeleteImportedKeyMaterial =
  req
    "DeleteImportedKeyMaterial"
    "fixture/DeleteImportedKeyMaterial.yaml"

-- Responses

responseEncrypt :: EncryptResponse -> TestTree
responseEncrypt =
  res
    "EncryptResponse"
    "fixture/EncryptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Encrypt)

responseCreateCustomKeyStore :: CreateCustomKeyStoreResponse -> TestTree
responseCreateCustomKeyStore =
  res
    "CreateCustomKeyStoreResponse"
    "fixture/CreateCustomKeyStoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCustomKeyStore)

responseListGrants :: ListGrantsResponse -> TestTree
responseListGrants =
  res
    "ListGrantsResponse"
    "fixture/ListGrantsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGrants)

responseDisableKeyRotation :: DisableKeyRotationResponse -> TestTree
responseDisableKeyRotation =
  res
    "DisableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableKeyRotation)

responseVerify :: VerifyResponse -> TestTree
responseVerify =
  res
    "VerifyResponse"
    "fixture/VerifyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Verify)

responseGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyWithoutPlaintext =
  res
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateDataKeyWithoutPlaintext)

responseUpdateCustomKeyStore :: UpdateCustomKeyStoreResponse -> TestTree
responseUpdateCustomKeyStore =
  res
    "UpdateCustomKeyStoreResponse"
    "fixture/UpdateCustomKeyStoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateCustomKeyStore)

responseGetParametersForImport :: GetParametersForImportResponse -> TestTree
responseGetParametersForImport =
  res
    "GetParametersForImportResponse"
    "fixture/GetParametersForImportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetParametersForImport)

responseEnableKeyRotation :: EnableKeyRotationResponse -> TestTree
responseEnableKeyRotation =
  res
    "EnableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableKeyRotation)

responseDeleteCustomKeyStore :: DeleteCustomKeyStoreResponse -> TestTree
responseDeleteCustomKeyStore =
  res
    "DeleteCustomKeyStoreResponse"
    "fixture/DeleteCustomKeyStoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCustomKeyStore)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAlias)

responseCreateGrant :: CreateGrantResponse -> TestTree
responseCreateGrant =
  res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGrant)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAliases)

responseConnectCustomKeyStore :: ConnectCustomKeyStoreResponse -> TestTree
responseConnectCustomKeyStore =
  res
    "ConnectCustomKeyStoreResponse"
    "fixture/ConnectCustomKeyStoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConnectCustomKeyStore)

responseListRetirableGrants :: ListGrantsResponse -> TestTree
responseListRetirableGrants =
  res
    "ListRetirableGrantsResponse"
    "fixture/ListRetirableGrantsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRetirableGrants)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPublicKey)

responseGenerateRandom :: GenerateRandomResponse -> TestTree
responseGenerateRandom =
  res
    "GenerateRandomResponse"
    "fixture/GenerateRandomResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateRandom)

responseCreateKey :: CreateKeyResponse -> TestTree
responseCreateKey =
  res
    "CreateKeyResponse"
    "fixture/CreateKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateKey)

responseDisableKey :: DisableKeyResponse -> TestTree
responseDisableKey =
  res
    "DisableKeyResponse"
    "fixture/DisableKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableKey)

responseDisconnectCustomKeyStore :: DisconnectCustomKeyStoreResponse -> TestTree
responseDisconnectCustomKeyStore =
  res
    "DisconnectCustomKeyStoreResponse"
    "fixture/DisconnectCustomKeyStoreResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisconnectCustomKeyStore)

responseRetireGrant :: RetireGrantResponse -> TestTree
responseRetireGrant =
  res
    "RetireGrantResponse"
    "fixture/RetireGrantResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RetireGrant)

responseListKeys :: ListKeysResponse -> TestTree
responseListKeys =
  res
    "ListKeysResponse"
    "fixture/ListKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListKeys)

responseListResourceTags :: ListResourceTagsResponse -> TestTree
responseListResourceTags =
  res
    "ListResourceTagsResponse"
    "fixture/ListResourceTagsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResourceTags)

responseGetKeyRotationStatus :: GetKeyRotationStatusResponse -> TestTree
responseGetKeyRotationStatus =
  res
    "GetKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetKeyRotationStatus)

responseGenerateDataKey :: GenerateDataKeyResponse -> TestTree
responseGenerateDataKey =
  res
    "GenerateDataKeyResponse"
    "fixture/GenerateDataKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateDataKey)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateAlias)

responseDescribeKey :: DescribeKeyResponse -> TestTree
responseDescribeKey =
  res
    "DescribeKeyResponse"
    "fixture/DescribeKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeKey)

responseDescribeCustomKeyStores :: DescribeCustomKeyStoresResponse -> TestTree
responseDescribeCustomKeyStores =
  res
    "DescribeCustomKeyStoresResponse"
    "fixture/DescribeCustomKeyStoresResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCustomKeyStores)

responseCancelKeyDeletion :: CancelKeyDeletionResponse -> TestTree
responseCancelKeyDeletion =
  res
    "CancelKeyDeletionResponse"
    "fixture/CancelKeyDeletionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelKeyDeletion)

responseDecrypt :: DecryptResponse -> TestTree
responseDecrypt =
  res
    "DecryptResponse"
    "fixture/DecryptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Decrypt)

responseGenerateDataKeyPairWithoutPlaintext :: GenerateDataKeyPairWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyPairWithoutPlaintext =
  res
    "GenerateDataKeyPairWithoutPlaintextResponse"
    "fixture/GenerateDataKeyPairWithoutPlaintextResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateDataKeyPairWithoutPlaintext)

responseUpdateKeyDescription :: UpdateKeyDescriptionResponse -> TestTree
responseUpdateKeyDescription =
  res
    "UpdateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateKeyDescription)

responseReEncrypt :: ReEncryptResponse -> TestTree
responseReEncrypt =
  res
    "ReEncryptResponse"
    "fixture/ReEncryptResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReEncrypt)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseListKeyPolicies :: ListKeyPoliciesResponse -> TestTree
responseListKeyPolicies =
  res
    "ListKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListKeyPolicies)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseSign :: SignResponse -> TestTree
responseSign =
  res
    "SignResponse"
    "fixture/SignResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Sign)

responseScheduleKeyDeletion :: ScheduleKeyDeletionResponse -> TestTree
responseScheduleKeyDeletion =
  res
    "ScheduleKeyDeletionResponse"
    "fixture/ScheduleKeyDeletionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ScheduleKeyDeletion)

responseGenerateDataKeyPair :: GenerateDataKeyPairResponse -> TestTree
responseGenerateDataKeyPair =
  res
    "GenerateDataKeyPairResponse"
    "fixture/GenerateDataKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GenerateDataKeyPair)

responsePutKeyPolicy :: PutKeyPolicyResponse -> TestTree
responsePutKeyPolicy =
  res
    "PutKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutKeyPolicy)

responseEnableKey :: EnableKeyResponse -> TestTree
responseEnableKey =
  res
    "EnableKeyResponse"
    "fixture/EnableKeyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableKey)

responseRevokeGrant :: RevokeGrantResponse -> TestTree
responseRevokeGrant =
  res
    "RevokeGrantResponse"
    "fixture/RevokeGrantResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RevokeGrant)

responseGetKeyPolicy :: GetKeyPolicyResponse -> TestTree
responseGetKeyPolicy =
  res
    "GetKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetKeyPolicy)

responseImportKeyMaterial :: ImportKeyMaterialResponse -> TestTree
responseImportKeyMaterial =
  res
    "ImportKeyMaterialResponse"
    "fixture/ImportKeyMaterialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportKeyMaterial)

responseDeleteImportedKeyMaterial :: DeleteImportedKeyMaterialResponse -> TestTree
responseDeleteImportedKeyMaterial =
  res
    "DeleteImportedKeyMaterialResponse"
    "fixture/DeleteImportedKeyMaterialResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteImportedKeyMaterial)
