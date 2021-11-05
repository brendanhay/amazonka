{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KMS where

import Amazonka.KMS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KMS.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestEncrypt $
--             newEncrypt
--
--         , requestCreateCustomKeyStore $
--             newCreateCustomKeyStore
--
--         , requestListGrants $
--             newListGrants
--
--         , requestDisableKeyRotation $
--             newDisableKeyRotation
--
--         , requestVerify $
--             newVerify
--
--         , requestGenerateDataKeyWithoutPlaintext $
--             newGenerateDataKeyWithoutPlaintext
--
--         , requestUpdateCustomKeyStore $
--             newUpdateCustomKeyStore
--
--         , requestGetParametersForImport $
--             newGetParametersForImport
--
--         , requestEnableKeyRotation $
--             newEnableKeyRotation
--
--         , requestDeleteCustomKeyStore $
--             newDeleteCustomKeyStore
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateGrant $
--             newCreateGrant
--
--         , requestListAliases $
--             newListAliases
--
--         , requestUpdatePrimaryRegion $
--             newUpdatePrimaryRegion
--
--         , requestConnectCustomKeyStore $
--             newConnectCustomKeyStore
--
--         , requestListRetirableGrants $
--             newListRetirableGrants
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestGenerateRandom $
--             newGenerateRandom
--
--         , requestCreateKey $
--             newCreateKey
--
--         , requestDisableKey $
--             newDisableKey
--
--         , requestDisconnectCustomKeyStore $
--             newDisconnectCustomKeyStore
--
--         , requestRetireGrant $
--             newRetireGrant
--
--         , requestListKeys $
--             newListKeys
--
--         , requestListResourceTags $
--             newListResourceTags
--
--         , requestGetKeyRotationStatus $
--             newGetKeyRotationStatus
--
--         , requestGenerateDataKey $
--             newGenerateDataKey
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestDescribeKey $
--             newDescribeKey
--
--         , requestDescribeCustomKeyStores $
--             newDescribeCustomKeyStores
--
--         , requestCancelKeyDeletion $
--             newCancelKeyDeletion
--
--         , requestDecrypt $
--             newDecrypt
--
--         , requestGenerateDataKeyPairWithoutPlaintext $
--             newGenerateDataKeyPairWithoutPlaintext
--
--         , requestUpdateKeyDescription $
--             newUpdateKeyDescription
--
--         , requestReEncrypt $
--             newReEncrypt
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListKeyPolicies $
--             newListKeyPolicies
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSign $
--             newSign
--
--         , requestScheduleKeyDeletion $
--             newScheduleKeyDeletion
--
--         , requestGenerateDataKeyPair $
--             newGenerateDataKeyPair
--
--         , requestReplicateKey $
--             newReplicateKey
--
--         , requestPutKeyPolicy $
--             newPutKeyPolicy
--
--         , requestEnableKey $
--             newEnableKey
--
--         , requestRevokeGrant $
--             newRevokeGrant
--
--         , requestGetKeyPolicy $
--             newGetKeyPolicy
--
--         , requestImportKeyMaterial $
--             newImportKeyMaterial
--
--         , requestDeleteImportedKeyMaterial $
--             newDeleteImportedKeyMaterial
--
--           ]

--     , testGroup "response"
--         [ responseEncrypt $
--             newEncryptResponse
--
--         , responseCreateCustomKeyStore $
--             newCreateCustomKeyStoreResponse
--
--         , responseListGrants $
--             newListGrantsResponse
--
--         , responseDisableKeyRotation $
--             newDisableKeyRotationResponse
--
--         , responseVerify $
--             newVerifyResponse
--
--         , responseGenerateDataKeyWithoutPlaintext $
--             newGenerateDataKeyWithoutPlaintextResponse
--
--         , responseUpdateCustomKeyStore $
--             newUpdateCustomKeyStoreResponse
--
--         , responseGetParametersForImport $
--             newGetParametersForImportResponse
--
--         , responseEnableKeyRotation $
--             newEnableKeyRotationResponse
--
--         , responseDeleteCustomKeyStore $
--             newDeleteCustomKeyStoreResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateGrant $
--             newCreateGrantResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseUpdatePrimaryRegion $
--             newUpdatePrimaryRegionResponse
--
--         , responseConnectCustomKeyStore $
--             newConnectCustomKeyStoreResponse
--
--         , responseListRetirableGrants $
--             newListGrantsResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseGenerateRandom $
--             newGenerateRandomResponse
--
--         , responseCreateKey $
--             newCreateKeyResponse
--
--         , responseDisableKey $
--             newDisableKeyResponse
--
--         , responseDisconnectCustomKeyStore $
--             newDisconnectCustomKeyStoreResponse
--
--         , responseRetireGrant $
--             newRetireGrantResponse
--
--         , responseListKeys $
--             newListKeysResponse
--
--         , responseListResourceTags $
--             newListResourceTagsResponse
--
--         , responseGetKeyRotationStatus $
--             newGetKeyRotationStatusResponse
--
--         , responseGenerateDataKey $
--             newGenerateDataKeyResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseDescribeKey $
--             newDescribeKeyResponse
--
--         , responseDescribeCustomKeyStores $
--             newDescribeCustomKeyStoresResponse
--
--         , responseCancelKeyDeletion $
--             newCancelKeyDeletionResponse
--
--         , responseDecrypt $
--             newDecryptResponse
--
--         , responseGenerateDataKeyPairWithoutPlaintext $
--             newGenerateDataKeyPairWithoutPlaintextResponse
--
--         , responseUpdateKeyDescription $
--             newUpdateKeyDescriptionResponse
--
--         , responseReEncrypt $
--             newReEncryptResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListKeyPolicies $
--             newListKeyPoliciesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSign $
--             newSignResponse
--
--         , responseScheduleKeyDeletion $
--             newScheduleKeyDeletionResponse
--
--         , responseGenerateDataKeyPair $
--             newGenerateDataKeyPairResponse
--
--         , responseReplicateKey $
--             newReplicateKeyResponse
--
--         , responsePutKeyPolicy $
--             newPutKeyPolicyResponse
--
--         , responseEnableKey $
--             newEnableKeyResponse
--
--         , responseRevokeGrant $
--             newRevokeGrantResponse
--
--         , responseGetKeyPolicy $
--             newGetKeyPolicyResponse
--
--         , responseImportKeyMaterial $
--             newImportKeyMaterialResponse
--
--         , responseDeleteImportedKeyMaterial $
--             newDeleteImportedKeyMaterialResponse
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

requestUpdatePrimaryRegion :: UpdatePrimaryRegion -> TestTree
requestUpdatePrimaryRegion =
  req
    "UpdatePrimaryRegion"
    "fixture/UpdatePrimaryRegion.yaml"

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

requestReplicateKey :: ReplicateKey -> TestTree
requestReplicateKey =
  req
    "ReplicateKey"
    "fixture/ReplicateKey.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Encrypt)

responseCreateCustomKeyStore :: CreateCustomKeyStoreResponse -> TestTree
responseCreateCustomKeyStore =
  res
    "CreateCustomKeyStoreResponse"
    "fixture/CreateCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomKeyStore)

responseListGrants :: ListGrantsResponse -> TestTree
responseListGrants =
  res
    "ListGrantsResponse"
    "fixture/ListGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGrants)

responseDisableKeyRotation :: DisableKeyRotationResponse -> TestTree
responseDisableKeyRotation =
  res
    "DisableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKeyRotation)

responseVerify :: VerifyResponse -> TestTree
responseVerify =
  res
    "VerifyResponse"
    "fixture/VerifyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Verify)

responseGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyWithoutPlaintext =
  res
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyWithoutPlaintext)

responseUpdateCustomKeyStore :: UpdateCustomKeyStoreResponse -> TestTree
responseUpdateCustomKeyStore =
  res
    "UpdateCustomKeyStoreResponse"
    "fixture/UpdateCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomKeyStore)

responseGetParametersForImport :: GetParametersForImportResponse -> TestTree
responseGetParametersForImport =
  res
    "GetParametersForImportResponse"
    "fixture/GetParametersForImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersForImport)

responseEnableKeyRotation :: EnableKeyRotationResponse -> TestTree
responseEnableKeyRotation =
  res
    "EnableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKeyRotation)

responseDeleteCustomKeyStore :: DeleteCustomKeyStoreResponse -> TestTree
responseDeleteCustomKeyStore =
  res
    "DeleteCustomKeyStoreResponse"
    "fixture/DeleteCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomKeyStore)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateGrant :: CreateGrantResponse -> TestTree
responseCreateGrant =
  res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrant)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseUpdatePrimaryRegion :: UpdatePrimaryRegionResponse -> TestTree
responseUpdatePrimaryRegion =
  res
    "UpdatePrimaryRegionResponse"
    "fixture/UpdatePrimaryRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrimaryRegion)

responseConnectCustomKeyStore :: ConnectCustomKeyStoreResponse -> TestTree
responseConnectCustomKeyStore =
  res
    "ConnectCustomKeyStoreResponse"
    "fixture/ConnectCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConnectCustomKeyStore)

responseListRetirableGrants :: ListGrantsResponse -> TestTree
responseListRetirableGrants =
  res
    "ListRetirableGrantsResponse"
    "fixture/ListRetirableGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRetirableGrants)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKey)

responseGenerateRandom :: GenerateRandomResponse -> TestTree
responseGenerateRandom =
  res
    "GenerateRandomResponse"
    "fixture/GenerateRandomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateRandom)

responseCreateKey :: CreateKeyResponse -> TestTree
responseCreateKey =
  res
    "CreateKeyResponse"
    "fixture/CreateKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKey)

responseDisableKey :: DisableKeyResponse -> TestTree
responseDisableKey =
  res
    "DisableKeyResponse"
    "fixture/DisableKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKey)

responseDisconnectCustomKeyStore :: DisconnectCustomKeyStoreResponse -> TestTree
responseDisconnectCustomKeyStore =
  res
    "DisconnectCustomKeyStoreResponse"
    "fixture/DisconnectCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectCustomKeyStore)

responseRetireGrant :: RetireGrantResponse -> TestTree
responseRetireGrant =
  res
    "RetireGrantResponse"
    "fixture/RetireGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetireGrant)

responseListKeys :: ListKeysResponse -> TestTree
responseListKeys =
  res
    "ListKeysResponse"
    "fixture/ListKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeys)

responseListResourceTags :: ListResourceTagsResponse -> TestTree
responseListResourceTags =
  res
    "ListResourceTagsResponse"
    "fixture/ListResourceTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceTags)

responseGetKeyRotationStatus :: GetKeyRotationStatusResponse -> TestTree
responseGetKeyRotationStatus =
  res
    "GetKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyRotationStatus)

responseGenerateDataKey :: GenerateDataKeyResponse -> TestTree
responseGenerateDataKey =
  res
    "GenerateDataKeyResponse"
    "fixture/GenerateDataKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKey)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseDescribeKey :: DescribeKeyResponse -> TestTree
responseDescribeKey =
  res
    "DescribeKeyResponse"
    "fixture/DescribeKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKey)

responseDescribeCustomKeyStores :: DescribeCustomKeyStoresResponse -> TestTree
responseDescribeCustomKeyStores =
  res
    "DescribeCustomKeyStoresResponse"
    "fixture/DescribeCustomKeyStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomKeyStores)

responseCancelKeyDeletion :: CancelKeyDeletionResponse -> TestTree
responseCancelKeyDeletion =
  res
    "CancelKeyDeletionResponse"
    "fixture/CancelKeyDeletionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelKeyDeletion)

responseDecrypt :: DecryptResponse -> TestTree
responseDecrypt =
  res
    "DecryptResponse"
    "fixture/DecryptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Decrypt)

responseGenerateDataKeyPairWithoutPlaintext :: GenerateDataKeyPairWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyPairWithoutPlaintext =
  res
    "GenerateDataKeyPairWithoutPlaintextResponse"
    "fixture/GenerateDataKeyPairWithoutPlaintextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyPairWithoutPlaintext)

responseUpdateKeyDescription :: UpdateKeyDescriptionResponse -> TestTree
responseUpdateKeyDescription =
  res
    "UpdateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKeyDescription)

responseReEncrypt :: ReEncryptResponse -> TestTree
responseReEncrypt =
  res
    "ReEncryptResponse"
    "fixture/ReEncryptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReEncrypt)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListKeyPolicies :: ListKeyPoliciesResponse -> TestTree
responseListKeyPolicies =
  res
    "ListKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyPolicies)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseSign :: SignResponse -> TestTree
responseSign =
  res
    "SignResponse"
    "fixture/SignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Sign)

responseScheduleKeyDeletion :: ScheduleKeyDeletionResponse -> TestTree
responseScheduleKeyDeletion =
  res
    "ScheduleKeyDeletionResponse"
    "fixture/ScheduleKeyDeletionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScheduleKeyDeletion)

responseGenerateDataKeyPair :: GenerateDataKeyPairResponse -> TestTree
responseGenerateDataKeyPair =
  res
    "GenerateDataKeyPairResponse"
    "fixture/GenerateDataKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyPair)

responseReplicateKey :: ReplicateKeyResponse -> TestTree
responseReplicateKey =
  res
    "ReplicateKeyResponse"
    "fixture/ReplicateKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplicateKey)

responsePutKeyPolicy :: PutKeyPolicyResponse -> TestTree
responsePutKeyPolicy =
  res
    "PutKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutKeyPolicy)

responseEnableKey :: EnableKeyResponse -> TestTree
responseEnableKey =
  res
    "EnableKeyResponse"
    "fixture/EnableKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKey)

responseRevokeGrant :: RevokeGrantResponse -> TestTree
responseRevokeGrant =
  res
    "RevokeGrantResponse"
    "fixture/RevokeGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeGrant)

responseGetKeyPolicy :: GetKeyPolicyResponse -> TestTree
responseGetKeyPolicy =
  res
    "GetKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPolicy)

responseImportKeyMaterial :: ImportKeyMaterialResponse -> TestTree
responseImportKeyMaterial =
  res
    "ImportKeyMaterialResponse"
    "fixture/ImportKeyMaterialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyMaterial)

responseDeleteImportedKeyMaterial :: DeleteImportedKeyMaterialResponse -> TestTree
responseDeleteImportedKeyMaterial =
  res
    "DeleteImportedKeyMaterialResponse"
    "fixture/DeleteImportedKeyMaterialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImportedKeyMaterial)
