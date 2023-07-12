{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KMS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCancelKeyDeletion $
--             newCancelKeyDeletion
--
--         , requestConnectCustomKeyStore $
--             newConnectCustomKeyStore
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateCustomKeyStore $
--             newCreateCustomKeyStore
--
--         , requestCreateGrant $
--             newCreateGrant
--
--         , requestCreateKey $
--             newCreateKey
--
--         , requestDecrypt $
--             newDecrypt
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDeleteCustomKeyStore $
--             newDeleteCustomKeyStore
--
--         , requestDeleteImportedKeyMaterial $
--             newDeleteImportedKeyMaterial
--
--         , requestDescribeCustomKeyStores $
--             newDescribeCustomKeyStores
--
--         , requestDescribeKey $
--             newDescribeKey
--
--         , requestDisableKey $
--             newDisableKey
--
--         , requestDisableKeyRotation $
--             newDisableKeyRotation
--
--         , requestDisconnectCustomKeyStore $
--             newDisconnectCustomKeyStore
--
--         , requestEnableKey $
--             newEnableKey
--
--         , requestEnableKeyRotation $
--             newEnableKeyRotation
--
--         , requestEncrypt $
--             newEncrypt
--
--         , requestGenerateDataKey $
--             newGenerateDataKey
--
--         , requestGenerateDataKeyPair $
--             newGenerateDataKeyPair
--
--         , requestGenerateDataKeyPairWithoutPlaintext $
--             newGenerateDataKeyPairWithoutPlaintext
--
--         , requestGenerateDataKeyWithoutPlaintext $
--             newGenerateDataKeyWithoutPlaintext
--
--         , requestGenerateMac $
--             newGenerateMac
--
--         , requestGenerateRandom $
--             newGenerateRandom
--
--         , requestGetKeyPolicy $
--             newGetKeyPolicy
--
--         , requestGetKeyRotationStatus $
--             newGetKeyRotationStatus
--
--         , requestGetParametersForImport $
--             newGetParametersForImport
--
--         , requestGetPublicKey $
--             newGetPublicKey
--
--         , requestImportKeyMaterial $
--             newImportKeyMaterial
--
--         , requestListAliases $
--             newListAliases
--
--         , requestListGrants $
--             newListGrants
--
--         , requestListKeyPolicies $
--             newListKeyPolicies
--
--         , requestListKeys $
--             newListKeys
--
--         , requestListResourceTags $
--             newListResourceTags
--
--         , requestListRetirableGrants $
--             newListRetirableGrants
--
--         , requestPutKeyPolicy $
--             newPutKeyPolicy
--
--         , requestReEncrypt $
--             newReEncrypt
--
--         , requestReplicateKey $
--             newReplicateKey
--
--         , requestRetireGrant $
--             newRetireGrant
--
--         , requestRevokeGrant $
--             newRevokeGrant
--
--         , requestScheduleKeyDeletion $
--             newScheduleKeyDeletion
--
--         , requestSign $
--             newSign
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAlias $
--             newUpdateAlias
--
--         , requestUpdateCustomKeyStore $
--             newUpdateCustomKeyStore
--
--         , requestUpdateKeyDescription $
--             newUpdateKeyDescription
--
--         , requestUpdatePrimaryRegion $
--             newUpdatePrimaryRegion
--
--         , requestVerify $
--             newVerify
--
--         , requestVerifyMac $
--             newVerifyMac
--
--           ]

--     , testGroup "response"
--         [ responseCancelKeyDeletion $
--             newCancelKeyDeletionResponse
--
--         , responseConnectCustomKeyStore $
--             newConnectCustomKeyStoreResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateCustomKeyStore $
--             newCreateCustomKeyStoreResponse
--
--         , responseCreateGrant $
--             newCreateGrantResponse
--
--         , responseCreateKey $
--             newCreateKeyResponse
--
--         , responseDecrypt $
--             newDecryptResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDeleteCustomKeyStore $
--             newDeleteCustomKeyStoreResponse
--
--         , responseDeleteImportedKeyMaterial $
--             newDeleteImportedKeyMaterialResponse
--
--         , responseDescribeCustomKeyStores $
--             newDescribeCustomKeyStoresResponse
--
--         , responseDescribeKey $
--             newDescribeKeyResponse
--
--         , responseDisableKey $
--             newDisableKeyResponse
--
--         , responseDisableKeyRotation $
--             newDisableKeyRotationResponse
--
--         , responseDisconnectCustomKeyStore $
--             newDisconnectCustomKeyStoreResponse
--
--         , responseEnableKey $
--             newEnableKeyResponse
--
--         , responseEnableKeyRotation $
--             newEnableKeyRotationResponse
--
--         , responseEncrypt $
--             newEncryptResponse
--
--         , responseGenerateDataKey $
--             newGenerateDataKeyResponse
--
--         , responseGenerateDataKeyPair $
--             newGenerateDataKeyPairResponse
--
--         , responseGenerateDataKeyPairWithoutPlaintext $
--             newGenerateDataKeyPairWithoutPlaintextResponse
--
--         , responseGenerateDataKeyWithoutPlaintext $
--             newGenerateDataKeyWithoutPlaintextResponse
--
--         , responseGenerateMac $
--             newGenerateMacResponse
--
--         , responseGenerateRandom $
--             newGenerateRandomResponse
--
--         , responseGetKeyPolicy $
--             newGetKeyPolicyResponse
--
--         , responseGetKeyRotationStatus $
--             newGetKeyRotationStatusResponse
--
--         , responseGetParametersForImport $
--             newGetParametersForImportResponse
--
--         , responseGetPublicKey $
--             newGetPublicKeyResponse
--
--         , responseImportKeyMaterial $
--             newImportKeyMaterialResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseListGrants $
--             newListGrantsResponse
--
--         , responseListKeyPolicies $
--             newListKeyPoliciesResponse
--
--         , responseListKeys $
--             newListKeysResponse
--
--         , responseListResourceTags $
--             newListResourceTagsResponse
--
--         , responseListRetirableGrants $
--             newListGrantsResponse
--
--         , responsePutKeyPolicy $
--             newPutKeyPolicyResponse
--
--         , responseReEncrypt $
--             newReEncryptResponse
--
--         , responseReplicateKey $
--             newReplicateKeyResponse
--
--         , responseRetireGrant $
--             newRetireGrantResponse
--
--         , responseRevokeGrant $
--             newRevokeGrantResponse
--
--         , responseScheduleKeyDeletion $
--             newScheduleKeyDeletionResponse
--
--         , responseSign $
--             newSignResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAlias $
--             newUpdateAliasResponse
--
--         , responseUpdateCustomKeyStore $
--             newUpdateCustomKeyStoreResponse
--
--         , responseUpdateKeyDescription $
--             newUpdateKeyDescriptionResponse
--
--         , responseUpdatePrimaryRegion $
--             newUpdatePrimaryRegionResponse
--
--         , responseVerify $
--             newVerifyResponse
--
--         , responseVerifyMac $
--             newVerifyMacResponse
--
--           ]
--     ]

-- Requests

requestCancelKeyDeletion :: CancelKeyDeletion -> TestTree
requestCancelKeyDeletion =
  req
    "CancelKeyDeletion"
    "fixture/CancelKeyDeletion.yaml"

requestConnectCustomKeyStore :: ConnectCustomKeyStore -> TestTree
requestConnectCustomKeyStore =
  req
    "ConnectCustomKeyStore"
    "fixture/ConnectCustomKeyStore.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateCustomKeyStore :: CreateCustomKeyStore -> TestTree
requestCreateCustomKeyStore =
  req
    "CreateCustomKeyStore"
    "fixture/CreateCustomKeyStore.yaml"

requestCreateGrant :: CreateGrant -> TestTree
requestCreateGrant =
  req
    "CreateGrant"
    "fixture/CreateGrant.yaml"

requestCreateKey :: CreateKey -> TestTree
requestCreateKey =
  req
    "CreateKey"
    "fixture/CreateKey.yaml"

requestDecrypt :: Decrypt -> TestTree
requestDecrypt =
  req
    "Decrypt"
    "fixture/Decrypt.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDeleteCustomKeyStore :: DeleteCustomKeyStore -> TestTree
requestDeleteCustomKeyStore =
  req
    "DeleteCustomKeyStore"
    "fixture/DeleteCustomKeyStore.yaml"

requestDeleteImportedKeyMaterial :: DeleteImportedKeyMaterial -> TestTree
requestDeleteImportedKeyMaterial =
  req
    "DeleteImportedKeyMaterial"
    "fixture/DeleteImportedKeyMaterial.yaml"

requestDescribeCustomKeyStores :: DescribeCustomKeyStores -> TestTree
requestDescribeCustomKeyStores =
  req
    "DescribeCustomKeyStores"
    "fixture/DescribeCustomKeyStores.yaml"

requestDescribeKey :: DescribeKey -> TestTree
requestDescribeKey =
  req
    "DescribeKey"
    "fixture/DescribeKey.yaml"

requestDisableKey :: DisableKey -> TestTree
requestDisableKey =
  req
    "DisableKey"
    "fixture/DisableKey.yaml"

requestDisableKeyRotation :: DisableKeyRotation -> TestTree
requestDisableKeyRotation =
  req
    "DisableKeyRotation"
    "fixture/DisableKeyRotation.yaml"

requestDisconnectCustomKeyStore :: DisconnectCustomKeyStore -> TestTree
requestDisconnectCustomKeyStore =
  req
    "DisconnectCustomKeyStore"
    "fixture/DisconnectCustomKeyStore.yaml"

requestEnableKey :: EnableKey -> TestTree
requestEnableKey =
  req
    "EnableKey"
    "fixture/EnableKey.yaml"

requestEnableKeyRotation :: EnableKeyRotation -> TestTree
requestEnableKeyRotation =
  req
    "EnableKeyRotation"
    "fixture/EnableKeyRotation.yaml"

requestEncrypt :: Encrypt -> TestTree
requestEncrypt =
  req
    "Encrypt"
    "fixture/Encrypt.yaml"

requestGenerateDataKey :: GenerateDataKey -> TestTree
requestGenerateDataKey =
  req
    "GenerateDataKey"
    "fixture/GenerateDataKey.yaml"

requestGenerateDataKeyPair :: GenerateDataKeyPair -> TestTree
requestGenerateDataKeyPair =
  req
    "GenerateDataKeyPair"
    "fixture/GenerateDataKeyPair.yaml"

requestGenerateDataKeyPairWithoutPlaintext :: GenerateDataKeyPairWithoutPlaintext -> TestTree
requestGenerateDataKeyPairWithoutPlaintext =
  req
    "GenerateDataKeyPairWithoutPlaintext"
    "fixture/GenerateDataKeyPairWithoutPlaintext.yaml"

requestGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintext -> TestTree
requestGenerateDataKeyWithoutPlaintext =
  req
    "GenerateDataKeyWithoutPlaintext"
    "fixture/GenerateDataKeyWithoutPlaintext.yaml"

requestGenerateMac :: GenerateMac -> TestTree
requestGenerateMac =
  req
    "GenerateMac"
    "fixture/GenerateMac.yaml"

requestGenerateRandom :: GenerateRandom -> TestTree
requestGenerateRandom =
  req
    "GenerateRandom"
    "fixture/GenerateRandom.yaml"

requestGetKeyPolicy :: GetKeyPolicy -> TestTree
requestGetKeyPolicy =
  req
    "GetKeyPolicy"
    "fixture/GetKeyPolicy.yaml"

requestGetKeyRotationStatus :: GetKeyRotationStatus -> TestTree
requestGetKeyRotationStatus =
  req
    "GetKeyRotationStatus"
    "fixture/GetKeyRotationStatus.yaml"

requestGetParametersForImport :: GetParametersForImport -> TestTree
requestGetParametersForImport =
  req
    "GetParametersForImport"
    "fixture/GetParametersForImport.yaml"

requestGetPublicKey :: GetPublicKey -> TestTree
requestGetPublicKey =
  req
    "GetPublicKey"
    "fixture/GetPublicKey.yaml"

requestImportKeyMaterial :: ImportKeyMaterial -> TestTree
requestImportKeyMaterial =
  req
    "ImportKeyMaterial"
    "fixture/ImportKeyMaterial.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestListGrants :: ListGrants -> TestTree
requestListGrants =
  req
    "ListGrants"
    "fixture/ListGrants.yaml"

requestListKeyPolicies :: ListKeyPolicies -> TestTree
requestListKeyPolicies =
  req
    "ListKeyPolicies"
    "fixture/ListKeyPolicies.yaml"

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

requestListRetirableGrants :: ListRetirableGrants -> TestTree
requestListRetirableGrants =
  req
    "ListRetirableGrants"
    "fixture/ListRetirableGrants.yaml"

requestPutKeyPolicy :: PutKeyPolicy -> TestTree
requestPutKeyPolicy =
  req
    "PutKeyPolicy"
    "fixture/PutKeyPolicy.yaml"

requestReEncrypt :: ReEncrypt -> TestTree
requestReEncrypt =
  req
    "ReEncrypt"
    "fixture/ReEncrypt.yaml"

requestReplicateKey :: ReplicateKey -> TestTree
requestReplicateKey =
  req
    "ReplicateKey"
    "fixture/ReplicateKey.yaml"

requestRetireGrant :: RetireGrant -> TestTree
requestRetireGrant =
  req
    "RetireGrant"
    "fixture/RetireGrant.yaml"

requestRevokeGrant :: RevokeGrant -> TestTree
requestRevokeGrant =
  req
    "RevokeGrant"
    "fixture/RevokeGrant.yaml"

requestScheduleKeyDeletion :: ScheduleKeyDeletion -> TestTree
requestScheduleKeyDeletion =
  req
    "ScheduleKeyDeletion"
    "fixture/ScheduleKeyDeletion.yaml"

requestSign :: Sign -> TestTree
requestSign =
  req
    "Sign"
    "fixture/Sign.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAlias :: UpdateAlias -> TestTree
requestUpdateAlias =
  req
    "UpdateAlias"
    "fixture/UpdateAlias.yaml"

requestUpdateCustomKeyStore :: UpdateCustomKeyStore -> TestTree
requestUpdateCustomKeyStore =
  req
    "UpdateCustomKeyStore"
    "fixture/UpdateCustomKeyStore.yaml"

requestUpdateKeyDescription :: UpdateKeyDescription -> TestTree
requestUpdateKeyDescription =
  req
    "UpdateKeyDescription"
    "fixture/UpdateKeyDescription.yaml"

requestUpdatePrimaryRegion :: UpdatePrimaryRegion -> TestTree
requestUpdatePrimaryRegion =
  req
    "UpdatePrimaryRegion"
    "fixture/UpdatePrimaryRegion.yaml"

requestVerify :: Verify -> TestTree
requestVerify =
  req
    "Verify"
    "fixture/Verify.yaml"

requestVerifyMac :: VerifyMac -> TestTree
requestVerifyMac =
  req
    "VerifyMac"
    "fixture/VerifyMac.yaml"

-- Responses

responseCancelKeyDeletion :: CancelKeyDeletionResponse -> TestTree
responseCancelKeyDeletion =
  res
    "CancelKeyDeletionResponse"
    "fixture/CancelKeyDeletionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelKeyDeletion)

responseConnectCustomKeyStore :: ConnectCustomKeyStoreResponse -> TestTree
responseConnectCustomKeyStore =
  res
    "ConnectCustomKeyStoreResponse"
    "fixture/ConnectCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConnectCustomKeyStore)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateCustomKeyStore :: CreateCustomKeyStoreResponse -> TestTree
responseCreateCustomKeyStore =
  res
    "CreateCustomKeyStoreResponse"
    "fixture/CreateCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomKeyStore)

responseCreateGrant :: CreateGrantResponse -> TestTree
responseCreateGrant =
  res
    "CreateGrantResponse"
    "fixture/CreateGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGrant)

responseCreateKey :: CreateKeyResponse -> TestTree
responseCreateKey =
  res
    "CreateKeyResponse"
    "fixture/CreateKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKey)

responseDecrypt :: DecryptResponse -> TestTree
responseDecrypt =
  res
    "DecryptResponse"
    "fixture/DecryptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Decrypt)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseDeleteCustomKeyStore :: DeleteCustomKeyStoreResponse -> TestTree
responseDeleteCustomKeyStore =
  res
    "DeleteCustomKeyStoreResponse"
    "fixture/DeleteCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomKeyStore)

responseDeleteImportedKeyMaterial :: DeleteImportedKeyMaterialResponse -> TestTree
responseDeleteImportedKeyMaterial =
  res
    "DeleteImportedKeyMaterialResponse"
    "fixture/DeleteImportedKeyMaterialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImportedKeyMaterial)

responseDescribeCustomKeyStores :: DescribeCustomKeyStoresResponse -> TestTree
responseDescribeCustomKeyStores =
  res
    "DescribeCustomKeyStoresResponse"
    "fixture/DescribeCustomKeyStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomKeyStores)

responseDescribeKey :: DescribeKeyResponse -> TestTree
responseDescribeKey =
  res
    "DescribeKeyResponse"
    "fixture/DescribeKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKey)

responseDisableKey :: DisableKeyResponse -> TestTree
responseDisableKey =
  res
    "DisableKeyResponse"
    "fixture/DisableKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKey)

responseDisableKeyRotation :: DisableKeyRotationResponse -> TestTree
responseDisableKeyRotation =
  res
    "DisableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableKeyRotation)

responseDisconnectCustomKeyStore :: DisconnectCustomKeyStoreResponse -> TestTree
responseDisconnectCustomKeyStore =
  res
    "DisconnectCustomKeyStoreResponse"
    "fixture/DisconnectCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectCustomKeyStore)

responseEnableKey :: EnableKeyResponse -> TestTree
responseEnableKey =
  res
    "EnableKeyResponse"
    "fixture/EnableKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKey)

responseEnableKeyRotation :: EnableKeyRotationResponse -> TestTree
responseEnableKeyRotation =
  res
    "EnableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableKeyRotation)

responseEncrypt :: EncryptResponse -> TestTree
responseEncrypt =
  res
    "EncryptResponse"
    "fixture/EncryptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Encrypt)

responseGenerateDataKey :: GenerateDataKeyResponse -> TestTree
responseGenerateDataKey =
  res
    "GenerateDataKeyResponse"
    "fixture/GenerateDataKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKey)

responseGenerateDataKeyPair :: GenerateDataKeyPairResponse -> TestTree
responseGenerateDataKeyPair =
  res
    "GenerateDataKeyPairResponse"
    "fixture/GenerateDataKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyPair)

responseGenerateDataKeyPairWithoutPlaintext :: GenerateDataKeyPairWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyPairWithoutPlaintext =
  res
    "GenerateDataKeyPairWithoutPlaintextResponse"
    "fixture/GenerateDataKeyPairWithoutPlaintextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyPairWithoutPlaintext)

responseGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
responseGenerateDataKeyWithoutPlaintext =
  res
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateDataKeyWithoutPlaintext)

responseGenerateMac :: GenerateMacResponse -> TestTree
responseGenerateMac =
  res
    "GenerateMacResponse"
    "fixture/GenerateMacResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateMac)

responseGenerateRandom :: GenerateRandomResponse -> TestTree
responseGenerateRandom =
  res
    "GenerateRandomResponse"
    "fixture/GenerateRandomResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateRandom)

responseGetKeyPolicy :: GetKeyPolicyResponse -> TestTree
responseGetKeyPolicy =
  res
    "GetKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPolicy)

responseGetKeyRotationStatus :: GetKeyRotationStatusResponse -> TestTree
responseGetKeyRotationStatus =
  res
    "GetKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyRotationStatus)

responseGetParametersForImport :: GetParametersForImportResponse -> TestTree
responseGetParametersForImport =
  res
    "GetParametersForImportResponse"
    "fixture/GetParametersForImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetParametersForImport)

responseGetPublicKey :: GetPublicKeyResponse -> TestTree
responseGetPublicKey =
  res
    "GetPublicKeyResponse"
    "fixture/GetPublicKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPublicKey)

responseImportKeyMaterial :: ImportKeyMaterialResponse -> TestTree
responseImportKeyMaterial =
  res
    "ImportKeyMaterialResponse"
    "fixture/ImportKeyMaterialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyMaterial)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseListGrants :: ListGrantsResponse -> TestTree
responseListGrants =
  res
    "ListGrantsResponse"
    "fixture/ListGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGrants)

responseListKeyPolicies :: ListKeyPoliciesResponse -> TestTree
responseListKeyPolicies =
  res
    "ListKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKeyPolicies)

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

responseListRetirableGrants :: ListGrantsResponse -> TestTree
responseListRetirableGrants =
  res
    "ListRetirableGrantsResponse"
    "fixture/ListRetirableGrantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRetirableGrants)

responsePutKeyPolicy :: PutKeyPolicyResponse -> TestTree
responsePutKeyPolicy =
  res
    "PutKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutKeyPolicy)

responseReEncrypt :: ReEncryptResponse -> TestTree
responseReEncrypt =
  res
    "ReEncryptResponse"
    "fixture/ReEncryptResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReEncrypt)

responseReplicateKey :: ReplicateKeyResponse -> TestTree
responseReplicateKey =
  res
    "ReplicateKeyResponse"
    "fixture/ReplicateKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplicateKey)

responseRetireGrant :: RetireGrantResponse -> TestTree
responseRetireGrant =
  res
    "RetireGrantResponse"
    "fixture/RetireGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetireGrant)

responseRevokeGrant :: RevokeGrantResponse -> TestTree
responseRevokeGrant =
  res
    "RevokeGrantResponse"
    "fixture/RevokeGrantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeGrant)

responseScheduleKeyDeletion :: ScheduleKeyDeletionResponse -> TestTree
responseScheduleKeyDeletion =
  res
    "ScheduleKeyDeletionResponse"
    "fixture/ScheduleKeyDeletionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ScheduleKeyDeletion)

responseSign :: SignResponse -> TestTree
responseSign =
  res
    "SignResponse"
    "fixture/SignResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Sign)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAlias :: UpdateAliasResponse -> TestTree
responseUpdateAlias =
  res
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAlias)

responseUpdateCustomKeyStore :: UpdateCustomKeyStoreResponse -> TestTree
responseUpdateCustomKeyStore =
  res
    "UpdateCustomKeyStoreResponse"
    "fixture/UpdateCustomKeyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomKeyStore)

responseUpdateKeyDescription :: UpdateKeyDescriptionResponse -> TestTree
responseUpdateKeyDescription =
  res
    "UpdateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKeyDescription)

responseUpdatePrimaryRegion :: UpdatePrimaryRegionResponse -> TestTree
responseUpdatePrimaryRegion =
  res
    "UpdatePrimaryRegionResponse"
    "fixture/UpdatePrimaryRegionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrimaryRegion)

responseVerify :: VerifyResponse -> TestTree
responseVerify =
  res
    "VerifyResponse"
    "fixture/VerifyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Verify)

responseVerifyMac :: VerifyMacResponse -> TestTree
responseVerifyMac =
  res
    "VerifyMacResponse"
    "fixture/VerifyMacResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifyMac)
