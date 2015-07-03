-- Module      : Test.AWS.Gen.KMS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.KMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.KMS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDisableKeyRotation $
--             disableKeyRotation
--
--         , testGenerateDataKeyWithoutPlaintext $
--             generateDataKeyWithoutPlaintext
--
--         , testListGrants $
--             listGrants
--
--         , testEncrypt $
--             encrypt
--
--         , testEnableKeyRotation $
--             enableKeyRotation
--
--         , testCreateGrant $
--             createGrant
--
--         , testCreateAlias $
--             createAlias
--
--         , testListAliases $
--             listAliases
--
--         , testGenerateRandom $
--             generateRandom
--
--         , testDisableKey $
--             disableKey
--
--         , testCreateKey $
--             createKey
--
--         , testRetireGrant $
--             retireGrant
--
--         , testListKeys $
--             listKeys
--
--         , testGetKeyRotationStatus $
--             getKeyRotationStatus
--
--         , testGenerateDataKey $
--             generateDataKey
--
--         , testDeleteAlias $
--             deleteAlias
--
--         , testUpdateAlias $
--             updateAlias
--
--         , testDescribeKey $
--             describeKey
--
--         , testDecrypt $
--             decrypt
--
--         , testUpdateKeyDescription $
--             updateKeyDescription
--
--         , testReEncrypt $
--             reEncrypt
--
--         , testListKeyPolicies $
--             listKeyPolicies
--
--         , testEnableKey $
--             enableKey
--
--         , testPutKeyPolicy $
--             putKeyPolicy
--
--         , testRevokeGrant $
--             revokeGrant
--
--         , testGetKeyPolicy $
--             getKeyPolicy
--
--           ]

--     , testGroup "response"
--         [ testDisableKeyRotationResponse $
--             disableKeyRotationResponse
--
--         , testGenerateDataKeyWithoutPlaintextResponse $
--             generateDataKeyWithoutPlaintextResponse
--
--         , testListGrantsResponse $
--             listGrantsResponse
--
--         , testEncryptResponse $
--             encryptResponse
--
--         , testEnableKeyRotationResponse $
--             enableKeyRotationResponse
--
--         , testCreateGrantResponse $
--             createGrantResponse
--
--         , testCreateAliasResponse $
--             createAliasResponse
--
--         , testListAliasesResponse $
--             listAliasesResponse
--
--         , testGenerateRandomResponse $
--             generateRandomResponse
--
--         , testDisableKeyResponse $
--             disableKeyResponse
--
--         , testCreateKeyResponse $
--             createKeyResponse
--
--         , testRetireGrantResponse $
--             retireGrantResponse
--
--         , testListKeysResponse $
--             listKeysResponse
--
--         , testGetKeyRotationStatusResponse $
--             getKeyRotationStatusResponse
--
--         , testGenerateDataKeyResponse $
--             generateDataKeyResponse
--
--         , testDeleteAliasResponse $
--             deleteAliasResponse
--
--         , testUpdateAliasResponse $
--             updateAliasResponse
--
--         , testDescribeKeyResponse $
--             describeKeyResponse
--
--         , testDecryptResponse $
--             decryptResponse
--
--         , testUpdateKeyDescriptionResponse $
--             updateKeyDescriptionResponse
--
--         , testReEncryptResponse $
--             reEncryptResponse
--
--         , testListKeyPoliciesResponse $
--             listKeyPoliciesResponse
--
--         , testEnableKeyResponse $
--             enableKeyResponse
--
--         , testPutKeyPolicyResponse $
--             putKeyPolicyResponse
--
--         , testRevokeGrantResponse $
--             revokeGrantResponse
--
--         , testGetKeyPolicyResponse $
--             getKeyPolicyResponse
--
--           ]
--     ]

-- Requests

testDisableKeyRotation :: DisableKeyRotation -> TestTree
testDisableKeyRotation = undefined

testGenerateDataKeyWithoutPlaintext :: GenerateDataKeyWithoutPlaintext -> TestTree
testGenerateDataKeyWithoutPlaintext = undefined

testListGrants :: ListGrants -> TestTree
testListGrants = undefined

testEncrypt :: Encrypt -> TestTree
testEncrypt = undefined

testEnableKeyRotation :: EnableKeyRotation -> TestTree
testEnableKeyRotation = undefined

testCreateGrant :: CreateGrant -> TestTree
testCreateGrant = undefined

testCreateAlias :: CreateAlias -> TestTree
testCreateAlias = undefined

testListAliases :: ListAliases -> TestTree
testListAliases = undefined

testGenerateRandom :: GenerateRandom -> TestTree
testGenerateRandom = undefined

testDisableKey :: DisableKey -> TestTree
testDisableKey = undefined

testCreateKey :: CreateKey -> TestTree
testCreateKey = undefined

testRetireGrant :: RetireGrant -> TestTree
testRetireGrant = undefined

testListKeys :: ListKeys -> TestTree
testListKeys = undefined

testGetKeyRotationStatus :: GetKeyRotationStatus -> TestTree
testGetKeyRotationStatus = undefined

testGenerateDataKey :: GenerateDataKey -> TestTree
testGenerateDataKey = undefined

testDeleteAlias :: DeleteAlias -> TestTree
testDeleteAlias = undefined

testUpdateAlias :: UpdateAlias -> TestTree
testUpdateAlias = undefined

testDescribeKey :: DescribeKey -> TestTree
testDescribeKey = undefined

testDecrypt :: Decrypt -> TestTree
testDecrypt = undefined

testUpdateKeyDescription :: UpdateKeyDescription -> TestTree
testUpdateKeyDescription = undefined

testReEncrypt :: ReEncrypt -> TestTree
testReEncrypt = undefined

testListKeyPolicies :: ListKeyPolicies -> TestTree
testListKeyPolicies = undefined

testEnableKey :: EnableKey -> TestTree
testEnableKey = undefined

testPutKeyPolicy :: PutKeyPolicy -> TestTree
testPutKeyPolicy = undefined

testRevokeGrant :: RevokeGrant -> TestTree
testRevokeGrant = undefined

testGetKeyPolicy :: GetKeyPolicy -> TestTree
testGetKeyPolicy = undefined

-- Responses

testDisableKeyRotationResponse :: DisableKeyRotationResponse -> TestTree
testDisableKeyRotationResponse = resp
    "DisableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse"
    (Proxy :: Proxy DisableKeyRotation)

testGenerateDataKeyWithoutPlaintextResponse :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
testGenerateDataKeyWithoutPlaintextResponse = resp
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse"
    (Proxy :: Proxy GenerateDataKeyWithoutPlaintext)

testListGrantsResponse :: ListGrantsResponse -> TestTree
testListGrantsResponse = resp
    "ListGrantsResponse"
    "fixture/ListGrantsResponse"
    (Proxy :: Proxy ListGrants)

testEncryptResponse :: EncryptResponse -> TestTree
testEncryptResponse = resp
    "EncryptResponse"
    "fixture/EncryptResponse"
    (Proxy :: Proxy Encrypt)

testEnableKeyRotationResponse :: EnableKeyRotationResponse -> TestTree
testEnableKeyRotationResponse = resp
    "EnableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse"
    (Proxy :: Proxy EnableKeyRotation)

testCreateGrantResponse :: CreateGrantResponse -> TestTree
testCreateGrantResponse = resp
    "CreateGrantResponse"
    "fixture/CreateGrantResponse"
    (Proxy :: Proxy CreateGrant)

testCreateAliasResponse :: CreateAliasResponse -> TestTree
testCreateAliasResponse = resp
    "CreateAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

testListAliasesResponse :: ListAliasesResponse -> TestTree
testListAliasesResponse = resp
    "ListAliasesResponse"
    "fixture/ListAliasesResponse"
    (Proxy :: Proxy ListAliases)

testGenerateRandomResponse :: GenerateRandomResponse -> TestTree
testGenerateRandomResponse = resp
    "GenerateRandomResponse"
    "fixture/GenerateRandomResponse"
    (Proxy :: Proxy GenerateRandom)

testDisableKeyResponse :: DisableKeyResponse -> TestTree
testDisableKeyResponse = resp
    "DisableKeyResponse"
    "fixture/DisableKeyResponse"
    (Proxy :: Proxy DisableKey)

testCreateKeyResponse :: CreateKeyResponse -> TestTree
testCreateKeyResponse = resp
    "CreateKeyResponse"
    "fixture/CreateKeyResponse"
    (Proxy :: Proxy CreateKey)

testRetireGrantResponse :: RetireGrantResponse -> TestTree
testRetireGrantResponse = resp
    "RetireGrantResponse"
    "fixture/RetireGrantResponse"
    (Proxy :: Proxy RetireGrant)

testListKeysResponse :: ListKeysResponse -> TestTree
testListKeysResponse = resp
    "ListKeysResponse"
    "fixture/ListKeysResponse"
    (Proxy :: Proxy ListKeys)

testGetKeyRotationStatusResponse :: GetKeyRotationStatusResponse -> TestTree
testGetKeyRotationStatusResponse = resp
    "GetKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse"
    (Proxy :: Proxy GetKeyRotationStatus)

testGenerateDataKeyResponse :: GenerateDataKeyResponse -> TestTree
testGenerateDataKeyResponse = resp
    "GenerateDataKeyResponse"
    "fixture/GenerateDataKeyResponse"
    (Proxy :: Proxy GenerateDataKey)

testDeleteAliasResponse :: DeleteAliasResponse -> TestTree
testDeleteAliasResponse = resp
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse"
    (Proxy :: Proxy DeleteAlias)

testUpdateAliasResponse :: UpdateAliasResponse -> TestTree
testUpdateAliasResponse = resp
    "UpdateAliasResponse"
    "fixture/UpdateAliasResponse"
    (Proxy :: Proxy UpdateAlias)

testDescribeKeyResponse :: DescribeKeyResponse -> TestTree
testDescribeKeyResponse = resp
    "DescribeKeyResponse"
    "fixture/DescribeKeyResponse"
    (Proxy :: Proxy DescribeKey)

testDecryptResponse :: DecryptResponse -> TestTree
testDecryptResponse = resp
    "DecryptResponse"
    "fixture/DecryptResponse"
    (Proxy :: Proxy Decrypt)

testUpdateKeyDescriptionResponse :: UpdateKeyDescriptionResponse -> TestTree
testUpdateKeyDescriptionResponse = resp
    "UpdateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse"
    (Proxy :: Proxy UpdateKeyDescription)

testReEncryptResponse :: ReEncryptResponse -> TestTree
testReEncryptResponse = resp
    "ReEncryptResponse"
    "fixture/ReEncryptResponse"
    (Proxy :: Proxy ReEncrypt)

testListKeyPoliciesResponse :: ListKeyPoliciesResponse -> TestTree
testListKeyPoliciesResponse = resp
    "ListKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse"
    (Proxy :: Proxy ListKeyPolicies)

testEnableKeyResponse :: EnableKeyResponse -> TestTree
testEnableKeyResponse = resp
    "EnableKeyResponse"
    "fixture/EnableKeyResponse"
    (Proxy :: Proxy EnableKey)

testPutKeyPolicyResponse :: PutKeyPolicyResponse -> TestTree
testPutKeyPolicyResponse = resp
    "PutKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse"
    (Proxy :: Proxy PutKeyPolicy)

testRevokeGrantResponse :: RevokeGrantResponse -> TestTree
testRevokeGrantResponse = resp
    "RevokeGrantResponse"
    "fixture/RevokeGrantResponse"
    (Proxy :: Proxy RevokeGrant)

testGetKeyPolicyResponse :: GetKeyPolicyResponse -> TestTree
testGetKeyPolicyResponse = resp
    "GetKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse"
    (Proxy :: Proxy GetKeyPolicy)
