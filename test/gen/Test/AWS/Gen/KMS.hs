-- Module      : Test.AWS.Gen.KMS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.KMS
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ disableKeyRotationTest $
--             disableKeyRotation
--
--         , generateDataKeyWithoutPlaintextTest $
--             generateDataKeyWithoutPlaintext
--
--         , listGrantsTest $
--             listGrants
--
--         , encryptTest $
--             encrypt
--
--         , enableKeyRotationTest $
--             enableKeyRotation
--
--         , createGrantTest $
--             createGrant
--
--         , createAliasTest $
--             createAlias
--
--         , listAliasesTest $
--             listAliases
--
--         , generateRandomTest $
--             generateRandom
--
--         , disableKeyTest $
--             disableKey
--
--         , createKeyTest $
--             createKey
--
--         , retireGrantTest $
--             retireGrant
--
--         , listKeysTest $
--             listKeys
--
--         , getKeyRotationStatusTest $
--             getKeyRotationStatus
--
--         , generateDataKeyTest $
--             generateDataKey
--
--         , deleteAliasTest $
--             deleteAlias
--
--         , updateAliasTest $
--             updateAlias
--
--         , describeKeyTest $
--             describeKey
--
--         , decryptTest $
--             decrypt
--
--         , updateKeyDescriptionTest $
--             updateKeyDescription
--
--         , reEncryptTest $
--             reEncrypt
--
--         , listKeyPoliciesTest $
--             listKeyPolicies
--
--         , enableKeyTest $
--             enableKey
--
--         , putKeyPolicyTest $
--             putKeyPolicy
--
--         , revokeGrantTest $
--             revokeGrant
--
--         , getKeyPolicyTest $
--             getKeyPolicy
--
--           ]

--     , testGroup "response"
--         [ disableKeyRotationResponseTest $
--             disableKeyRotationResponse
--
--         , generateDataKeyWithoutPlaintextResponseTest $
--             generateDataKeyWithoutPlaintextResponse
--
--         , listGrantsResponseTest $
--             listGrantsResponse
--
--         , encryptResponseTest $
--             encryptResponse
--
--         , enableKeyRotationResponseTest $
--             enableKeyRotationResponse
--
--         , createGrantResponseTest $
--             createGrantResponse
--
--         , createAliasResponseTest $
--             createAliasResponse
--
--         , listAliasesResponseTest $
--             listAliasesResponse
--
--         , generateRandomResponseTest $
--             generateRandomResponse
--
--         , disableKeyResponseTest $
--             disableKeyResponse
--
--         , createKeyResponseTest $
--             createKeyResponse
--
--         , retireGrantResponseTest $
--             retireGrantResponse
--
--         , listKeysResponseTest $
--             listKeysResponse
--
--         , getKeyRotationStatusResponseTest $
--             getKeyRotationStatusResponse
--
--         , generateDataKeyResponseTest $
--             generateDataKeyResponse
--
--         , deleteAliasResponseTest $
--             deleteAliasResponse
--
--         , updateAliasResponseTest $
--             updateAliasResponse
--
--         , describeKeyResponseTest $
--             describeKeyResponse
--
--         , decryptResponseTest $
--             decryptResponse
--
--         , updateKeyDescriptionResponseTest $
--             updateKeyDescriptionResponse
--
--         , reEncryptResponseTest $
--             reEncryptResponse
--
--         , listKeyPoliciesResponseTest $
--             listKeyPoliciesResponse
--
--         , enableKeyResponseTest $
--             enableKeyResponse
--
--         , putKeyPolicyResponseTest $
--             putKeyPolicyResponse
--
--         , revokeGrantResponseTest $
--             revokeGrantResponse
--
--         , getKeyPolicyResponseTest $
--             getKeyPolicyResponse
--
--           ]
--     ]

-- Requests

disableKeyRotationTest :: DisableKeyRotation -> TestTree
disableKeyRotationTest = undefined

generateDataKeyWithoutPlaintextTest :: GenerateDataKeyWithoutPlaintext -> TestTree
generateDataKeyWithoutPlaintextTest = undefined

listGrantsTest :: ListGrants -> TestTree
listGrantsTest = undefined

encryptTest :: Encrypt -> TestTree
encryptTest = undefined

enableKeyRotationTest :: EnableKeyRotation -> TestTree
enableKeyRotationTest = undefined

createGrantTest :: CreateGrant -> TestTree
createGrantTest = undefined

createAliasTest :: CreateAlias -> TestTree
createAliasTest = undefined

listAliasesTest :: ListAliases -> TestTree
listAliasesTest = undefined

generateRandomTest :: GenerateRandom -> TestTree
generateRandomTest = undefined

disableKeyTest :: DisableKey -> TestTree
disableKeyTest = undefined

createKeyTest :: CreateKey -> TestTree
createKeyTest = undefined

retireGrantTest :: RetireGrant -> TestTree
retireGrantTest = undefined

listKeysTest :: ListKeys -> TestTree
listKeysTest = undefined

getKeyRotationStatusTest :: GetKeyRotationStatus -> TestTree
getKeyRotationStatusTest = undefined

generateDataKeyTest :: GenerateDataKey -> TestTree
generateDataKeyTest = undefined

deleteAliasTest :: DeleteAlias -> TestTree
deleteAliasTest = undefined

updateAliasTest :: UpdateAlias -> TestTree
updateAliasTest = undefined

describeKeyTest :: DescribeKey -> TestTree
describeKeyTest = undefined

decryptTest :: Decrypt -> TestTree
decryptTest = undefined

updateKeyDescriptionTest :: UpdateKeyDescription -> TestTree
updateKeyDescriptionTest = undefined

reEncryptTest :: ReEncrypt -> TestTree
reEncryptTest = undefined

listKeyPoliciesTest :: ListKeyPolicies -> TestTree
listKeyPoliciesTest = undefined

enableKeyTest :: EnableKey -> TestTree
enableKeyTest = undefined

putKeyPolicyTest :: PutKeyPolicy -> TestTree
putKeyPolicyTest = undefined

revokeGrantTest :: RevokeGrant -> TestTree
revokeGrantTest = undefined

getKeyPolicyTest :: GetKeyPolicy -> TestTree
getKeyPolicyTest = undefined

-- Responses

disableKeyRotationResponseTest :: DisableKeyRotationResponse -> TestTree
disableKeyRotationResponseTest = resp
    "DisableKeyRotationResponse"
    "fixture/KMS/DisableKeyRotationResponse"
    (Proxy :: Proxy DisableKeyRotation)

generateDataKeyWithoutPlaintextResponseTest :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
generateDataKeyWithoutPlaintextResponseTest = resp
    "GenerateDataKeyWithoutPlaintextResponse"
    "fixture/KMS/GenerateDataKeyWithoutPlaintextResponse"
    (Proxy :: Proxy GenerateDataKeyWithoutPlaintext)

listGrantsResponseTest :: ListGrantsResponse -> TestTree
listGrantsResponseTest = resp
    "ListGrantsResponse"
    "fixture/KMS/ListGrantsResponse"
    (Proxy :: Proxy ListGrants)

encryptResponseTest :: EncryptResponse -> TestTree
encryptResponseTest = resp
    "EncryptResponse"
    "fixture/KMS/EncryptResponse"
    (Proxy :: Proxy Encrypt)

enableKeyRotationResponseTest :: EnableKeyRotationResponse -> TestTree
enableKeyRotationResponseTest = resp
    "EnableKeyRotationResponse"
    "fixture/KMS/EnableKeyRotationResponse"
    (Proxy :: Proxy EnableKeyRotation)

createGrantResponseTest :: CreateGrantResponse -> TestTree
createGrantResponseTest = resp
    "CreateGrantResponse"
    "fixture/KMS/CreateGrantResponse"
    (Proxy :: Proxy CreateGrant)

createAliasResponseTest :: CreateAliasResponse -> TestTree
createAliasResponseTest = resp
    "CreateAliasResponse"
    "fixture/KMS/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

listAliasesResponseTest :: ListAliasesResponse -> TestTree
listAliasesResponseTest = resp
    "ListAliasesResponse"
    "fixture/KMS/ListAliasesResponse"
    (Proxy :: Proxy ListAliases)

generateRandomResponseTest :: GenerateRandomResponse -> TestTree
generateRandomResponseTest = resp
    "GenerateRandomResponse"
    "fixture/KMS/GenerateRandomResponse"
    (Proxy :: Proxy GenerateRandom)

disableKeyResponseTest :: DisableKeyResponse -> TestTree
disableKeyResponseTest = resp
    "DisableKeyResponse"
    "fixture/KMS/DisableKeyResponse"
    (Proxy :: Proxy DisableKey)

createKeyResponseTest :: CreateKeyResponse -> TestTree
createKeyResponseTest = resp
    "CreateKeyResponse"
    "fixture/KMS/CreateKeyResponse"
    (Proxy :: Proxy CreateKey)

retireGrantResponseTest :: RetireGrantResponse -> TestTree
retireGrantResponseTest = resp
    "RetireGrantResponse"
    "fixture/KMS/RetireGrantResponse"
    (Proxy :: Proxy RetireGrant)

listKeysResponseTest :: ListKeysResponse -> TestTree
listKeysResponseTest = resp
    "ListKeysResponse"
    "fixture/KMS/ListKeysResponse"
    (Proxy :: Proxy ListKeys)

getKeyRotationStatusResponseTest :: GetKeyRotationStatusResponse -> TestTree
getKeyRotationStatusResponseTest = resp
    "GetKeyRotationStatusResponse"
    "fixture/KMS/GetKeyRotationStatusResponse"
    (Proxy :: Proxy GetKeyRotationStatus)

generateDataKeyResponseTest :: GenerateDataKeyResponse -> TestTree
generateDataKeyResponseTest = resp
    "GenerateDataKeyResponse"
    "fixture/KMS/GenerateDataKeyResponse"
    (Proxy :: Proxy GenerateDataKey)

deleteAliasResponseTest :: DeleteAliasResponse -> TestTree
deleteAliasResponseTest = resp
    "DeleteAliasResponse"
    "fixture/KMS/DeleteAliasResponse"
    (Proxy :: Proxy DeleteAlias)

updateAliasResponseTest :: UpdateAliasResponse -> TestTree
updateAliasResponseTest = resp
    "UpdateAliasResponse"
    "fixture/KMS/UpdateAliasResponse"
    (Proxy :: Proxy UpdateAlias)

describeKeyResponseTest :: DescribeKeyResponse -> TestTree
describeKeyResponseTest = resp
    "DescribeKeyResponse"
    "fixture/KMS/DescribeKeyResponse"
    (Proxy :: Proxy DescribeKey)

decryptResponseTest :: DecryptResponse -> TestTree
decryptResponseTest = resp
    "DecryptResponse"
    "fixture/KMS/DecryptResponse"
    (Proxy :: Proxy Decrypt)

updateKeyDescriptionResponseTest :: UpdateKeyDescriptionResponse -> TestTree
updateKeyDescriptionResponseTest = resp
    "UpdateKeyDescriptionResponse"
    "fixture/KMS/UpdateKeyDescriptionResponse"
    (Proxy :: Proxy UpdateKeyDescription)

reEncryptResponseTest :: ReEncryptResponse -> TestTree
reEncryptResponseTest = resp
    "ReEncryptResponse"
    "fixture/KMS/ReEncryptResponse"
    (Proxy :: Proxy ReEncrypt)

listKeyPoliciesResponseTest :: ListKeyPoliciesResponse -> TestTree
listKeyPoliciesResponseTest = resp
    "ListKeyPoliciesResponse"
    "fixture/KMS/ListKeyPoliciesResponse"
    (Proxy :: Proxy ListKeyPolicies)

enableKeyResponseTest :: EnableKeyResponse -> TestTree
enableKeyResponseTest = resp
    "EnableKeyResponse"
    "fixture/KMS/EnableKeyResponse"
    (Proxy :: Proxy EnableKey)

putKeyPolicyResponseTest :: PutKeyPolicyResponse -> TestTree
putKeyPolicyResponseTest = resp
    "PutKeyPolicyResponse"
    "fixture/KMS/PutKeyPolicyResponse"
    (Proxy :: Proxy PutKeyPolicy)

revokeGrantResponseTest :: RevokeGrantResponse -> TestTree
revokeGrantResponseTest = resp
    "RevokeGrantResponse"
    "fixture/KMS/RevokeGrantResponse"
    (Proxy :: Proxy RevokeGrant)

getKeyPolicyResponseTest :: GetKeyPolicyResponse -> TestTree
getKeyPolicyResponseTest = resp
    "GetKeyPolicyResponse"
    "fixture/KMS/GetKeyPolicyResponse"
    (Proxy :: Proxy GetKeyPolicy)
