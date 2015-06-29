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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.KMS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createAliasTest $
--             createAlias
--
--         , createGrantTest $
--             createGrant
--
--         , createKeyTest $
--             createKey
--
--         , decryptTest $
--             decrypt
--
--         , deleteAliasTest $
--             deleteAlias
--
--         , describeKeyTest $
--             describeKey
--
--         , disableKeyTest $
--             disableKey
--
--         , disableKeyRotationTest $
--             disableKeyRotation
--
--         , enableKeyTest $
--             enableKey
--
--         , enableKeyRotationTest $
--             enableKeyRotation
--
--         , encryptTest $
--             encrypt
--
--         , generateDataKeyTest $
--             generateDataKey
--
--         , generateDataKeyWithoutPlaintextTest $
--             generateDataKeyWithoutPlaintext
--
--         , generateRandomTest $
--             generateRandom
--
--         , getKeyPolicyTest $
--             getKeyPolicy
--
--         , getKeyRotationStatusTest $
--             getKeyRotationStatus
--
--         , listAliasesTest $
--             listAliases
--
--         , listGrantsTest $
--             listGrants
--
--         , listKeyPoliciesTest $
--             listKeyPolicies
--
--         , listKeysTest $
--             listKeys
--
--         , putKeyPolicyTest $
--             putKeyPolicy
--
--         , reEncryptTest $
--             reEncrypt
--
--         , retireGrantTest $
--             retireGrant
--
--         , revokeGrantTest $
--             revokeGrant
--
--         , updateAliasTest $
--             updateAlias
--
--         , updateKeyDescriptionTest $
--             updateKeyDescription
--
--           ]

--     , testGroup "response"
--         [ createAliasResponseTest $
--             createAliasResponse
--
--         , createGrantResponseTest $
--             createGrantResponse
--
--         , createKeyResponseTest $
--             createKeyResponse
--
--         , decryptResponseTest $
--             decryptResponse
--
--         , deleteAliasResponseTest $
--             deleteAliasResponse
--
--         , describeKeyResponseTest $
--             describeKeyResponse
--
--         , disableKeyResponseTest $
--             disableKeyResponse
--
--         , disableKeyRotationResponseTest $
--             disableKeyRotationResponse
--
--         , enableKeyResponseTest $
--             enableKeyResponse
--
--         , enableKeyRotationResponseTest $
--             enableKeyRotationResponse
--
--         , encryptResponseTest $
--             encryptResponse
--
--         , generateDataKeyResponseTest $
--             generateDataKeyResponse
--
--         , generateDataKeyWithoutPlaintextResponseTest $
--             generateDataKeyWithoutPlaintextResponse
--
--         , generateRandomResponseTest $
--             generateRandomResponse
--
--         , getKeyPolicyResponseTest $
--             getKeyPolicyResponse
--
--         , getKeyRotationStatusResponseTest $
--             getKeyRotationStatusResponse
--
--         , listAliasesResponseTest $
--             listAliasesResponse
--
--         , listGrantsResponseTest $
--             listGrantsResponse
--
--         , listKeyPoliciesResponseTest $
--             listKeyPoliciesResponse
--
--         , listKeysResponseTest $
--             listKeysResponse
--
--         , putKeyPolicyResponseTest $
--             putKeyPolicyResponse
--
--         , reEncryptResponseTest $
--             reEncryptResponse
--
--         , retireGrantResponseTest $
--             retireGrantResponse
--
--         , revokeGrantResponseTest $
--             revokeGrantResponse
--
--         , updateAliasResponseTest $
--             updateAliasResponse
--
--         , updateKeyDescriptionResponseTest $
--             updateKeyDescriptionResponse
--
--           ]
--     ]

-- Requests

createAliasTest :: CreateAlias -> TestTree
createAliasTest = undefined

createGrantTest :: CreateGrant -> TestTree
createGrantTest = undefined

createKeyTest :: CreateKey -> TestTree
createKeyTest = undefined

decryptTest :: Decrypt -> TestTree
decryptTest = undefined

deleteAliasTest :: DeleteAlias -> TestTree
deleteAliasTest = undefined

describeKeyTest :: DescribeKey -> TestTree
describeKeyTest = undefined

disableKeyTest :: DisableKey -> TestTree
disableKeyTest = undefined

disableKeyRotationTest :: DisableKeyRotation -> TestTree
disableKeyRotationTest = undefined

enableKeyTest :: EnableKey -> TestTree
enableKeyTest = undefined

enableKeyRotationTest :: EnableKeyRotation -> TestTree
enableKeyRotationTest = undefined

encryptTest :: Encrypt -> TestTree
encryptTest = undefined

generateDataKeyTest :: GenerateDataKey -> TestTree
generateDataKeyTest = undefined

generateDataKeyWithoutPlaintextTest :: GenerateDataKeyWithoutPlaintext -> TestTree
generateDataKeyWithoutPlaintextTest = undefined

generateRandomTest :: GenerateRandom -> TestTree
generateRandomTest = undefined

getKeyPolicyTest :: GetKeyPolicy -> TestTree
getKeyPolicyTest = undefined

getKeyRotationStatusTest :: GetKeyRotationStatus -> TestTree
getKeyRotationStatusTest = undefined

listAliasesTest :: ListAliases -> TestTree
listAliasesTest = undefined

listGrantsTest :: ListGrants -> TestTree
listGrantsTest = undefined

listKeyPoliciesTest :: ListKeyPolicies -> TestTree
listKeyPoliciesTest = undefined

listKeysTest :: ListKeys -> TestTree
listKeysTest = undefined

putKeyPolicyTest :: PutKeyPolicy -> TestTree
putKeyPolicyTest = undefined

reEncryptTest :: ReEncrypt -> TestTree
reEncryptTest = undefined

retireGrantTest :: RetireGrant -> TestTree
retireGrantTest = undefined

revokeGrantTest :: RevokeGrant -> TestTree
revokeGrantTest = undefined

updateAliasTest :: UpdateAlias -> TestTree
updateAliasTest = undefined

updateKeyDescriptionTest :: UpdateKeyDescription -> TestTree
updateKeyDescriptionTest = undefined

-- Responses

createAliasResponseTest :: CreateAliasResponse -> TestTree
createAliasResponseTest = resp
    "createAliasResponse"
    "fixture/CreateAliasResponse"
    (Proxy :: Proxy CreateAlias)

createGrantResponseTest :: CreateGrantResponse -> TestTree
createGrantResponseTest = resp
    "createGrantResponse"
    "fixture/CreateGrantResponse"
    (Proxy :: Proxy CreateGrant)

createKeyResponseTest :: CreateKeyResponse -> TestTree
createKeyResponseTest = resp
    "createKeyResponse"
    "fixture/CreateKeyResponse"
    (Proxy :: Proxy CreateKey)

decryptResponseTest :: DecryptResponse -> TestTree
decryptResponseTest = resp
    "decryptResponse"
    "fixture/DecryptResponse"
    (Proxy :: Proxy Decrypt)

deleteAliasResponseTest :: DeleteAliasResponse -> TestTree
deleteAliasResponseTest = resp
    "deleteAliasResponse"
    "fixture/DeleteAliasResponse"
    (Proxy :: Proxy DeleteAlias)

describeKeyResponseTest :: DescribeKeyResponse -> TestTree
describeKeyResponseTest = resp
    "describeKeyResponse"
    "fixture/DescribeKeyResponse"
    (Proxy :: Proxy DescribeKey)

disableKeyResponseTest :: DisableKeyResponse -> TestTree
disableKeyResponseTest = resp
    "disableKeyResponse"
    "fixture/DisableKeyResponse"
    (Proxy :: Proxy DisableKey)

disableKeyRotationResponseTest :: DisableKeyRotationResponse -> TestTree
disableKeyRotationResponseTest = resp
    "disableKeyRotationResponse"
    "fixture/DisableKeyRotationResponse"
    (Proxy :: Proxy DisableKeyRotation)

enableKeyResponseTest :: EnableKeyResponse -> TestTree
enableKeyResponseTest = resp
    "enableKeyResponse"
    "fixture/EnableKeyResponse"
    (Proxy :: Proxy EnableKey)

enableKeyRotationResponseTest :: EnableKeyRotationResponse -> TestTree
enableKeyRotationResponseTest = resp
    "enableKeyRotationResponse"
    "fixture/EnableKeyRotationResponse"
    (Proxy :: Proxy EnableKeyRotation)

encryptResponseTest :: EncryptResponse -> TestTree
encryptResponseTest = resp
    "encryptResponse"
    "fixture/EncryptResponse"
    (Proxy :: Proxy Encrypt)

generateDataKeyResponseTest :: GenerateDataKeyResponse -> TestTree
generateDataKeyResponseTest = resp
    "generateDataKeyResponse"
    "fixture/GenerateDataKeyResponse"
    (Proxy :: Proxy GenerateDataKey)

generateDataKeyWithoutPlaintextResponseTest :: GenerateDataKeyWithoutPlaintextResponse -> TestTree
generateDataKeyWithoutPlaintextResponseTest = resp
    "generateDataKeyWithoutPlaintextResponse"
    "fixture/GenerateDataKeyWithoutPlaintextResponse"
    (Proxy :: Proxy GenerateDataKeyWithoutPlaintext)

generateRandomResponseTest :: GenerateRandomResponse -> TestTree
generateRandomResponseTest = resp
    "generateRandomResponse"
    "fixture/GenerateRandomResponse"
    (Proxy :: Proxy GenerateRandom)

getKeyPolicyResponseTest :: GetKeyPolicyResponse -> TestTree
getKeyPolicyResponseTest = resp
    "getKeyPolicyResponse"
    "fixture/GetKeyPolicyResponse"
    (Proxy :: Proxy GetKeyPolicy)

getKeyRotationStatusResponseTest :: GetKeyRotationStatusResponse -> TestTree
getKeyRotationStatusResponseTest = resp
    "getKeyRotationStatusResponse"
    "fixture/GetKeyRotationStatusResponse"
    (Proxy :: Proxy GetKeyRotationStatus)

listAliasesResponseTest :: ListAliasesResponse -> TestTree
listAliasesResponseTest = resp
    "listAliasesResponse"
    "fixture/ListAliasesResponse"
    (Proxy :: Proxy ListAliases)

listGrantsResponseTest :: ListGrantsResponse -> TestTree
listGrantsResponseTest = resp
    "listGrantsResponse"
    "fixture/ListGrantsResponse"
    (Proxy :: Proxy ListGrants)

listKeyPoliciesResponseTest :: ListKeyPoliciesResponse -> TestTree
listKeyPoliciesResponseTest = resp
    "listKeyPoliciesResponse"
    "fixture/ListKeyPoliciesResponse"
    (Proxy :: Proxy ListKeyPolicies)

listKeysResponseTest :: ListKeysResponse -> TestTree
listKeysResponseTest = resp
    "listKeysResponse"
    "fixture/ListKeysResponse"
    (Proxy :: Proxy ListKeys)

putKeyPolicyResponseTest :: PutKeyPolicyResponse -> TestTree
putKeyPolicyResponseTest = resp
    "putKeyPolicyResponse"
    "fixture/PutKeyPolicyResponse"
    (Proxy :: Proxy PutKeyPolicy)

reEncryptResponseTest :: ReEncryptResponse -> TestTree
reEncryptResponseTest = resp
    "reEncryptResponse"
    "fixture/ReEncryptResponse"
    (Proxy :: Proxy ReEncrypt)

retireGrantResponseTest :: RetireGrantResponse -> TestTree
retireGrantResponseTest = resp
    "retireGrantResponse"
    "fixture/RetireGrantResponse"
    (Proxy :: Proxy RetireGrant)

revokeGrantResponseTest :: RevokeGrantResponse -> TestTree
revokeGrantResponseTest = resp
    "revokeGrantResponse"
    "fixture/RevokeGrantResponse"
    (Proxy :: Proxy RevokeGrant)

updateAliasResponseTest :: UpdateAliasResponse -> TestTree
updateAliasResponseTest = resp
    "updateAliasResponse"
    "fixture/UpdateAliasResponse"
    (Proxy :: Proxy UpdateAlias)

updateKeyDescriptionResponseTest :: UpdateKeyDescriptionResponse -> TestTree
updateKeyDescriptionResponseTest = resp
    "updateKeyDescriptionResponse"
    "fixture/UpdateKeyDescriptionResponse"
    (Proxy :: Proxy UpdateKeyDescription)
