{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SecretsManager
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SecretsManager where

import Data.Proxy
import Network.AWS.SecretsManager
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SecretsManager.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteSecret $
--             deleteSecret
--
--         , requestListSecrets $
--             listSecrets
--
--         , requestUpdateSecret $
--             updateSecret
--
--         , requestRotateSecret $
--             rotateSecret
--
--         , requestCreateSecret $
--             createSecret
--
--         , requestGetSecretValue $
--             getSecretValue
--
--         , requestDescribeSecret $
--             describeSecret
--
--         , requestRestoreSecret $
--             restoreSecret
--
--         , requestCancelRotateSecret $
--             cancelRotateSecret
--
--         , requestPutSecretValue $
--             putSecretValue
--
--         , requestGetRandomPassword $
--             getRandomPassword
--
--         , requestListSecretVersionIds $
--             listSecretVersionIds
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestUpdateSecretVersionStage $
--             updateSecretVersionStage
--
--           ]

--     , testGroup "response"
--         [ responseDeleteSecret $
--             deleteSecretResponse
--
--         , responseListSecrets $
--             listSecretsResponse
--
--         , responseUpdateSecret $
--             updateSecretResponse
--
--         , responseRotateSecret $
--             rotateSecretResponse
--
--         , responseCreateSecret $
--             createSecretResponse
--
--         , responseGetSecretValue $
--             getSecretValueResponse
--
--         , responseDescribeSecret $
--             describeSecretResponse
--
--         , responseRestoreSecret $
--             restoreSecretResponse
--
--         , responseCancelRotateSecret $
--             cancelRotateSecretResponse
--
--         , responsePutSecretValue $
--             putSecretValueResponse
--
--         , responseGetRandomPassword $
--             getRandomPasswordResponse
--
--         , responseListSecretVersionIds $
--             listSecretVersionIdsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseUpdateSecretVersionStage $
--             updateSecretVersionStageResponse
--
--           ]
--     ]

-- Requests

requestDeleteSecret :: DeleteSecret -> TestTree
requestDeleteSecret = req
    "DeleteSecret"
    "fixture/DeleteSecret.yaml"

requestListSecrets :: ListSecrets -> TestTree
requestListSecrets = req
    "ListSecrets"
    "fixture/ListSecrets.yaml"

requestUpdateSecret :: UpdateSecret -> TestTree
requestUpdateSecret = req
    "UpdateSecret"
    "fixture/UpdateSecret.yaml"

requestRotateSecret :: RotateSecret -> TestTree
requestRotateSecret = req
    "RotateSecret"
    "fixture/RotateSecret.yaml"

requestCreateSecret :: CreateSecret -> TestTree
requestCreateSecret = req
    "CreateSecret"
    "fixture/CreateSecret.yaml"

requestGetSecretValue :: GetSecretValue -> TestTree
requestGetSecretValue = req
    "GetSecretValue"
    "fixture/GetSecretValue.yaml"

requestDescribeSecret :: DescribeSecret -> TestTree
requestDescribeSecret = req
    "DescribeSecret"
    "fixture/DescribeSecret.yaml"

requestRestoreSecret :: RestoreSecret -> TestTree
requestRestoreSecret = req
    "RestoreSecret"
    "fixture/RestoreSecret.yaml"

requestCancelRotateSecret :: CancelRotateSecret -> TestTree
requestCancelRotateSecret = req
    "CancelRotateSecret"
    "fixture/CancelRotateSecret.yaml"

requestPutSecretValue :: PutSecretValue -> TestTree
requestPutSecretValue = req
    "PutSecretValue"
    "fixture/PutSecretValue.yaml"

requestGetRandomPassword :: GetRandomPassword -> TestTree
requestGetRandomPassword = req
    "GetRandomPassword"
    "fixture/GetRandomPassword.yaml"

requestListSecretVersionIds :: ListSecretVersionIds -> TestTree
requestListSecretVersionIds = req
    "ListSecretVersionIds"
    "fixture/ListSecretVersionIds.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateSecretVersionStage :: UpdateSecretVersionStage -> TestTree
requestUpdateSecretVersionStage = req
    "UpdateSecretVersionStage"
    "fixture/UpdateSecretVersionStage.yaml"

-- Responses

responseDeleteSecret :: DeleteSecretResponse -> TestTree
responseDeleteSecret = res
    "DeleteSecretResponse"
    "fixture/DeleteSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy DeleteSecret)

responseListSecrets :: ListSecretsResponse -> TestTree
responseListSecrets = res
    "ListSecretsResponse"
    "fixture/ListSecretsResponse.proto"
    secretsManager
    (Proxy :: Proxy ListSecrets)

responseUpdateSecret :: UpdateSecretResponse -> TestTree
responseUpdateSecret = res
    "UpdateSecretResponse"
    "fixture/UpdateSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy UpdateSecret)

responseRotateSecret :: RotateSecretResponse -> TestTree
responseRotateSecret = res
    "RotateSecretResponse"
    "fixture/RotateSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy RotateSecret)

responseCreateSecret :: CreateSecretResponse -> TestTree
responseCreateSecret = res
    "CreateSecretResponse"
    "fixture/CreateSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy CreateSecret)

responseGetSecretValue :: GetSecretValueResponse -> TestTree
responseGetSecretValue = res
    "GetSecretValueResponse"
    "fixture/GetSecretValueResponse.proto"
    secretsManager
    (Proxy :: Proxy GetSecretValue)

responseDescribeSecret :: DescribeSecretResponse -> TestTree
responseDescribeSecret = res
    "DescribeSecretResponse"
    "fixture/DescribeSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy DescribeSecret)

responseRestoreSecret :: RestoreSecretResponse -> TestTree
responseRestoreSecret = res
    "RestoreSecretResponse"
    "fixture/RestoreSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy RestoreSecret)

responseCancelRotateSecret :: CancelRotateSecretResponse -> TestTree
responseCancelRotateSecret = res
    "CancelRotateSecretResponse"
    "fixture/CancelRotateSecretResponse.proto"
    secretsManager
    (Proxy :: Proxy CancelRotateSecret)

responsePutSecretValue :: PutSecretValueResponse -> TestTree
responsePutSecretValue = res
    "PutSecretValueResponse"
    "fixture/PutSecretValueResponse.proto"
    secretsManager
    (Proxy :: Proxy PutSecretValue)

responseGetRandomPassword :: GetRandomPasswordResponse -> TestTree
responseGetRandomPassword = res
    "GetRandomPasswordResponse"
    "fixture/GetRandomPasswordResponse.proto"
    secretsManager
    (Proxy :: Proxy GetRandomPassword)

responseListSecretVersionIds :: ListSecretVersionIdsResponse -> TestTree
responseListSecretVersionIds = res
    "ListSecretVersionIdsResponse"
    "fixture/ListSecretVersionIdsResponse.proto"
    secretsManager
    (Proxy :: Proxy ListSecretVersionIds)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    secretsManager
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    secretsManager
    (Proxy :: Proxy UntagResource)

responseUpdateSecretVersionStage :: UpdateSecretVersionStageResponse -> TestTree
responseUpdateSecretVersionStage = res
    "UpdateSecretVersionStageResponse"
    "fixture/UpdateSecretVersionStageResponse.proto"
    secretsManager
    (Proxy :: Proxy UpdateSecretVersionStage)
