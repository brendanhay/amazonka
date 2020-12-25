{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SecretsManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestValidateResourcePolicy $
--             mkValidateResourcePolicy
--
--         , requestDeleteSecret $
--             mkDeleteSecret
--
--         , requestListSecrets $
--             mkListSecrets
--
--         , requestUpdateSecret $
--             mkUpdateSecret
--
--         , requestRotateSecret $
--             mkRotateSecret
--
--         , requestCreateSecret $
--             mkCreateSecret
--
--         , requestGetSecretValue $
--             mkGetSecretValue
--
--         , requestDescribeSecret $
--             mkDescribeSecret
--
--         , requestRestoreSecret $
--             mkRestoreSecret
--
--         , requestCancelRotateSecret $
--             mkCancelRotateSecret
--
--         , requestGetResourcePolicy $
--             mkGetResourcePolicy
--
--         , requestPutSecretValue $
--             mkPutSecretValue
--
--         , requestGetRandomPassword $
--             mkGetRandomPassword
--
--         , requestListSecretVersionIds $
--             mkListSecretVersionIds
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestPutResourcePolicy $
--             mkPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             mkDeleteResourcePolicy
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestUpdateSecretVersionStage $
--             mkUpdateSecretVersionStage
--
--           ]

--     , testGroup "response"
--         [ responseValidateResourcePolicy $
--             mkValidateResourcePolicyResponse
--
--         , responseDeleteSecret $
--             mkDeleteSecretResponse
--
--         , responseListSecrets $
--             mkListSecretsResponse
--
--         , responseUpdateSecret $
--             mkUpdateSecretResponse
--
--         , responseRotateSecret $
--             mkRotateSecretResponse
--
--         , responseCreateSecret $
--             mkCreateSecretResponse
--
--         , responseGetSecretValue $
--             mkGetSecretValueResponse
--
--         , responseDescribeSecret $
--             mkDescribeSecretResponse
--
--         , responseRestoreSecret $
--             mkRestoreSecretResponse
--
--         , responseCancelRotateSecret $
--             mkCancelRotateSecretResponse
--
--         , responseGetResourcePolicy $
--             mkGetResourcePolicyResponse
--
--         , responsePutSecretValue $
--             mkPutSecretValueResponse
--
--         , responseGetRandomPassword $
--             mkGetRandomPasswordResponse
--
--         , responseListSecretVersionIds $
--             mkListSecretVersionIdsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responsePutResourcePolicy $
--             mkPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             mkDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseUpdateSecretVersionStage $
--             mkUpdateSecretVersionStageResponse
--
--           ]
--     ]

-- Requests

requestValidateResourcePolicy :: ValidateResourcePolicy -> TestTree
requestValidateResourcePolicy =
  req
    "ValidateResourcePolicy"
    "fixture/ValidateResourcePolicy.yaml"

requestDeleteSecret :: DeleteSecret -> TestTree
requestDeleteSecret =
  req
    "DeleteSecret"
    "fixture/DeleteSecret.yaml"

requestListSecrets :: ListSecrets -> TestTree
requestListSecrets =
  req
    "ListSecrets"
    "fixture/ListSecrets.yaml"

requestUpdateSecret :: UpdateSecret -> TestTree
requestUpdateSecret =
  req
    "UpdateSecret"
    "fixture/UpdateSecret.yaml"

requestRotateSecret :: RotateSecret -> TestTree
requestRotateSecret =
  req
    "RotateSecret"
    "fixture/RotateSecret.yaml"

requestCreateSecret :: CreateSecret -> TestTree
requestCreateSecret =
  req
    "CreateSecret"
    "fixture/CreateSecret.yaml"

requestGetSecretValue :: GetSecretValue -> TestTree
requestGetSecretValue =
  req
    "GetSecretValue"
    "fixture/GetSecretValue.yaml"

requestDescribeSecret :: DescribeSecret -> TestTree
requestDescribeSecret =
  req
    "DescribeSecret"
    "fixture/DescribeSecret.yaml"

requestRestoreSecret :: RestoreSecret -> TestTree
requestRestoreSecret =
  req
    "RestoreSecret"
    "fixture/RestoreSecret.yaml"

requestCancelRotateSecret :: CancelRotateSecret -> TestTree
requestCancelRotateSecret =
  req
    "CancelRotateSecret"
    "fixture/CancelRotateSecret.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestPutSecretValue :: PutSecretValue -> TestTree
requestPutSecretValue =
  req
    "PutSecretValue"
    "fixture/PutSecretValue.yaml"

requestGetRandomPassword :: GetRandomPassword -> TestTree
requestGetRandomPassword =
  req
    "GetRandomPassword"
    "fixture/GetRandomPassword.yaml"

requestListSecretVersionIds :: ListSecretVersionIds -> TestTree
requestListSecretVersionIds =
  req
    "ListSecretVersionIds"
    "fixture/ListSecretVersionIds.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateSecretVersionStage :: UpdateSecretVersionStage -> TestTree
requestUpdateSecretVersionStage =
  req
    "UpdateSecretVersionStage"
    "fixture/UpdateSecretVersionStage.yaml"

-- Responses

responseValidateResourcePolicy :: ValidateResourcePolicyResponse -> TestTree
responseValidateResourcePolicy =
  res
    "ValidateResourcePolicyResponse"
    "fixture/ValidateResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ValidateResourcePolicy)

responseDeleteSecret :: DeleteSecretResponse -> TestTree
responseDeleteSecret =
  res
    "DeleteSecretResponse"
    "fixture/DeleteSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSecret)

responseListSecrets :: ListSecretsResponse -> TestTree
responseListSecrets =
  res
    "ListSecretsResponse"
    "fixture/ListSecretsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecrets)

responseUpdateSecret :: UpdateSecretResponse -> TestTree
responseUpdateSecret =
  res
    "UpdateSecretResponse"
    "fixture/UpdateSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSecret)

responseRotateSecret :: RotateSecretResponse -> TestTree
responseRotateSecret =
  res
    "RotateSecretResponse"
    "fixture/RotateSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RotateSecret)

responseCreateSecret :: CreateSecretResponse -> TestTree
responseCreateSecret =
  res
    "CreateSecretResponse"
    "fixture/CreateSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSecret)

responseGetSecretValue :: GetSecretValueResponse -> TestTree
responseGetSecretValue =
  res
    "GetSecretValueResponse"
    "fixture/GetSecretValueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSecretValue)

responseDescribeSecret :: DescribeSecretResponse -> TestTree
responseDescribeSecret =
  res
    "DescribeSecretResponse"
    "fixture/DescribeSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeSecret)

responseRestoreSecret :: RestoreSecretResponse -> TestTree
responseRestoreSecret =
  res
    "RestoreSecretResponse"
    "fixture/RestoreSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RestoreSecret)

responseCancelRotateSecret :: CancelRotateSecretResponse -> TestTree
responseCancelRotateSecret =
  res
    "CancelRotateSecretResponse"
    "fixture/CancelRotateSecretResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelRotateSecret)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetResourcePolicy)

responsePutSecretValue :: PutSecretValueResponse -> TestTree
responsePutSecretValue =
  res
    "PutSecretValueResponse"
    "fixture/PutSecretValueResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutSecretValue)

responseGetRandomPassword :: GetRandomPasswordResponse -> TestTree
responseGetRandomPassword =
  res
    "GetRandomPasswordResponse"
    "fixture/GetRandomPasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRandomPassword)

responseListSecretVersionIds :: ListSecretVersionIdsResponse -> TestTree
responseListSecretVersionIds =
  res
    "ListSecretVersionIdsResponse"
    "fixture/ListSecretVersionIdsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSecretVersionIds)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseUpdateSecretVersionStage :: UpdateSecretVersionStageResponse -> TestTree
responseUpdateSecretVersionStage =
  res
    "UpdateSecretVersionStageResponse"
    "fixture/UpdateSecretVersionStageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSecretVersionStage)
