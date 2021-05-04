{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SecretsManager
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDeleteSecret $
--             newDeleteSecret
--
--         , requestUpdateSecret $
--             newUpdateSecret
--
--         , requestRotateSecret $
--             newRotateSecret
--
--         , requestRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplication
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestValidateResourcePolicy $
--             newValidateResourcePolicy
--
--         , requestCancelRotateSecret $
--             newCancelRotateSecret
--
--         , requestRestoreSecret $
--             newRestoreSecret
--
--         , requestGetSecretValue $
--             newGetSecretValue
--
--         , requestDescribeSecret $
--             newDescribeSecret
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetRandomPassword $
--             newGetRandomPassword
--
--         , requestCreateSecret $
--             newCreateSecret
--
--         , requestStopReplicationToReplica $
--             newStopReplicationToReplica
--
--         , requestListSecrets $
--             newListSecrets
--
--         , requestReplicateSecretToRegions $
--             newReplicateSecretToRegions
--
--         , requestPutSecretValue $
--             newPutSecretValue
--
--         , requestUpdateSecretVersionStage $
--             newUpdateSecretVersionStage
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestListSecretVersionIds $
--             newListSecretVersionIds
--
--           ]

--     , testGroup "response"
--         [ responseDeleteSecret $
--             newDeleteSecretResponse
--
--         , responseUpdateSecret $
--             newUpdateSecretResponse
--
--         , responseRotateSecret $
--             newRotateSecretResponse
--
--         , responseRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplicationResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseValidateResourcePolicy $
--             newValidateResourcePolicyResponse
--
--         , responseCancelRotateSecret $
--             newCancelRotateSecretResponse
--
--         , responseRestoreSecret $
--             newRestoreSecretResponse
--
--         , responseGetSecretValue $
--             newGetSecretValueResponse
--
--         , responseDescribeSecret $
--             newDescribeSecretResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetRandomPassword $
--             newGetRandomPasswordResponse
--
--         , responseCreateSecret $
--             newCreateSecretResponse
--
--         , responseStopReplicationToReplica $
--             newStopReplicationToReplicaResponse
--
--         , responseListSecrets $
--             newListSecretsResponse
--
--         , responseReplicateSecretToRegions $
--             newReplicateSecretToRegionsResponse
--
--         , responsePutSecretValue $
--             newPutSecretValueResponse
--
--         , responseUpdateSecretVersionStage $
--             newUpdateSecretVersionStageResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseListSecretVersionIds $
--             newListSecretVersionIdsResponse
--
--           ]
--     ]

-- Requests

requestDeleteSecret :: DeleteSecret -> TestTree
requestDeleteSecret =
  req
    "DeleteSecret"
    "fixture/DeleteSecret.yaml"

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

requestRemoveRegionsFromReplication :: RemoveRegionsFromReplication -> TestTree
requestRemoveRegionsFromReplication =
  req
    "RemoveRegionsFromReplication"
    "fixture/RemoveRegionsFromReplication.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestValidateResourcePolicy :: ValidateResourcePolicy -> TestTree
requestValidateResourcePolicy =
  req
    "ValidateResourcePolicy"
    "fixture/ValidateResourcePolicy.yaml"

requestCancelRotateSecret :: CancelRotateSecret -> TestTree
requestCancelRotateSecret =
  req
    "CancelRotateSecret"
    "fixture/CancelRotateSecret.yaml"

requestRestoreSecret :: RestoreSecret -> TestTree
requestRestoreSecret =
  req
    "RestoreSecret"
    "fixture/RestoreSecret.yaml"

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

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetRandomPassword :: GetRandomPassword -> TestTree
requestGetRandomPassword =
  req
    "GetRandomPassword"
    "fixture/GetRandomPassword.yaml"

requestCreateSecret :: CreateSecret -> TestTree
requestCreateSecret =
  req
    "CreateSecret"
    "fixture/CreateSecret.yaml"

requestStopReplicationToReplica :: StopReplicationToReplica -> TestTree
requestStopReplicationToReplica =
  req
    "StopReplicationToReplica"
    "fixture/StopReplicationToReplica.yaml"

requestListSecrets :: ListSecrets -> TestTree
requestListSecrets =
  req
    "ListSecrets"
    "fixture/ListSecrets.yaml"

requestReplicateSecretToRegions :: ReplicateSecretToRegions -> TestTree
requestReplicateSecretToRegions =
  req
    "ReplicateSecretToRegions"
    "fixture/ReplicateSecretToRegions.yaml"

requestPutSecretValue :: PutSecretValue -> TestTree
requestPutSecretValue =
  req
    "PutSecretValue"
    "fixture/PutSecretValue.yaml"

requestUpdateSecretVersionStage :: UpdateSecretVersionStage -> TestTree
requestUpdateSecretVersionStage =
  req
    "UpdateSecretVersionStage"
    "fixture/UpdateSecretVersionStage.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestListSecretVersionIds :: ListSecretVersionIds -> TestTree
requestListSecretVersionIds =
  req
    "ListSecretVersionIds"
    "fixture/ListSecretVersionIds.yaml"

-- Responses

responseDeleteSecret :: DeleteSecretResponse -> TestTree
responseDeleteSecret =
  res
    "DeleteSecretResponse"
    "fixture/DeleteSecretResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSecret)

responseUpdateSecret :: UpdateSecretResponse -> TestTree
responseUpdateSecret =
  res
    "UpdateSecretResponse"
    "fixture/UpdateSecretResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecret)

responseRotateSecret :: RotateSecretResponse -> TestTree
responseRotateSecret =
  res
    "RotateSecretResponse"
    "fixture/RotateSecretResponse.proto"
    defaultService
    (Proxy :: Proxy RotateSecret)

responseRemoveRegionsFromReplication :: RemoveRegionsFromReplicationResponse -> TestTree
responseRemoveRegionsFromReplication =
  res
    "RemoveRegionsFromReplicationResponse"
    "fixture/RemoveRegionsFromReplicationResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveRegionsFromReplication)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourcePolicy)

responseValidateResourcePolicy :: ValidateResourcePolicyResponse -> TestTree
responseValidateResourcePolicy =
  res
    "ValidateResourcePolicyResponse"
    "fixture/ValidateResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateResourcePolicy)

responseCancelRotateSecret :: CancelRotateSecretResponse -> TestTree
responseCancelRotateSecret =
  res
    "CancelRotateSecretResponse"
    "fixture/CancelRotateSecretResponse.proto"
    defaultService
    (Proxy :: Proxy CancelRotateSecret)

responseRestoreSecret :: RestoreSecretResponse -> TestTree
responseRestoreSecret =
  res
    "RestoreSecretResponse"
    "fixture/RestoreSecretResponse.proto"
    defaultService
    (Proxy :: Proxy RestoreSecret)

responseGetSecretValue :: GetSecretValueResponse -> TestTree
responseGetSecretValue =
  res
    "GetSecretValueResponse"
    "fixture/GetSecretValueResponse.proto"
    defaultService
    (Proxy :: Proxy GetSecretValue)

responseDescribeSecret :: DescribeSecretResponse -> TestTree
responseDescribeSecret =
  res
    "DescribeSecretResponse"
    "fixture/DescribeSecretResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSecret)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetRandomPassword :: GetRandomPasswordResponse -> TestTree
responseGetRandomPassword =
  res
    "GetRandomPasswordResponse"
    "fixture/GetRandomPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy GetRandomPassword)

responseCreateSecret :: CreateSecretResponse -> TestTree
responseCreateSecret =
  res
    "CreateSecretResponse"
    "fixture/CreateSecretResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSecret)

responseStopReplicationToReplica :: StopReplicationToReplicaResponse -> TestTree
responseStopReplicationToReplica =
  res
    "StopReplicationToReplicaResponse"
    "fixture/StopReplicationToReplicaResponse.proto"
    defaultService
    (Proxy :: Proxy StopReplicationToReplica)

responseListSecrets :: ListSecretsResponse -> TestTree
responseListSecrets =
  res
    "ListSecretsResponse"
    "fixture/ListSecretsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecrets)

responseReplicateSecretToRegions :: ReplicateSecretToRegionsResponse -> TestTree
responseReplicateSecretToRegions =
  res
    "ReplicateSecretToRegionsResponse"
    "fixture/ReplicateSecretToRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy ReplicateSecretToRegions)

responsePutSecretValue :: PutSecretValueResponse -> TestTree
responsePutSecretValue =
  res
    "PutSecretValueResponse"
    "fixture/PutSecretValueResponse.proto"
    defaultService
    (Proxy :: Proxy PutSecretValue)

responseUpdateSecretVersionStage :: UpdateSecretVersionStageResponse -> TestTree
responseUpdateSecretVersionStage =
  res
    "UpdateSecretVersionStageResponse"
    "fixture/UpdateSecretVersionStageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSecretVersionStage)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseListSecretVersionIds :: ListSecretVersionIdsResponse -> TestTree
responseListSecretVersionIds =
  res
    "ListSecretVersionIdsResponse"
    "fixture/ListSecretVersionIdsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSecretVersionIds)
