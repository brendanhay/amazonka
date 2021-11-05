{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SecretsManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SecretsManager where

import Amazonka.SecretsManager
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SecretsManager.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestValidateResourcePolicy $
--             newValidateResourcePolicy
--
--         , requestDeleteSecret $
--             newDeleteSecret
--
--         , requestListSecrets $
--             newListSecrets
--
--         , requestUpdateSecret $
--             newUpdateSecret
--
--         , requestRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplication
--
--         , requestRotateSecret $
--             newRotateSecret
--
--         , requestCreateSecret $
--             newCreateSecret
--
--         , requestGetSecretValue $
--             newGetSecretValue
--
--         , requestDescribeSecret $
--             newDescribeSecret
--
--         , requestRestoreSecret $
--             newRestoreSecret
--
--         , requestCancelRotateSecret $
--             newCancelRotateSecret
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestPutSecretValue $
--             newPutSecretValue
--
--         , requestReplicateSecretToRegions $
--             newReplicateSecretToRegions
--
--         , requestStopReplicationToReplica $
--             newStopReplicationToReplica
--
--         , requestGetRandomPassword $
--             newGetRandomPassword
--
--         , requestListSecretVersionIds $
--             newListSecretVersionIds
--
--         , requestTagResource $
--             newTagResource
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateSecretVersionStage $
--             newUpdateSecretVersionStage
--
--           ]

--     , testGroup "response"
--         [ responseValidateResourcePolicy $
--             newValidateResourcePolicyResponse
--
--         , responseDeleteSecret $
--             newDeleteSecretResponse
--
--         , responseListSecrets $
--             newListSecretsResponse
--
--         , responseUpdateSecret $
--             newUpdateSecretResponse
--
--         , responseRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplicationResponse
--
--         , responseRotateSecret $
--             newRotateSecretResponse
--
--         , responseCreateSecret $
--             newCreateSecretResponse
--
--         , responseGetSecretValue $
--             newGetSecretValueResponse
--
--         , responseDescribeSecret $
--             newDescribeSecretResponse
--
--         , responseRestoreSecret $
--             newRestoreSecretResponse
--
--         , responseCancelRotateSecret $
--             newCancelRotateSecretResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responsePutSecretValue $
--             newPutSecretValueResponse
--
--         , responseReplicateSecretToRegions $
--             newReplicateSecretToRegionsResponse
--
--         , responseStopReplicationToReplica $
--             newStopReplicationToReplicaResponse
--
--         , responseGetRandomPassword $
--             newGetRandomPasswordResponse
--
--         , responseListSecretVersionIds $
--             newListSecretVersionIdsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateSecretVersionStage $
--             newUpdateSecretVersionStageResponse
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

requestRemoveRegionsFromReplication :: RemoveRegionsFromReplication -> TestTree
requestRemoveRegionsFromReplication =
  req
    "RemoveRegionsFromReplication"
    "fixture/RemoveRegionsFromReplication.yaml"

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

requestReplicateSecretToRegions :: ReplicateSecretToRegions -> TestTree
requestReplicateSecretToRegions =
  req
    "ReplicateSecretToRegions"
    "fixture/ReplicateSecretToRegions.yaml"

requestStopReplicationToReplica :: StopReplicationToReplica -> TestTree
requestStopReplicationToReplica =
  req
    "StopReplicationToReplica"
    "fixture/StopReplicationToReplica.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateResourcePolicy)

responseDeleteSecret :: DeleteSecretResponse -> TestTree
responseDeleteSecret =
  res
    "DeleteSecretResponse"
    "fixture/DeleteSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecret)

responseListSecrets :: ListSecretsResponse -> TestTree
responseListSecrets =
  res
    "ListSecretsResponse"
    "fixture/ListSecretsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecrets)

responseUpdateSecret :: UpdateSecretResponse -> TestTree
responseUpdateSecret =
  res
    "UpdateSecretResponse"
    "fixture/UpdateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecret)

responseRemoveRegionsFromReplication :: RemoveRegionsFromReplicationResponse -> TestTree
responseRemoveRegionsFromReplication =
  res
    "RemoveRegionsFromReplicationResponse"
    "fixture/RemoveRegionsFromReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRegionsFromReplication)

responseRotateSecret :: RotateSecretResponse -> TestTree
responseRotateSecret =
  res
    "RotateSecretResponse"
    "fixture/RotateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RotateSecret)

responseCreateSecret :: CreateSecretResponse -> TestTree
responseCreateSecret =
  res
    "CreateSecretResponse"
    "fixture/CreateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecret)

responseGetSecretValue :: GetSecretValueResponse -> TestTree
responseGetSecretValue =
  res
    "GetSecretValueResponse"
    "fixture/GetSecretValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecretValue)

responseDescribeSecret :: DescribeSecretResponse -> TestTree
responseDescribeSecret =
  res
    "DescribeSecretResponse"
    "fixture/DescribeSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecret)

responseRestoreSecret :: RestoreSecretResponse -> TestTree
responseRestoreSecret =
  res
    "RestoreSecretResponse"
    "fixture/RestoreSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreSecret)

responseCancelRotateSecret :: CancelRotateSecretResponse -> TestTree
responseCancelRotateSecret =
  res
    "CancelRotateSecretResponse"
    "fixture/CancelRotateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelRotateSecret)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responsePutSecretValue :: PutSecretValueResponse -> TestTree
responsePutSecretValue =
  res
    "PutSecretValueResponse"
    "fixture/PutSecretValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSecretValue)

responseReplicateSecretToRegions :: ReplicateSecretToRegionsResponse -> TestTree
responseReplicateSecretToRegions =
  res
    "ReplicateSecretToRegionsResponse"
    "fixture/ReplicateSecretToRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplicateSecretToRegions)

responseStopReplicationToReplica :: StopReplicationToReplicaResponse -> TestTree
responseStopReplicationToReplica =
  res
    "StopReplicationToReplicaResponse"
    "fixture/StopReplicationToReplicaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopReplicationToReplica)

responseGetRandomPassword :: GetRandomPasswordResponse -> TestTree
responseGetRandomPassword =
  res
    "GetRandomPasswordResponse"
    "fixture/GetRandomPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRandomPassword)

responseListSecretVersionIds :: ListSecretVersionIdsResponse -> TestTree
responseListSecretVersionIds =
  res
    "ListSecretVersionIdsResponse"
    "fixture/ListSecretVersionIdsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecretVersionIds)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateSecretVersionStage :: UpdateSecretVersionStageResponse -> TestTree
responseUpdateSecretVersionStage =
  res
    "UpdateSecretVersionStageResponse"
    "fixture/UpdateSecretVersionStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecretVersionStage)
