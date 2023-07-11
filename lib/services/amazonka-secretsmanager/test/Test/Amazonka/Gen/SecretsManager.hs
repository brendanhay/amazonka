{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SecretsManager
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCancelRotateSecret $
--             newCancelRotateSecret
--
--         , requestCreateSecret $
--             newCreateSecret
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteSecret $
--             newDeleteSecret
--
--         , requestDescribeSecret $
--             newDescribeSecret
--
--         , requestGetRandomPassword $
--             newGetRandomPassword
--
--         , requestGetResourcePolicy $
--             newGetResourcePolicy
--
--         , requestGetSecretValue $
--             newGetSecretValue
--
--         , requestListSecretVersionIds $
--             newListSecretVersionIds
--
--         , requestListSecrets $
--             newListSecrets
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestPutSecretValue $
--             newPutSecretValue
--
--         , requestRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplication
--
--         , requestReplicateSecretToRegions $
--             newReplicateSecretToRegions
--
--         , requestRestoreSecret $
--             newRestoreSecret
--
--         , requestRotateSecret $
--             newRotateSecret
--
--         , requestStopReplicationToReplica $
--             newStopReplicationToReplica
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateSecret $
--             newUpdateSecret
--
--         , requestUpdateSecretVersionStage $
--             newUpdateSecretVersionStage
--
--         , requestValidateResourcePolicy $
--             newValidateResourcePolicy
--
--           ]

--     , testGroup "response"
--         [ responseCancelRotateSecret $
--             newCancelRotateSecretResponse
--
--         , responseCreateSecret $
--             newCreateSecretResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteSecret $
--             newDeleteSecretResponse
--
--         , responseDescribeSecret $
--             newDescribeSecretResponse
--
--         , responseGetRandomPassword $
--             newGetRandomPasswordResponse
--
--         , responseGetResourcePolicy $
--             newGetResourcePolicyResponse
--
--         , responseGetSecretValue $
--             newGetSecretValueResponse
--
--         , responseListSecretVersionIds $
--             newListSecretVersionIdsResponse
--
--         , responseListSecrets $
--             newListSecretsResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responsePutSecretValue $
--             newPutSecretValueResponse
--
--         , responseRemoveRegionsFromReplication $
--             newRemoveRegionsFromReplicationResponse
--
--         , responseReplicateSecretToRegions $
--             newReplicateSecretToRegionsResponse
--
--         , responseRestoreSecret $
--             newRestoreSecretResponse
--
--         , responseRotateSecret $
--             newRotateSecretResponse
--
--         , responseStopReplicationToReplica $
--             newStopReplicationToReplicaResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateSecret $
--             newUpdateSecretResponse
--
--         , responseUpdateSecretVersionStage $
--             newUpdateSecretVersionStageResponse
--
--         , responseValidateResourcePolicy $
--             newValidateResourcePolicyResponse
--
--           ]
--     ]

-- Requests

requestCancelRotateSecret :: CancelRotateSecret -> TestTree
requestCancelRotateSecret =
  req
    "CancelRotateSecret"
    "fixture/CancelRotateSecret.yaml"

requestCreateSecret :: CreateSecret -> TestTree
requestCreateSecret =
  req
    "CreateSecret"
    "fixture/CreateSecret.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteSecret :: DeleteSecret -> TestTree
requestDeleteSecret =
  req
    "DeleteSecret"
    "fixture/DeleteSecret.yaml"

requestDescribeSecret :: DescribeSecret -> TestTree
requestDescribeSecret =
  req
    "DescribeSecret"
    "fixture/DescribeSecret.yaml"

requestGetRandomPassword :: GetRandomPassword -> TestTree
requestGetRandomPassword =
  req
    "GetRandomPassword"
    "fixture/GetRandomPassword.yaml"

requestGetResourcePolicy :: GetResourcePolicy -> TestTree
requestGetResourcePolicy =
  req
    "GetResourcePolicy"
    "fixture/GetResourcePolicy.yaml"

requestGetSecretValue :: GetSecretValue -> TestTree
requestGetSecretValue =
  req
    "GetSecretValue"
    "fixture/GetSecretValue.yaml"

requestListSecretVersionIds :: ListSecretVersionIds -> TestTree
requestListSecretVersionIds =
  req
    "ListSecretVersionIds"
    "fixture/ListSecretVersionIds.yaml"

requestListSecrets :: ListSecrets -> TestTree
requestListSecrets =
  req
    "ListSecrets"
    "fixture/ListSecrets.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestPutSecretValue :: PutSecretValue -> TestTree
requestPutSecretValue =
  req
    "PutSecretValue"
    "fixture/PutSecretValue.yaml"

requestRemoveRegionsFromReplication :: RemoveRegionsFromReplication -> TestTree
requestRemoveRegionsFromReplication =
  req
    "RemoveRegionsFromReplication"
    "fixture/RemoveRegionsFromReplication.yaml"

requestReplicateSecretToRegions :: ReplicateSecretToRegions -> TestTree
requestReplicateSecretToRegions =
  req
    "ReplicateSecretToRegions"
    "fixture/ReplicateSecretToRegions.yaml"

requestRestoreSecret :: RestoreSecret -> TestTree
requestRestoreSecret =
  req
    "RestoreSecret"
    "fixture/RestoreSecret.yaml"

requestRotateSecret :: RotateSecret -> TestTree
requestRotateSecret =
  req
    "RotateSecret"
    "fixture/RotateSecret.yaml"

requestStopReplicationToReplica :: StopReplicationToReplica -> TestTree
requestStopReplicationToReplica =
  req
    "StopReplicationToReplica"
    "fixture/StopReplicationToReplica.yaml"

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

requestUpdateSecret :: UpdateSecret -> TestTree
requestUpdateSecret =
  req
    "UpdateSecret"
    "fixture/UpdateSecret.yaml"

requestUpdateSecretVersionStage :: UpdateSecretVersionStage -> TestTree
requestUpdateSecretVersionStage =
  req
    "UpdateSecretVersionStage"
    "fixture/UpdateSecretVersionStage.yaml"

requestValidateResourcePolicy :: ValidateResourcePolicy -> TestTree
requestValidateResourcePolicy =
  req
    "ValidateResourcePolicy"
    "fixture/ValidateResourcePolicy.yaml"

-- Responses

responseCancelRotateSecret :: CancelRotateSecretResponse -> TestTree
responseCancelRotateSecret =
  res
    "CancelRotateSecretResponse"
    "fixture/CancelRotateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelRotateSecret)

responseCreateSecret :: CreateSecretResponse -> TestTree
responseCreateSecret =
  res
    "CreateSecretResponse"
    "fixture/CreateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSecret)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteSecret :: DeleteSecretResponse -> TestTree
responseDeleteSecret =
  res
    "DeleteSecretResponse"
    "fixture/DeleteSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSecret)

responseDescribeSecret :: DescribeSecretResponse -> TestTree
responseDescribeSecret =
  res
    "DescribeSecretResponse"
    "fixture/DescribeSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSecret)

responseGetRandomPassword :: GetRandomPasswordResponse -> TestTree
responseGetRandomPassword =
  res
    "GetRandomPasswordResponse"
    "fixture/GetRandomPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRandomPassword)

responseGetResourcePolicy :: GetResourcePolicyResponse -> TestTree
responseGetResourcePolicy =
  res
    "GetResourcePolicyResponse"
    "fixture/GetResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicy)

responseGetSecretValue :: GetSecretValueResponse -> TestTree
responseGetSecretValue =
  res
    "GetSecretValueResponse"
    "fixture/GetSecretValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSecretValue)

responseListSecretVersionIds :: ListSecretVersionIdsResponse -> TestTree
responseListSecretVersionIds =
  res
    "ListSecretVersionIdsResponse"
    "fixture/ListSecretVersionIdsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecretVersionIds)

responseListSecrets :: ListSecretsResponse -> TestTree
responseListSecrets =
  res
    "ListSecretsResponse"
    "fixture/ListSecretsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSecrets)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responsePutSecretValue :: PutSecretValueResponse -> TestTree
responsePutSecretValue =
  res
    "PutSecretValueResponse"
    "fixture/PutSecretValueResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSecretValue)

responseRemoveRegionsFromReplication :: RemoveRegionsFromReplicationResponse -> TestTree
responseRemoveRegionsFromReplication =
  res
    "RemoveRegionsFromReplicationResponse"
    "fixture/RemoveRegionsFromReplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveRegionsFromReplication)

responseReplicateSecretToRegions :: ReplicateSecretToRegionsResponse -> TestTree
responseReplicateSecretToRegions =
  res
    "ReplicateSecretToRegionsResponse"
    "fixture/ReplicateSecretToRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReplicateSecretToRegions)

responseRestoreSecret :: RestoreSecretResponse -> TestTree
responseRestoreSecret =
  res
    "RestoreSecretResponse"
    "fixture/RestoreSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestoreSecret)

responseRotateSecret :: RotateSecretResponse -> TestTree
responseRotateSecret =
  res
    "RotateSecretResponse"
    "fixture/RotateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RotateSecret)

responseStopReplicationToReplica :: StopReplicationToReplicaResponse -> TestTree
responseStopReplicationToReplica =
  res
    "StopReplicationToReplicaResponse"
    "fixture/StopReplicationToReplicaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopReplicationToReplica)

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

responseUpdateSecret :: UpdateSecretResponse -> TestTree
responseUpdateSecret =
  res
    "UpdateSecretResponse"
    "fixture/UpdateSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecret)

responseUpdateSecretVersionStage :: UpdateSecretVersionStageResponse -> TestTree
responseUpdateSecretVersionStage =
  res
    "UpdateSecretVersionStageResponse"
    "fixture/UpdateSecretVersionStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecretVersionStage)

responseValidateResourcePolicy :: ValidateResourcePolicyResponse -> TestTree
responseValidateResourcePolicy =
  res
    "ValidateResourcePolicyResponse"
    "fixture/ValidateResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateResourcePolicy)
