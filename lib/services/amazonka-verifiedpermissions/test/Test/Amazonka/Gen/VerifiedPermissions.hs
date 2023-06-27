{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.VerifiedPermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.VerifiedPermissions where

import Amazonka.VerifiedPermissions
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.VerifiedPermissions.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateIdentitySource $
--             newCreateIdentitySource
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestCreatePolicyStore $
--             newCreatePolicyStore
--
--         , requestCreatePolicyTemplate $
--             newCreatePolicyTemplate
--
--         , requestDeleteIdentitySource $
--             newDeleteIdentitySource
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeletePolicyStore $
--             newDeletePolicyStore
--
--         , requestDeletePolicyTemplate $
--             newDeletePolicyTemplate
--
--         , requestGetIdentitySource $
--             newGetIdentitySource
--
--         , requestGetPolicy $
--             newGetPolicy
--
--         , requestGetPolicyStore $
--             newGetPolicyStore
--
--         , requestGetPolicyTemplate $
--             newGetPolicyTemplate
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestIsAuthorized $
--             newIsAuthorized
--
--         , requestIsAuthorizedWithToken $
--             newIsAuthorizedWithToken
--
--         , requestListIdentitySources $
--             newListIdentitySources
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListPolicyStores $
--             newListPolicyStores
--
--         , requestListPolicyTemplates $
--             newListPolicyTemplates
--
--         , requestPutSchema $
--             newPutSchema
--
--         , requestUpdateIdentitySource $
--             newUpdateIdentitySource
--
--         , requestUpdatePolicy $
--             newUpdatePolicy
--
--         , requestUpdatePolicyStore $
--             newUpdatePolicyStore
--
--         , requestUpdatePolicyTemplate $
--             newUpdatePolicyTemplate
--
--           ]

--     , testGroup "response"
--         [ responseCreateIdentitySource $
--             newCreateIdentitySourceResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseCreatePolicyStore $
--             newCreatePolicyStoreResponse
--
--         , responseCreatePolicyTemplate $
--             newCreatePolicyTemplateResponse
--
--         , responseDeleteIdentitySource $
--             newDeleteIdentitySourceResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeletePolicyStore $
--             newDeletePolicyStoreResponse
--
--         , responseDeletePolicyTemplate $
--             newDeletePolicyTemplateResponse
--
--         , responseGetIdentitySource $
--             newGetIdentitySourceResponse
--
--         , responseGetPolicy $
--             newGetPolicyResponse
--
--         , responseGetPolicyStore $
--             newGetPolicyStoreResponse
--
--         , responseGetPolicyTemplate $
--             newGetPolicyTemplateResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseIsAuthorized $
--             newIsAuthorizedResponse
--
--         , responseIsAuthorizedWithToken $
--             newIsAuthorizedWithTokenResponse
--
--         , responseListIdentitySources $
--             newListIdentitySourcesResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListPolicyStores $
--             newListPolicyStoresResponse
--
--         , responseListPolicyTemplates $
--             newListPolicyTemplatesResponse
--
--         , responsePutSchema $
--             newPutSchemaResponse
--
--         , responseUpdateIdentitySource $
--             newUpdateIdentitySourceResponse
--
--         , responseUpdatePolicy $
--             newUpdatePolicyResponse
--
--         , responseUpdatePolicyStore $
--             newUpdatePolicyStoreResponse
--
--         , responseUpdatePolicyTemplate $
--             newUpdatePolicyTemplateResponse
--
--           ]
--     ]

-- Requests

requestCreateIdentitySource :: CreateIdentitySource -> TestTree
requestCreateIdentitySource =
  req
    "CreateIdentitySource"
    "fixture/CreateIdentitySource.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestCreatePolicyStore :: CreatePolicyStore -> TestTree
requestCreatePolicyStore =
  req
    "CreatePolicyStore"
    "fixture/CreatePolicyStore.yaml"

requestCreatePolicyTemplate :: CreatePolicyTemplate -> TestTree
requestCreatePolicyTemplate =
  req
    "CreatePolicyTemplate"
    "fixture/CreatePolicyTemplate.yaml"

requestDeleteIdentitySource :: DeleteIdentitySource -> TestTree
requestDeleteIdentitySource =
  req
    "DeleteIdentitySource"
    "fixture/DeleteIdentitySource.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeletePolicyStore :: DeletePolicyStore -> TestTree
requestDeletePolicyStore =
  req
    "DeletePolicyStore"
    "fixture/DeletePolicyStore.yaml"

requestDeletePolicyTemplate :: DeletePolicyTemplate -> TestTree
requestDeletePolicyTemplate =
  req
    "DeletePolicyTemplate"
    "fixture/DeletePolicyTemplate.yaml"

requestGetIdentitySource :: GetIdentitySource -> TestTree
requestGetIdentitySource =
  req
    "GetIdentitySource"
    "fixture/GetIdentitySource.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy =
  req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetPolicyStore :: GetPolicyStore -> TestTree
requestGetPolicyStore =
  req
    "GetPolicyStore"
    "fixture/GetPolicyStore.yaml"

requestGetPolicyTemplate :: GetPolicyTemplate -> TestTree
requestGetPolicyTemplate =
  req
    "GetPolicyTemplate"
    "fixture/GetPolicyTemplate.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

requestIsAuthorized :: IsAuthorized -> TestTree
requestIsAuthorized =
  req
    "IsAuthorized"
    "fixture/IsAuthorized.yaml"

requestIsAuthorizedWithToken :: IsAuthorizedWithToken -> TestTree
requestIsAuthorizedWithToken =
  req
    "IsAuthorizedWithToken"
    "fixture/IsAuthorizedWithToken.yaml"

requestListIdentitySources :: ListIdentitySources -> TestTree
requestListIdentitySources =
  req
    "ListIdentitySources"
    "fixture/ListIdentitySources.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListPolicyStores :: ListPolicyStores -> TestTree
requestListPolicyStores =
  req
    "ListPolicyStores"
    "fixture/ListPolicyStores.yaml"

requestListPolicyTemplates :: ListPolicyTemplates -> TestTree
requestListPolicyTemplates =
  req
    "ListPolicyTemplates"
    "fixture/ListPolicyTemplates.yaml"

requestPutSchema :: PutSchema -> TestTree
requestPutSchema =
  req
    "PutSchema"
    "fixture/PutSchema.yaml"

requestUpdateIdentitySource :: UpdateIdentitySource -> TestTree
requestUpdateIdentitySource =
  req
    "UpdateIdentitySource"
    "fixture/UpdateIdentitySource.yaml"

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy =
  req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

requestUpdatePolicyStore :: UpdatePolicyStore -> TestTree
requestUpdatePolicyStore =
  req
    "UpdatePolicyStore"
    "fixture/UpdatePolicyStore.yaml"

requestUpdatePolicyTemplate :: UpdatePolicyTemplate -> TestTree
requestUpdatePolicyTemplate =
  req
    "UpdatePolicyTemplate"
    "fixture/UpdatePolicyTemplate.yaml"

-- Responses

responseCreateIdentitySource :: CreateIdentitySourceResponse -> TestTree
responseCreateIdentitySource =
  res
    "CreateIdentitySourceResponse"
    "fixture/CreateIdentitySourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIdentitySource)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseCreatePolicyStore :: CreatePolicyStoreResponse -> TestTree
responseCreatePolicyStore =
  res
    "CreatePolicyStoreResponse"
    "fixture/CreatePolicyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicyStore)

responseCreatePolicyTemplate :: CreatePolicyTemplateResponse -> TestTree
responseCreatePolicyTemplate =
  res
    "CreatePolicyTemplateResponse"
    "fixture/CreatePolicyTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicyTemplate)

responseDeleteIdentitySource :: DeleteIdentitySourceResponse -> TestTree
responseDeleteIdentitySource =
  res
    "DeleteIdentitySourceResponse"
    "fixture/DeleteIdentitySourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentitySource)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeletePolicyStore :: DeletePolicyStoreResponse -> TestTree
responseDeletePolicyStore =
  res
    "DeletePolicyStoreResponse"
    "fixture/DeletePolicyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicyStore)

responseDeletePolicyTemplate :: DeletePolicyTemplateResponse -> TestTree
responseDeletePolicyTemplate =
  res
    "DeletePolicyTemplateResponse"
    "fixture/DeletePolicyTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicyTemplate)

responseGetIdentitySource :: GetIdentitySourceResponse -> TestTree
responseGetIdentitySource =
  res
    "GetIdentitySourceResponse"
    "fixture/GetIdentitySourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentitySource)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy =
  res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicy)

responseGetPolicyStore :: GetPolicyStoreResponse -> TestTree
responseGetPolicyStore =
  res
    "GetPolicyStoreResponse"
    "fixture/GetPolicyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicyStore)

responseGetPolicyTemplate :: GetPolicyTemplateResponse -> TestTree
responseGetPolicyTemplate =
  res
    "GetPolicyTemplateResponse"
    "fixture/GetPolicyTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPolicyTemplate)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchema)

responseIsAuthorized :: IsAuthorizedResponse -> TestTree
responseIsAuthorized =
  res
    "IsAuthorizedResponse"
    "fixture/IsAuthorizedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IsAuthorized)

responseIsAuthorizedWithToken :: IsAuthorizedWithTokenResponse -> TestTree
responseIsAuthorizedWithToken =
  res
    "IsAuthorizedWithTokenResponse"
    "fixture/IsAuthorizedWithTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IsAuthorizedWithToken)

responseListIdentitySources :: ListIdentitySourcesResponse -> TestTree
responseListIdentitySources =
  res
    "ListIdentitySourcesResponse"
    "fixture/ListIdentitySourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentitySources)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseListPolicyStores :: ListPolicyStoresResponse -> TestTree
responseListPolicyStores =
  res
    "ListPolicyStoresResponse"
    "fixture/ListPolicyStoresResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyStores)

responseListPolicyTemplates :: ListPolicyTemplatesResponse -> TestTree
responseListPolicyTemplates =
  res
    "ListPolicyTemplatesResponse"
    "fixture/ListPolicyTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyTemplates)

responsePutSchema :: PutSchemaResponse -> TestTree
responsePutSchema =
  res
    "PutSchemaResponse"
    "fixture/PutSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSchema)

responseUpdateIdentitySource :: UpdateIdentitySourceResponse -> TestTree
responseUpdateIdentitySource =
  res
    "UpdateIdentitySourceResponse"
    "fixture/UpdateIdentitySourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentitySource)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy =
  res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePolicy)

responseUpdatePolicyStore :: UpdatePolicyStoreResponse -> TestTree
responseUpdatePolicyStore =
  res
    "UpdatePolicyStoreResponse"
    "fixture/UpdatePolicyStoreResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePolicyStore)

responseUpdatePolicyTemplate :: UpdatePolicyTemplateResponse -> TestTree
responseUpdatePolicyTemplate =
  res
    "UpdatePolicyTemplateResponse"
    "fixture/UpdatePolicyTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePolicyTemplate)
