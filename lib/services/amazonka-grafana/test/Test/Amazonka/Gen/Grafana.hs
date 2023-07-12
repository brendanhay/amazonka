{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Grafana
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Grafana where

import Amazonka.Grafana
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Grafana.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateLicense $
--             newAssociateLicense
--
--         , requestCreateWorkspace $
--             newCreateWorkspace
--
--         , requestCreateWorkspaceApiKey $
--             newCreateWorkspaceApiKey
--
--         , requestDeleteWorkspace $
--             newDeleteWorkspace
--
--         , requestDeleteWorkspaceApiKey $
--             newDeleteWorkspaceApiKey
--
--         , requestDescribeWorkspace $
--             newDescribeWorkspace
--
--         , requestDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthentication
--
--         , requestDescribeWorkspaceConfiguration $
--             newDescribeWorkspaceConfiguration
--
--         , requestDisassociateLicense $
--             newDisassociateLicense
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkspaces $
--             newListWorkspaces
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePermissions $
--             newUpdatePermissions
--
--         , requestUpdateWorkspace $
--             newUpdateWorkspace
--
--         , requestUpdateWorkspaceAuthentication $
--             newUpdateWorkspaceAuthentication
--
--         , requestUpdateWorkspaceConfiguration $
--             newUpdateWorkspaceConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAssociateLicense $
--             newAssociateLicenseResponse
--
--         , responseCreateWorkspace $
--             newCreateWorkspaceResponse
--
--         , responseCreateWorkspaceApiKey $
--             newCreateWorkspaceApiKeyResponse
--
--         , responseDeleteWorkspace $
--             newDeleteWorkspaceResponse
--
--         , responseDeleteWorkspaceApiKey $
--             newDeleteWorkspaceApiKeyResponse
--
--         , responseDescribeWorkspace $
--             newDescribeWorkspaceResponse
--
--         , responseDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthenticationResponse
--
--         , responseDescribeWorkspaceConfiguration $
--             newDescribeWorkspaceConfigurationResponse
--
--         , responseDisassociateLicense $
--             newDisassociateLicenseResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkspaces $
--             newListWorkspacesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePermissions $
--             newUpdatePermissionsResponse
--
--         , responseUpdateWorkspace $
--             newUpdateWorkspaceResponse
--
--         , responseUpdateWorkspaceAuthentication $
--             newUpdateWorkspaceAuthenticationResponse
--
--         , responseUpdateWorkspaceConfiguration $
--             newUpdateWorkspaceConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAssociateLicense :: AssociateLicense -> TestTree
requestAssociateLicense =
  req
    "AssociateLicense"
    "fixture/AssociateLicense.yaml"

requestCreateWorkspace :: CreateWorkspace -> TestTree
requestCreateWorkspace =
  req
    "CreateWorkspace"
    "fixture/CreateWorkspace.yaml"

requestCreateWorkspaceApiKey :: CreateWorkspaceApiKey -> TestTree
requestCreateWorkspaceApiKey =
  req
    "CreateWorkspaceApiKey"
    "fixture/CreateWorkspaceApiKey.yaml"

requestDeleteWorkspace :: DeleteWorkspace -> TestTree
requestDeleteWorkspace =
  req
    "DeleteWorkspace"
    "fixture/DeleteWorkspace.yaml"

requestDeleteWorkspaceApiKey :: DeleteWorkspaceApiKey -> TestTree
requestDeleteWorkspaceApiKey =
  req
    "DeleteWorkspaceApiKey"
    "fixture/DeleteWorkspaceApiKey.yaml"

requestDescribeWorkspace :: DescribeWorkspace -> TestTree
requestDescribeWorkspace =
  req
    "DescribeWorkspace"
    "fixture/DescribeWorkspace.yaml"

requestDescribeWorkspaceAuthentication :: DescribeWorkspaceAuthentication -> TestTree
requestDescribeWorkspaceAuthentication =
  req
    "DescribeWorkspaceAuthentication"
    "fixture/DescribeWorkspaceAuthentication.yaml"

requestDescribeWorkspaceConfiguration :: DescribeWorkspaceConfiguration -> TestTree
requestDescribeWorkspaceConfiguration =
  req
    "DescribeWorkspaceConfiguration"
    "fixture/DescribeWorkspaceConfiguration.yaml"

requestDisassociateLicense :: DisassociateLicense -> TestTree
requestDisassociateLicense =
  req
    "DisassociateLicense"
    "fixture/DisassociateLicense.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkspaces :: ListWorkspaces -> TestTree
requestListWorkspaces =
  req
    "ListWorkspaces"
    "fixture/ListWorkspaces.yaml"

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

requestUpdatePermissions :: UpdatePermissions -> TestTree
requestUpdatePermissions =
  req
    "UpdatePermissions"
    "fixture/UpdatePermissions.yaml"

requestUpdateWorkspace :: UpdateWorkspace -> TestTree
requestUpdateWorkspace =
  req
    "UpdateWorkspace"
    "fixture/UpdateWorkspace.yaml"

requestUpdateWorkspaceAuthentication :: UpdateWorkspaceAuthentication -> TestTree
requestUpdateWorkspaceAuthentication =
  req
    "UpdateWorkspaceAuthentication"
    "fixture/UpdateWorkspaceAuthentication.yaml"

requestUpdateWorkspaceConfiguration :: UpdateWorkspaceConfiguration -> TestTree
requestUpdateWorkspaceConfiguration =
  req
    "UpdateWorkspaceConfiguration"
    "fixture/UpdateWorkspaceConfiguration.yaml"

-- Responses

responseAssociateLicense :: AssociateLicenseResponse -> TestTree
responseAssociateLicense =
  res
    "AssociateLicenseResponse"
    "fixture/AssociateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLicense)

responseCreateWorkspace :: CreateWorkspaceResponse -> TestTree
responseCreateWorkspace =
  res
    "CreateWorkspaceResponse"
    "fixture/CreateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspace)

responseCreateWorkspaceApiKey :: CreateWorkspaceApiKeyResponse -> TestTree
responseCreateWorkspaceApiKey =
  res
    "CreateWorkspaceApiKeyResponse"
    "fixture/CreateWorkspaceApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspaceApiKey)

responseDeleteWorkspace :: DeleteWorkspaceResponse -> TestTree
responseDeleteWorkspace =
  res
    "DeleteWorkspaceResponse"
    "fixture/DeleteWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspace)

responseDeleteWorkspaceApiKey :: DeleteWorkspaceApiKeyResponse -> TestTree
responseDeleteWorkspaceApiKey =
  res
    "DeleteWorkspaceApiKeyResponse"
    "fixture/DeleteWorkspaceApiKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspaceApiKey)

responseDescribeWorkspace :: DescribeWorkspaceResponse -> TestTree
responseDescribeWorkspace =
  res
    "DescribeWorkspaceResponse"
    "fixture/DescribeWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspace)

responseDescribeWorkspaceAuthentication :: DescribeWorkspaceAuthenticationResponse -> TestTree
responseDescribeWorkspaceAuthentication =
  res
    "DescribeWorkspaceAuthenticationResponse"
    "fixture/DescribeWorkspaceAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceAuthentication)

responseDescribeWorkspaceConfiguration :: DescribeWorkspaceConfigurationResponse -> TestTree
responseDescribeWorkspaceConfiguration =
  res
    "DescribeWorkspaceConfigurationResponse"
    "fixture/DescribeWorkspaceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceConfiguration)

responseDisassociateLicense :: DisassociateLicenseResponse -> TestTree
responseDisassociateLicense =
  res
    "DisassociateLicenseResponse"
    "fixture/DisassociateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLicense)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkspaces :: ListWorkspacesResponse -> TestTree
responseListWorkspaces =
  res
    "ListWorkspacesResponse"
    "fixture/ListWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkspaces)

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

responseUpdatePermissions :: UpdatePermissionsResponse -> TestTree
responseUpdatePermissions =
  res
    "UpdatePermissionsResponse"
    "fixture/UpdatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePermissions)

responseUpdateWorkspace :: UpdateWorkspaceResponse -> TestTree
responseUpdateWorkspace =
  res
    "UpdateWorkspaceResponse"
    "fixture/UpdateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspace)

responseUpdateWorkspaceAuthentication :: UpdateWorkspaceAuthenticationResponse -> TestTree
responseUpdateWorkspaceAuthentication =
  res
    "UpdateWorkspaceAuthenticationResponse"
    "fixture/UpdateWorkspaceAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceAuthentication)

responseUpdateWorkspaceConfiguration :: UpdateWorkspaceConfigurationResponse -> TestTree
responseUpdateWorkspaceConfiguration =
  res
    "UpdateWorkspaceConfigurationResponse"
    "fixture/UpdateWorkspaceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceConfiguration)
