{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Grafana
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListWorkspaces $
--             newListWorkspaces
--
--         , requestDeleteWorkspace $
--             newDeleteWorkspace
--
--         , requestUpdateWorkspace $
--             newUpdateWorkspace
--
--         , requestUpdateWorkspaceAuthentication $
--             newUpdateWorkspaceAuthentication
--
--         , requestDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthentication
--
--         , requestDescribeWorkspace $
--             newDescribeWorkspace
--
--         , requestAssociateLicense $
--             newAssociateLicense
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestUpdatePermissions $
--             newUpdatePermissions
--
--         , requestDisassociateLicense $
--             newDisassociateLicense
--
--         , requestCreateWorkspace $
--             newCreateWorkspace
--
--           ]

--     , testGroup "response"
--         [ responseListWorkspaces $
--             newListWorkspacesResponse
--
--         , responseDeleteWorkspace $
--             newDeleteWorkspaceResponse
--
--         , responseUpdateWorkspace $
--             newUpdateWorkspaceResponse
--
--         , responseUpdateWorkspaceAuthentication $
--             newUpdateWorkspaceAuthenticationResponse
--
--         , responseDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthenticationResponse
--
--         , responseDescribeWorkspace $
--             newDescribeWorkspaceResponse
--
--         , responseAssociateLicense $
--             newAssociateLicenseResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseUpdatePermissions $
--             newUpdatePermissionsResponse
--
--         , responseDisassociateLicense $
--             newDisassociateLicenseResponse
--
--         , responseCreateWorkspace $
--             newCreateWorkspaceResponse
--
--           ]
--     ]

-- Requests

requestListWorkspaces :: ListWorkspaces -> TestTree
requestListWorkspaces =
  req
    "ListWorkspaces"
    "fixture/ListWorkspaces.yaml"

requestDeleteWorkspace :: DeleteWorkspace -> TestTree
requestDeleteWorkspace =
  req
    "DeleteWorkspace"
    "fixture/DeleteWorkspace.yaml"

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

requestDescribeWorkspaceAuthentication :: DescribeWorkspaceAuthentication -> TestTree
requestDescribeWorkspaceAuthentication =
  req
    "DescribeWorkspaceAuthentication"
    "fixture/DescribeWorkspaceAuthentication.yaml"

requestDescribeWorkspace :: DescribeWorkspace -> TestTree
requestDescribeWorkspace =
  req
    "DescribeWorkspace"
    "fixture/DescribeWorkspace.yaml"

requestAssociateLicense :: AssociateLicense -> TestTree
requestAssociateLicense =
  req
    "AssociateLicense"
    "fixture/AssociateLicense.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestUpdatePermissions :: UpdatePermissions -> TestTree
requestUpdatePermissions =
  req
    "UpdatePermissions"
    "fixture/UpdatePermissions.yaml"

requestDisassociateLicense :: DisassociateLicense -> TestTree
requestDisassociateLicense =
  req
    "DisassociateLicense"
    "fixture/DisassociateLicense.yaml"

requestCreateWorkspace :: CreateWorkspace -> TestTree
requestCreateWorkspace =
  req
    "CreateWorkspace"
    "fixture/CreateWorkspace.yaml"

-- Responses

responseListWorkspaces :: ListWorkspacesResponse -> TestTree
responseListWorkspaces =
  res
    "ListWorkspacesResponse"
    "fixture/ListWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkspaces)

responseDeleteWorkspace :: DeleteWorkspaceResponse -> TestTree
responseDeleteWorkspace =
  res
    "DeleteWorkspaceResponse"
    "fixture/DeleteWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspace)

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

responseDescribeWorkspaceAuthentication :: DescribeWorkspaceAuthenticationResponse -> TestTree
responseDescribeWorkspaceAuthentication =
  res
    "DescribeWorkspaceAuthenticationResponse"
    "fixture/DescribeWorkspaceAuthenticationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspaceAuthentication)

responseDescribeWorkspace :: DescribeWorkspaceResponse -> TestTree
responseDescribeWorkspace =
  res
    "DescribeWorkspaceResponse"
    "fixture/DescribeWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspace)

responseAssociateLicense :: AssociateLicenseResponse -> TestTree
responseAssociateLicense =
  res
    "AssociateLicenseResponse"
    "fixture/AssociateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateLicense)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseUpdatePermissions :: UpdatePermissionsResponse -> TestTree
responseUpdatePermissions =
  res
    "UpdatePermissionsResponse"
    "fixture/UpdatePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePermissions)

responseDisassociateLicense :: DisassociateLicenseResponse -> TestTree
responseDisassociateLicense =
  res
    "DisassociateLicenseResponse"
    "fixture/DisassociateLicenseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateLicense)

responseCreateWorkspace :: CreateWorkspaceResponse -> TestTree
responseCreateWorkspace =
  res
    "CreateWorkspaceResponse"
    "fixture/CreateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspace)
