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
--         [ requestAssociateLicense $
--             newAssociateLicense
--
--         , requestCreateWorkspace $
--             newCreateWorkspace
--
--         , requestDeleteWorkspace $
--             newDeleteWorkspace
--
--         , requestDescribeWorkspace $
--             newDescribeWorkspace
--
--         , requestDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthentication
--
--         , requestDisassociateLicense $
--             newDisassociateLicense
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListWorkspaces $
--             newListWorkspaces
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
--           ]

--     , testGroup "response"
--         [ responseAssociateLicense $
--             newAssociateLicenseResponse
--
--         , responseCreateWorkspace $
--             newCreateWorkspaceResponse
--
--         , responseDeleteWorkspace $
--             newDeleteWorkspaceResponse
--
--         , responseDescribeWorkspace $
--             newDescribeWorkspaceResponse
--
--         , responseDescribeWorkspaceAuthentication $
--             newDescribeWorkspaceAuthenticationResponse
--
--         , responseDisassociateLicense $
--             newDisassociateLicenseResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListWorkspaces $
--             newListWorkspacesResponse
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

requestDeleteWorkspace :: DeleteWorkspace -> TestTree
requestDeleteWorkspace =
  req
    "DeleteWorkspace"
    "fixture/DeleteWorkspace.yaml"

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

requestListWorkspaces :: ListWorkspaces -> TestTree
requestListWorkspaces =
  req
    "ListWorkspaces"
    "fixture/ListWorkspaces.yaml"

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

responseDeleteWorkspace :: DeleteWorkspaceResponse -> TestTree
responseDeleteWorkspace =
  res
    "DeleteWorkspaceResponse"
    "fixture/DeleteWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspace)

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

responseListWorkspaces :: ListWorkspacesResponse -> TestTree
responseListWorkspaces =
  res
    "ListWorkspacesResponse"
    "fixture/ListWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkspaces)

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
