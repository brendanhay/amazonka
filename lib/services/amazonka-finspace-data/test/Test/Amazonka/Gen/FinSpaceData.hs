{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FinSpaceData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FinSpaceData where

import Amazonka.FinSpaceData
import qualified Data.Proxy as Proxy
import Test.Amazonka.FinSpaceData.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateUserToPermissionGroup $
--             newAssociateUserToPermissionGroup
--
--         , requestCreateChangeset $
--             newCreateChangeset
--
--         , requestCreateDataView $
--             newCreateDataView
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestCreatePermissionGroup $
--             newCreatePermissionGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeletePermissionGroup $
--             newDeletePermissionGroup
--
--         , requestDisableUser $
--             newDisableUser
--
--         , requestDisassociateUserFromPermissionGroup $
--             newDisassociateUserFromPermissionGroup
--
--         , requestEnableUser $
--             newEnableUser
--
--         , requestGetChangeset $
--             newGetChangeset
--
--         , requestGetDataView $
--             newGetDataView
--
--         , requestGetDataset $
--             newGetDataset
--
--         , requestGetExternalDataViewAccessDetails $
--             newGetExternalDataViewAccessDetails
--
--         , requestGetPermissionGroup $
--             newGetPermissionGroup
--
--         , requestGetProgrammaticAccessCredentials $
--             newGetProgrammaticAccessCredentials
--
--         , requestGetUser $
--             newGetUser
--
--         , requestGetWorkingLocation $
--             newGetWorkingLocation
--
--         , requestListChangesets $
--             newListChangesets
--
--         , requestListDataViews $
--             newListDataViews
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListPermissionGroups $
--             newListPermissionGroups
--
--         , requestListPermissionGroupsByUser $
--             newListPermissionGroupsByUser
--
--         , requestListUsers $
--             newListUsers
--
--         , requestListUsersByPermissionGroup $
--             newListUsersByPermissionGroup
--
--         , requestResetUserPassword $
--             newResetUserPassword
--
--         , requestUpdateChangeset $
--             newUpdateChangeset
--
--         , requestUpdateDataset $
--             newUpdateDataset
--
--         , requestUpdatePermissionGroup $
--             newUpdatePermissionGroup
--
--         , requestUpdateUser $
--             newUpdateUser
--
--           ]

--     , testGroup "response"
--         [ responseAssociateUserToPermissionGroup $
--             newAssociateUserToPermissionGroupResponse
--
--         , responseCreateChangeset $
--             newCreateChangesetResponse
--
--         , responseCreateDataView $
--             newCreateDataViewResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreatePermissionGroup $
--             newCreatePermissionGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeletePermissionGroup $
--             newDeletePermissionGroupResponse
--
--         , responseDisableUser $
--             newDisableUserResponse
--
--         , responseDisassociateUserFromPermissionGroup $
--             newDisassociateUserFromPermissionGroupResponse
--
--         , responseEnableUser $
--             newEnableUserResponse
--
--         , responseGetChangeset $
--             newGetChangesetResponse
--
--         , responseGetDataView $
--             newGetDataViewResponse
--
--         , responseGetDataset $
--             newGetDatasetResponse
--
--         , responseGetExternalDataViewAccessDetails $
--             newGetExternalDataViewAccessDetailsResponse
--
--         , responseGetPermissionGroup $
--             newGetPermissionGroupResponse
--
--         , responseGetProgrammaticAccessCredentials $
--             newGetProgrammaticAccessCredentialsResponse
--
--         , responseGetUser $
--             newGetUserResponse
--
--         , responseGetWorkingLocation $
--             newGetWorkingLocationResponse
--
--         , responseListChangesets $
--             newListChangesetsResponse
--
--         , responseListDataViews $
--             newListDataViewsResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListPermissionGroups $
--             newListPermissionGroupsResponse
--
--         , responseListPermissionGroupsByUser $
--             newListPermissionGroupsByUserResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseListUsersByPermissionGroup $
--             newListUsersByPermissionGroupResponse
--
--         , responseResetUserPassword $
--             newResetUserPasswordResponse
--
--         , responseUpdateChangeset $
--             newUpdateChangesetResponse
--
--         , responseUpdateDataset $
--             newUpdateDatasetResponse
--
--         , responseUpdatePermissionGroup $
--             newUpdatePermissionGroupResponse
--
--         , responseUpdateUser $
--             newUpdateUserResponse
--
--           ]
--     ]

-- Requests

requestAssociateUserToPermissionGroup :: AssociateUserToPermissionGroup -> TestTree
requestAssociateUserToPermissionGroup =
  req
    "AssociateUserToPermissionGroup"
    "fixture/AssociateUserToPermissionGroup.yaml"

requestCreateChangeset :: CreateChangeset -> TestTree
requestCreateChangeset =
  req
    "CreateChangeset"
    "fixture/CreateChangeset.yaml"

requestCreateDataView :: CreateDataView -> TestTree
requestCreateDataView =
  req
    "CreateDataView"
    "fixture/CreateDataView.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreatePermissionGroup :: CreatePermissionGroup -> TestTree
requestCreatePermissionGroup =
  req
    "CreatePermissionGroup"
    "fixture/CreatePermissionGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeletePermissionGroup :: DeletePermissionGroup -> TestTree
requestDeletePermissionGroup =
  req
    "DeletePermissionGroup"
    "fixture/DeletePermissionGroup.yaml"

requestDisableUser :: DisableUser -> TestTree
requestDisableUser =
  req
    "DisableUser"
    "fixture/DisableUser.yaml"

requestDisassociateUserFromPermissionGroup :: DisassociateUserFromPermissionGroup -> TestTree
requestDisassociateUserFromPermissionGroup =
  req
    "DisassociateUserFromPermissionGroup"
    "fixture/DisassociateUserFromPermissionGroup.yaml"

requestEnableUser :: EnableUser -> TestTree
requestEnableUser =
  req
    "EnableUser"
    "fixture/EnableUser.yaml"

requestGetChangeset :: GetChangeset -> TestTree
requestGetChangeset =
  req
    "GetChangeset"
    "fixture/GetChangeset.yaml"

requestGetDataView :: GetDataView -> TestTree
requestGetDataView =
  req
    "GetDataView"
    "fixture/GetDataView.yaml"

requestGetDataset :: GetDataset -> TestTree
requestGetDataset =
  req
    "GetDataset"
    "fixture/GetDataset.yaml"

requestGetExternalDataViewAccessDetails :: GetExternalDataViewAccessDetails -> TestTree
requestGetExternalDataViewAccessDetails =
  req
    "GetExternalDataViewAccessDetails"
    "fixture/GetExternalDataViewAccessDetails.yaml"

requestGetPermissionGroup :: GetPermissionGroup -> TestTree
requestGetPermissionGroup =
  req
    "GetPermissionGroup"
    "fixture/GetPermissionGroup.yaml"

requestGetProgrammaticAccessCredentials :: GetProgrammaticAccessCredentials -> TestTree
requestGetProgrammaticAccessCredentials =
  req
    "GetProgrammaticAccessCredentials"
    "fixture/GetProgrammaticAccessCredentials.yaml"

requestGetUser :: GetUser -> TestTree
requestGetUser =
  req
    "GetUser"
    "fixture/GetUser.yaml"

requestGetWorkingLocation :: GetWorkingLocation -> TestTree
requestGetWorkingLocation =
  req
    "GetWorkingLocation"
    "fixture/GetWorkingLocation.yaml"

requestListChangesets :: ListChangesets -> TestTree
requestListChangesets =
  req
    "ListChangesets"
    "fixture/ListChangesets.yaml"

requestListDataViews :: ListDataViews -> TestTree
requestListDataViews =
  req
    "ListDataViews"
    "fixture/ListDataViews.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListPermissionGroups :: ListPermissionGroups -> TestTree
requestListPermissionGroups =
  req
    "ListPermissionGroups"
    "fixture/ListPermissionGroups.yaml"

requestListPermissionGroupsByUser :: ListPermissionGroupsByUser -> TestTree
requestListPermissionGroupsByUser =
  req
    "ListPermissionGroupsByUser"
    "fixture/ListPermissionGroupsByUser.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListUsersByPermissionGroup :: ListUsersByPermissionGroup -> TestTree
requestListUsersByPermissionGroup =
  req
    "ListUsersByPermissionGroup"
    "fixture/ListUsersByPermissionGroup.yaml"

requestResetUserPassword :: ResetUserPassword -> TestTree
requestResetUserPassword =
  req
    "ResetUserPassword"
    "fixture/ResetUserPassword.yaml"

requestUpdateChangeset :: UpdateChangeset -> TestTree
requestUpdateChangeset =
  req
    "UpdateChangeset"
    "fixture/UpdateChangeset.yaml"

requestUpdateDataset :: UpdateDataset -> TestTree
requestUpdateDataset =
  req
    "UpdateDataset"
    "fixture/UpdateDataset.yaml"

requestUpdatePermissionGroup :: UpdatePermissionGroup -> TestTree
requestUpdatePermissionGroup =
  req
    "UpdatePermissionGroup"
    "fixture/UpdatePermissionGroup.yaml"

requestUpdateUser :: UpdateUser -> TestTree
requestUpdateUser =
  req
    "UpdateUser"
    "fixture/UpdateUser.yaml"

-- Responses

responseAssociateUserToPermissionGroup :: AssociateUserToPermissionGroupResponse -> TestTree
responseAssociateUserToPermissionGroup =
  res
    "AssociateUserToPermissionGroupResponse"
    "fixture/AssociateUserToPermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateUserToPermissionGroup)

responseCreateChangeset :: CreateChangesetResponse -> TestTree
responseCreateChangeset =
  res
    "CreateChangesetResponse"
    "fixture/CreateChangesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateChangeset)

responseCreateDataView :: CreateDataViewResponse -> TestTree
responseCreateDataView =
  res
    "CreateDataViewResponse"
    "fixture/CreateDataViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataView)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreatePermissionGroup :: CreatePermissionGroupResponse -> TestTree
responseCreatePermissionGroup =
  res
    "CreatePermissionGroupResponse"
    "fixture/CreatePermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePermissionGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeletePermissionGroup :: DeletePermissionGroupResponse -> TestTree
responseDeletePermissionGroup =
  res
    "DeletePermissionGroupResponse"
    "fixture/DeletePermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionGroup)

responseDisableUser :: DisableUserResponse -> TestTree
responseDisableUser =
  res
    "DisableUserResponse"
    "fixture/DisableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableUser)

responseDisassociateUserFromPermissionGroup :: DisassociateUserFromPermissionGroupResponse -> TestTree
responseDisassociateUserFromPermissionGroup =
  res
    "DisassociateUserFromPermissionGroupResponse"
    "fixture/DisassociateUserFromPermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateUserFromPermissionGroup)

responseEnableUser :: EnableUserResponse -> TestTree
responseEnableUser =
  res
    "EnableUserResponse"
    "fixture/EnableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableUser)

responseGetChangeset :: GetChangesetResponse -> TestTree
responseGetChangeset =
  res
    "GetChangesetResponse"
    "fixture/GetChangesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeset)

responseGetDataView :: GetDataViewResponse -> TestTree
responseGetDataView =
  res
    "GetDataViewResponse"
    "fixture/GetDataViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataView)

responseGetDataset :: GetDatasetResponse -> TestTree
responseGetDataset =
  res
    "GetDatasetResponse"
    "fixture/GetDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataset)

responseGetExternalDataViewAccessDetails :: GetExternalDataViewAccessDetailsResponse -> TestTree
responseGetExternalDataViewAccessDetails =
  res
    "GetExternalDataViewAccessDetailsResponse"
    "fixture/GetExternalDataViewAccessDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExternalDataViewAccessDetails)

responseGetPermissionGroup :: GetPermissionGroupResponse -> TestTree
responseGetPermissionGroup =
  res
    "GetPermissionGroupResponse"
    "fixture/GetPermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermissionGroup)

responseGetProgrammaticAccessCredentials :: GetProgrammaticAccessCredentialsResponse -> TestTree
responseGetProgrammaticAccessCredentials =
  res
    "GetProgrammaticAccessCredentialsResponse"
    "fixture/GetProgrammaticAccessCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProgrammaticAccessCredentials)

responseGetUser :: GetUserResponse -> TestTree
responseGetUser =
  res
    "GetUserResponse"
    "fixture/GetUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUser)

responseGetWorkingLocation :: GetWorkingLocationResponse -> TestTree
responseGetWorkingLocation =
  res
    "GetWorkingLocationResponse"
    "fixture/GetWorkingLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkingLocation)

responseListChangesets :: ListChangesetsResponse -> TestTree
responseListChangesets =
  res
    "ListChangesetsResponse"
    "fixture/ListChangesetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChangesets)

responseListDataViews :: ListDataViewsResponse -> TestTree
responseListDataViews =
  res
    "ListDataViewsResponse"
    "fixture/ListDataViewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataViews)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListPermissionGroups :: ListPermissionGroupsResponse -> TestTree
responseListPermissionGroups =
  res
    "ListPermissionGroupsResponse"
    "fixture/ListPermissionGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionGroups)

responseListPermissionGroupsByUser :: ListPermissionGroupsByUserResponse -> TestTree
responseListPermissionGroupsByUser =
  res
    "ListPermissionGroupsByUserResponse"
    "fixture/ListPermissionGroupsByUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionGroupsByUser)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responseListUsersByPermissionGroup :: ListUsersByPermissionGroupResponse -> TestTree
responseListUsersByPermissionGroup =
  res
    "ListUsersByPermissionGroupResponse"
    "fixture/ListUsersByPermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsersByPermissionGroup)

responseResetUserPassword :: ResetUserPasswordResponse -> TestTree
responseResetUserPassword =
  res
    "ResetUserPasswordResponse"
    "fixture/ResetUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetUserPassword)

responseUpdateChangeset :: UpdateChangesetResponse -> TestTree
responseUpdateChangeset =
  res
    "UpdateChangesetResponse"
    "fixture/UpdateChangesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateChangeset)

responseUpdateDataset :: UpdateDatasetResponse -> TestTree
responseUpdateDataset =
  res
    "UpdateDatasetResponse"
    "fixture/UpdateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDataset)

responseUpdatePermissionGroup :: UpdatePermissionGroupResponse -> TestTree
responseUpdatePermissionGroup =
  res
    "UpdatePermissionGroupResponse"
    "fixture/UpdatePermissionGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePermissionGroup)

responseUpdateUser :: UpdateUserResponse -> TestTree
responseUpdateUser =
  res
    "UpdateUserResponse"
    "fixture/UpdateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUser)
