{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserHierarchyGroupName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of the user hierarchy group.
module Network.AWS.Connect.UpdateUserHierarchyGroupName
  ( -- * Creating a Request
    UpdateUserHierarchyGroupName (..),
    newUpdateUserHierarchyGroupName,

    -- * Request Lenses
    updateUserHierarchyGroupName_name,
    updateUserHierarchyGroupName_hierarchyGroupId,
    updateUserHierarchyGroupName_instanceId,

    -- * Destructuring the Response
    UpdateUserHierarchyGroupNameResponse (..),
    newUpdateUserHierarchyGroupNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserHierarchyGroupName' smart constructor.
data UpdateUserHierarchyGroupName = UpdateUserHierarchyGroupName'
  { -- | The name of the hierarchy group. Must not be more than 100 characters.
    name :: Prelude.Text,
    -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyGroupName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateUserHierarchyGroupName_name' - The name of the hierarchy group. Must not be more than 100 characters.
--
-- 'hierarchyGroupId', 'updateUserHierarchyGroupName_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'instanceId', 'updateUserHierarchyGroupName_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserHierarchyGroupName ::
  -- | 'name'
  Prelude.Text ->
  -- | 'hierarchyGroupId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserHierarchyGroupName
newUpdateUserHierarchyGroupName
  pName_
  pHierarchyGroupId_
  pInstanceId_ =
    UpdateUserHierarchyGroupName'
      { name = pName_,
        hierarchyGroupId = pHierarchyGroupId_,
        instanceId = pInstanceId_
      }

-- | The name of the hierarchy group. Must not be more than 100 characters.
updateUserHierarchyGroupName_name :: Lens.Lens' UpdateUserHierarchyGroupName Prelude.Text
updateUserHierarchyGroupName_name = Lens.lens (\UpdateUserHierarchyGroupName' {name} -> name) (\s@UpdateUserHierarchyGroupName' {} a -> s {name = a} :: UpdateUserHierarchyGroupName)

-- | The identifier of the hierarchy group.
updateUserHierarchyGroupName_hierarchyGroupId :: Lens.Lens' UpdateUserHierarchyGroupName Prelude.Text
updateUserHierarchyGroupName_hierarchyGroupId = Lens.lens (\UpdateUserHierarchyGroupName' {hierarchyGroupId} -> hierarchyGroupId) (\s@UpdateUserHierarchyGroupName' {} a -> s {hierarchyGroupId = a} :: UpdateUserHierarchyGroupName)

-- | The identifier of the Amazon Connect instance.
updateUserHierarchyGroupName_instanceId :: Lens.Lens' UpdateUserHierarchyGroupName Prelude.Text
updateUserHierarchyGroupName_instanceId = Lens.lens (\UpdateUserHierarchyGroupName' {instanceId} -> instanceId) (\s@UpdateUserHierarchyGroupName' {} a -> s {instanceId = a} :: UpdateUserHierarchyGroupName)

instance
  Prelude.AWSRequest
    UpdateUserHierarchyGroupName
  where
  type
    Rs UpdateUserHierarchyGroupName =
      UpdateUserHierarchyGroupNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateUserHierarchyGroupNameResponse'

instance
  Prelude.Hashable
    UpdateUserHierarchyGroupName

instance Prelude.NFData UpdateUserHierarchyGroupName

instance
  Prelude.ToHeaders
    UpdateUserHierarchyGroupName
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserHierarchyGroupName where
  toJSON UpdateUserHierarchyGroupName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath UpdateUserHierarchyGroupName where
  toPath UpdateUserHierarchyGroupName' {..} =
    Prelude.mconcat
      [ "/user-hierarchy-groups/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS hierarchyGroupId,
        "/name"
      ]

instance Prelude.ToQuery UpdateUserHierarchyGroupName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserHierarchyGroupNameResponse' smart constructor.
data UpdateUserHierarchyGroupNameResponse = UpdateUserHierarchyGroupNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyGroupNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserHierarchyGroupNameResponse ::
  UpdateUserHierarchyGroupNameResponse
newUpdateUserHierarchyGroupNameResponse =
  UpdateUserHierarchyGroupNameResponse'

instance
  Prelude.NFData
    UpdateUserHierarchyGroupNameResponse
