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
-- Module      : Network.AWS.Connect.UpdateUserHierarchy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified hierarchy group to the specified user.
module Network.AWS.Connect.UpdateUserHierarchy
  ( -- * Creating a Request
    UpdateUserHierarchy (..),
    newUpdateUserHierarchy,

    -- * Request Lenses
    updateUserHierarchy_hierarchyGroupId,
    updateUserHierarchy_userId,
    updateUserHierarchy_instanceId,

    -- * Destructuring the Response
    UpdateUserHierarchyResponse (..),
    newUpdateUserHierarchyResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserHierarchy' smart constructor.
data UpdateUserHierarchy = UpdateUserHierarchy'
  { -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupId', 'updateUserHierarchy_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'userId', 'updateUserHierarchy_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserHierarchy_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserHierarchy ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserHierarchy
newUpdateUserHierarchy pUserId_ pInstanceId_ =
  UpdateUserHierarchy'
    { hierarchyGroupId =
        Prelude.Nothing,
      userId = pUserId_,
      instanceId = pInstanceId_
    }

-- | The identifier of the hierarchy group.
updateUserHierarchy_hierarchyGroupId :: Lens.Lens' UpdateUserHierarchy (Prelude.Maybe Prelude.Text)
updateUserHierarchy_hierarchyGroupId = Lens.lens (\UpdateUserHierarchy' {hierarchyGroupId} -> hierarchyGroupId) (\s@UpdateUserHierarchy' {} a -> s {hierarchyGroupId = a} :: UpdateUserHierarchy)

-- | The identifier of the user account.
updateUserHierarchy_userId :: Lens.Lens' UpdateUserHierarchy Prelude.Text
updateUserHierarchy_userId = Lens.lens (\UpdateUserHierarchy' {userId} -> userId) (\s@UpdateUserHierarchy' {} a -> s {userId = a} :: UpdateUserHierarchy)

-- | The identifier of the Amazon Connect instance.
updateUserHierarchy_instanceId :: Lens.Lens' UpdateUserHierarchy Prelude.Text
updateUserHierarchy_instanceId = Lens.lens (\UpdateUserHierarchy' {instanceId} -> instanceId) (\s@UpdateUserHierarchy' {} a -> s {instanceId = a} :: UpdateUserHierarchy)

instance Prelude.AWSRequest UpdateUserHierarchy where
  type
    Rs UpdateUserHierarchy =
      UpdateUserHierarchyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateUserHierarchyResponse'

instance Prelude.Hashable UpdateUserHierarchy

instance Prelude.NFData UpdateUserHierarchy

instance Prelude.ToHeaders UpdateUserHierarchy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserHierarchy where
  toJSON UpdateUserHierarchy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HierarchyGroupId" Prelude..=)
              Prelude.<$> hierarchyGroupId
          ]
      )

instance Prelude.ToPath UpdateUserHierarchy where
  toPath UpdateUserHierarchy' {..} =
    Prelude.mconcat
      [ "/users/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS userId,
        "/hierarchy"
      ]

instance Prelude.ToQuery UpdateUserHierarchy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserHierarchyResponse' smart constructor.
data UpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserHierarchyResponse ::
  UpdateUserHierarchyResponse
newUpdateUserHierarchyResponse =
  UpdateUserHierarchyResponse'

instance Prelude.NFData UpdateUserHierarchyResponse
