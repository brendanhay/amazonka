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
-- Module      : Amazonka.Connect.UpdateUserHierarchy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified hierarchy group to the specified user.
module Amazonka.Connect.UpdateUserHierarchy
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUserHierarchy' smart constructor.
data UpdateUserHierarchy = UpdateUserHierarchy'
  { -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'instanceId', 'updateUserHierarchy_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateUserHierarchy_instanceId :: Lens.Lens' UpdateUserHierarchy Prelude.Text
updateUserHierarchy_instanceId = Lens.lens (\UpdateUserHierarchy' {instanceId} -> instanceId) (\s@UpdateUserHierarchy' {} a -> s {instanceId = a} :: UpdateUserHierarchy)

instance Core.AWSRequest UpdateUserHierarchy where
  type
    AWSResponse UpdateUserHierarchy =
      UpdateUserHierarchyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateUserHierarchyResponse'

instance Prelude.Hashable UpdateUserHierarchy where
  hashWithSalt _salt UpdateUserHierarchy' {..} =
    _salt `Prelude.hashWithSalt` hierarchyGroupId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData UpdateUserHierarchy where
  rnf UpdateUserHierarchy' {..} =
    Prelude.rnf hierarchyGroupId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders UpdateUserHierarchy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserHierarchy where
  toJSON UpdateUserHierarchy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HierarchyGroupId" Data..=)
              Prelude.<$> hierarchyGroupId
          ]
      )

instance Data.ToPath UpdateUserHierarchy where
  toPath UpdateUserHierarchy' {..} =
    Prelude.mconcat
      [ "/users/",
        Data.toBS instanceId,
        "/",
        Data.toBS userId,
        "/hierarchy"
      ]

instance Data.ToQuery UpdateUserHierarchy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserHierarchyResponse' smart constructor.
data UpdateUserHierarchyResponse = UpdateUserHierarchyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserHierarchyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserHierarchyResponse ::
  UpdateUserHierarchyResponse
newUpdateUserHierarchyResponse =
  UpdateUserHierarchyResponse'

instance Prelude.NFData UpdateUserHierarchyResponse where
  rnf _ = ()
