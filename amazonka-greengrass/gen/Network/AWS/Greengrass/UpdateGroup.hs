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
-- Module      : Network.AWS.Greengrass.UpdateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a group.
module Network.AWS.Greengrass.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_name,
    updateGroup_groupId,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,

    -- * Response Lenses
    updateGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateGroup_name' - The name of the definition.
--
-- 'groupId', 'updateGroup_groupId' - The ID of the Greengrass group.
newUpdateGroup ::
  -- | 'groupId'
  Core.Text ->
  UpdateGroup
newUpdateGroup pGroupId_ =
  UpdateGroup'
    { name = Core.Nothing,
      groupId = pGroupId_
    }

-- | The name of the definition.
updateGroup_name :: Lens.Lens' UpdateGroup (Core.Maybe Core.Text)
updateGroup_name = Lens.lens (\UpdateGroup' {name} -> name) (\s@UpdateGroup' {} a -> s {name = a} :: UpdateGroup)

-- | The ID of the Greengrass group.
updateGroup_groupId :: Lens.Lens' UpdateGroup Core.Text
updateGroup_groupId = Lens.lens (\UpdateGroup' {groupId} -> groupId) (\s@UpdateGroup' {} a -> s {groupId = a} :: UpdateGroup)

instance Core.AWSRequest UpdateGroup where
  type AWSResponse UpdateGroup = UpdateGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGroup

instance Core.NFData UpdateGroup

instance Core.ToHeaders UpdateGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateGroup where
  toPath UpdateGroup' {..} =
    Core.mconcat
      ["/greengrass/groups/", Core.toBS groupId]

instance Core.ToQuery UpdateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGroupResponse_httpStatus' - The response's http status code.
newUpdateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGroupResponse
newUpdateGroupResponse pHttpStatus_ =
  UpdateGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateGroupResponse_httpStatus :: Lens.Lens' UpdateGroupResponse Core.Int
updateGroupResponse_httpStatus = Lens.lens (\UpdateGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupResponse' {} a -> s {httpStatus = a} :: UpdateGroupResponse)

instance Core.NFData UpdateGroupResponse
