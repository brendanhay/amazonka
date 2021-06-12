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
-- Module      : Network.AWS.Greengrass.DisassociateRoleFromGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the role from a group.
module Network.AWS.Greengrass.DisassociateRoleFromGroup
  ( -- * Creating a Request
    DisassociateRoleFromGroup (..),
    newDisassociateRoleFromGroup,

    -- * Request Lenses
    disassociateRoleFromGroup_groupId,

    -- * Destructuring the Response
    DisassociateRoleFromGroupResponse (..),
    newDisassociateRoleFromGroupResponse,

    -- * Response Lenses
    disassociateRoleFromGroupResponse_disassociatedAt,
    disassociateRoleFromGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRoleFromGroup' smart constructor.
data DisassociateRoleFromGroup = DisassociateRoleFromGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRoleFromGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'disassociateRoleFromGroup_groupId' - The ID of the Greengrass group.
newDisassociateRoleFromGroup ::
  -- | 'groupId'
  Core.Text ->
  DisassociateRoleFromGroup
newDisassociateRoleFromGroup pGroupId_ =
  DisassociateRoleFromGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
disassociateRoleFromGroup_groupId :: Lens.Lens' DisassociateRoleFromGroup Core.Text
disassociateRoleFromGroup_groupId = Lens.lens (\DisassociateRoleFromGroup' {groupId} -> groupId) (\s@DisassociateRoleFromGroup' {} a -> s {groupId = a} :: DisassociateRoleFromGroup)

instance Core.AWSRequest DisassociateRoleFromGroup where
  type
    AWSResponse DisassociateRoleFromGroup =
      DisassociateRoleFromGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateRoleFromGroupResponse'
            Core.<$> (x Core..?> "DisassociatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateRoleFromGroup

instance Core.NFData DisassociateRoleFromGroup

instance Core.ToHeaders DisassociateRoleFromGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DisassociateRoleFromGroup where
  toPath DisassociateRoleFromGroup' {..} =
    Core.mconcat
      ["/greengrass/groups/", Core.toBS groupId, "/role"]

instance Core.ToQuery DisassociateRoleFromGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateRoleFromGroupResponse' smart constructor.
data DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role was
    -- disassociated from the group.
    disassociatedAt :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateRoleFromGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedAt', 'disassociateRoleFromGroupResponse_disassociatedAt' - The time, in milliseconds since the epoch, when the role was
-- disassociated from the group.
--
-- 'httpStatus', 'disassociateRoleFromGroupResponse_httpStatus' - The response's http status code.
newDisassociateRoleFromGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateRoleFromGroupResponse
newDisassociateRoleFromGroupResponse pHttpStatus_ =
  DisassociateRoleFromGroupResponse'
    { disassociatedAt =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the role was
-- disassociated from the group.
disassociateRoleFromGroupResponse_disassociatedAt :: Lens.Lens' DisassociateRoleFromGroupResponse (Core.Maybe Core.Text)
disassociateRoleFromGroupResponse_disassociatedAt = Lens.lens (\DisassociateRoleFromGroupResponse' {disassociatedAt} -> disassociatedAt) (\s@DisassociateRoleFromGroupResponse' {} a -> s {disassociatedAt = a} :: DisassociateRoleFromGroupResponse)

-- | The response's http status code.
disassociateRoleFromGroupResponse_httpStatus :: Lens.Lens' DisassociateRoleFromGroupResponse Core.Int
disassociateRoleFromGroupResponse_httpStatus = Lens.lens (\DisassociateRoleFromGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateRoleFromGroupResponse' {} a -> s {httpStatus = a} :: DisassociateRoleFromGroupResponse)

instance
  Core.NFData
    DisassociateRoleFromGroupResponse
