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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateRoleFromGroup' smart constructor.
data DisassociateRoleFromGroup = DisassociateRoleFromGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisassociateRoleFromGroup
newDisassociateRoleFromGroup pGroupId_ =
  DisassociateRoleFromGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
disassociateRoleFromGroup_groupId :: Lens.Lens' DisassociateRoleFromGroup Prelude.Text
disassociateRoleFromGroup_groupId = Lens.lens (\DisassociateRoleFromGroup' {groupId} -> groupId) (\s@DisassociateRoleFromGroup' {} a -> s {groupId = a} :: DisassociateRoleFromGroup)

instance Prelude.AWSRequest DisassociateRoleFromGroup where
  type
    Rs DisassociateRoleFromGroup =
      DisassociateRoleFromGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateRoleFromGroupResponse'
            Prelude.<$> (x Prelude..?> "DisassociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateRoleFromGroup

instance Prelude.NFData DisassociateRoleFromGroup

instance Prelude.ToHeaders DisassociateRoleFromGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DisassociateRoleFromGroup where
  toPath DisassociateRoleFromGroup' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/role"
      ]

instance Prelude.ToQuery DisassociateRoleFromGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRoleFromGroupResponse' smart constructor.
data DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role was
    -- disassociated from the group.
    disassociatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateRoleFromGroupResponse
newDisassociateRoleFromGroupResponse pHttpStatus_ =
  DisassociateRoleFromGroupResponse'
    { disassociatedAt =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the role was
-- disassociated from the group.
disassociateRoleFromGroupResponse_disassociatedAt :: Lens.Lens' DisassociateRoleFromGroupResponse (Prelude.Maybe Prelude.Text)
disassociateRoleFromGroupResponse_disassociatedAt = Lens.lens (\DisassociateRoleFromGroupResponse' {disassociatedAt} -> disassociatedAt) (\s@DisassociateRoleFromGroupResponse' {} a -> s {disassociatedAt = a} :: DisassociateRoleFromGroupResponse)

-- | The response's http status code.
disassociateRoleFromGroupResponse_httpStatus :: Lens.Lens' DisassociateRoleFromGroupResponse Prelude.Int
disassociateRoleFromGroupResponse_httpStatus = Lens.lens (\DisassociateRoleFromGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateRoleFromGroupResponse' {} a -> s {httpStatus = a} :: DisassociateRoleFromGroupResponse)

instance
  Prelude.NFData
    DisassociateRoleFromGroupResponse
