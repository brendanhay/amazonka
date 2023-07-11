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
-- Module      : Amazonka.Greengrass.DisassociateRoleFromGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the role from a group.
module Amazonka.Greengrass.DisassociateRoleFromGroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateRoleFromGroup' smart constructor.
data DisassociateRoleFromGroup = DisassociateRoleFromGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisassociateRoleFromGroup where
  type
    AWSResponse DisassociateRoleFromGroup =
      DisassociateRoleFromGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateRoleFromGroupResponse'
            Prelude.<$> (x Data..?> "DisassociatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateRoleFromGroup where
  hashWithSalt _salt DisassociateRoleFromGroup' {..} =
    _salt `Prelude.hashWithSalt` groupId

instance Prelude.NFData DisassociateRoleFromGroup where
  rnf DisassociateRoleFromGroup' {..} =
    Prelude.rnf groupId

instance Data.ToHeaders DisassociateRoleFromGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DisassociateRoleFromGroup where
  toPath DisassociateRoleFromGroup' {..} =
    Prelude.mconcat
      ["/greengrass/groups/", Data.toBS groupId, "/role"]

instance Data.ToQuery DisassociateRoleFromGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateRoleFromGroupResponse' smart constructor.
data DisassociateRoleFromGroupResponse = DisassociateRoleFromGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role was
    -- disassociated from the group.
    disassociatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateRoleFromGroupResponse' {..} =
    Prelude.rnf disassociatedAt
      `Prelude.seq` Prelude.rnf httpStatus
