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
-- Module      : Amazonka.Scheduler.DeleteScheduleGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified schedule group. Deleting a schedule group results
-- in EventBridge Scheduler deleting all schedules associated with the
-- group. When you delete a group, it remains in a @DELETING@ state until
-- all of its associated schedules are deleted. Schedules associated with
-- the group that are set to run while the schedule group is in the process
-- of being deleted might continue to invoke their targets until the
-- schedule group and its associated schedules are deleted.
--
-- This operation is eventually consistent.
module Amazonka.Scheduler.DeleteScheduleGroup
  ( -- * Creating a Request
    DeleteScheduleGroup (..),
    newDeleteScheduleGroup,

    -- * Request Lenses
    deleteScheduleGroup_clientToken,
    deleteScheduleGroup_name,

    -- * Destructuring the Response
    DeleteScheduleGroupResponse (..),
    newDeleteScheduleGroupResponse,

    -- * Response Lenses
    deleteScheduleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newDeleteScheduleGroup' smart constructor.
data DeleteScheduleGroup = DeleteScheduleGroup'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, EventBridge
    -- Scheduler uses a randomly generated token for the request to ensure
    -- idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the schedule group to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteScheduleGroup_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
--
-- 'name', 'deleteScheduleGroup_name' - The name of the schedule group to delete.
newDeleteScheduleGroup ::
  -- | 'name'
  Prelude.Text ->
  DeleteScheduleGroup
newDeleteScheduleGroup pName_ =
  DeleteScheduleGroup'
    { clientToken = Prelude.Nothing,
      name = pName_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
deleteScheduleGroup_clientToken :: Lens.Lens' DeleteScheduleGroup (Prelude.Maybe Prelude.Text)
deleteScheduleGroup_clientToken = Lens.lens (\DeleteScheduleGroup' {clientToken} -> clientToken) (\s@DeleteScheduleGroup' {} a -> s {clientToken = a} :: DeleteScheduleGroup)

-- | The name of the schedule group to delete.
deleteScheduleGroup_name :: Lens.Lens' DeleteScheduleGroup Prelude.Text
deleteScheduleGroup_name = Lens.lens (\DeleteScheduleGroup' {name} -> name) (\s@DeleteScheduleGroup' {} a -> s {name = a} :: DeleteScheduleGroup)

instance Core.AWSRequest DeleteScheduleGroup where
  type
    AWSResponse DeleteScheduleGroup =
      DeleteScheduleGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteScheduleGroup where
  hashWithSalt _salt DeleteScheduleGroup' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteScheduleGroup where
  rnf DeleteScheduleGroup' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteScheduleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteScheduleGroup where
  toPath DeleteScheduleGroup' {..} =
    Prelude.mconcat
      ["/schedule-groups/", Data.toBS name]

instance Data.ToQuery DeleteScheduleGroup where
  toQuery DeleteScheduleGroup' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteScheduleGroupResponse' smart constructor.
data DeleteScheduleGroupResponse = DeleteScheduleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduleGroupResponse_httpStatus' - The response's http status code.
newDeleteScheduleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteScheduleGroupResponse
newDeleteScheduleGroupResponse pHttpStatus_ =
  DeleteScheduleGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteScheduleGroupResponse_httpStatus :: Lens.Lens' DeleteScheduleGroupResponse Prelude.Int
deleteScheduleGroupResponse_httpStatus = Lens.lens (\DeleteScheduleGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduleGroupResponse' {} a -> s {httpStatus = a} :: DeleteScheduleGroupResponse)

instance Prelude.NFData DeleteScheduleGroupResponse where
  rnf DeleteScheduleGroupResponse' {..} =
    Prelude.rnf httpStatus
