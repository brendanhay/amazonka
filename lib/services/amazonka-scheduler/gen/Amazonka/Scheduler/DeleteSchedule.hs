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
-- Module      : Amazonka.Scheduler.DeleteSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified schedule.
module Amazonka.Scheduler.DeleteSchedule
  ( -- * Creating a Request
    DeleteSchedule (..),
    newDeleteSchedule,

    -- * Request Lenses
    deleteSchedule_clientToken,
    deleteSchedule_groupName,
    deleteSchedule_name,

    -- * Destructuring the Response
    DeleteScheduleResponse (..),
    newDeleteScheduleResponse,

    -- * Response Lenses
    deleteScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Scheduler.Types

-- | /See:/ 'newDeleteSchedule' smart constructor.
data DeleteSchedule = DeleteSchedule'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you do not specify a client token, EventBridge
    -- Scheduler uses a randomly generated token for the request to ensure
    -- idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the schedule group associated with this schedule. If you
    -- omit this, the default schedule group is used.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schedule to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteSchedule_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
--
-- 'groupName', 'deleteSchedule_groupName' - The name of the schedule group associated with this schedule. If you
-- omit this, the default schedule group is used.
--
-- 'name', 'deleteSchedule_name' - The name of the schedule to delete.
newDeleteSchedule ::
  -- | 'name'
  Prelude.Text ->
  DeleteSchedule
newDeleteSchedule pName_ =
  DeleteSchedule'
    { clientToken = Prelude.Nothing,
      groupName = Prelude.Nothing,
      name = pName_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you do not specify a client token, EventBridge
-- Scheduler uses a randomly generated token for the request to ensure
-- idempotency.
deleteSchedule_clientToken :: Lens.Lens' DeleteSchedule (Prelude.Maybe Prelude.Text)
deleteSchedule_clientToken = Lens.lens (\DeleteSchedule' {clientToken} -> clientToken) (\s@DeleteSchedule' {} a -> s {clientToken = a} :: DeleteSchedule)

-- | The name of the schedule group associated with this schedule. If you
-- omit this, the default schedule group is used.
deleteSchedule_groupName :: Lens.Lens' DeleteSchedule (Prelude.Maybe Prelude.Text)
deleteSchedule_groupName = Lens.lens (\DeleteSchedule' {groupName} -> groupName) (\s@DeleteSchedule' {} a -> s {groupName = a} :: DeleteSchedule)

-- | The name of the schedule to delete.
deleteSchedule_name :: Lens.Lens' DeleteSchedule Prelude.Text
deleteSchedule_name = Lens.lens (\DeleteSchedule' {name} -> name) (\s@DeleteSchedule' {} a -> s {name = a} :: DeleteSchedule)

instance Core.AWSRequest DeleteSchedule where
  type
    AWSResponse DeleteSchedule =
      DeleteScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchedule where
  hashWithSalt _salt DeleteSchedule' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteSchedule where
  rnf DeleteSchedule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    Prelude.mconcat ["/schedules/", Data.toBS name]

instance Data.ToQuery DeleteSchedule where
  toQuery DeleteSchedule' {..} =
    Prelude.mconcat
      [ "clientToken" Data.=: clientToken,
        "groupName" Data.=: groupName
      ]

-- | /See:/ 'newDeleteScheduleResponse' smart constructor.
data DeleteScheduleResponse = DeleteScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduleResponse_httpStatus' - The response's http status code.
newDeleteScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteScheduleResponse
newDeleteScheduleResponse pHttpStatus_ =
  DeleteScheduleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteScheduleResponse_httpStatus :: Lens.Lens' DeleteScheduleResponse Prelude.Int
deleteScheduleResponse_httpStatus = Lens.lens (\DeleteScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduleResponse' {} a -> s {httpStatus = a} :: DeleteScheduleResponse)

instance Prelude.NFData DeleteScheduleResponse where
  rnf DeleteScheduleResponse' {..} =
    Prelude.rnf httpStatus
