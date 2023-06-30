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
-- Module      : Amazonka.Connect.UpdateContactSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the scheduled time of a task contact that is already scheduled.
module Amazonka.Connect.UpdateContactSchedule
  ( -- * Creating a Request
    UpdateContactSchedule (..),
    newUpdateContactSchedule,

    -- * Request Lenses
    updateContactSchedule_instanceId,
    updateContactSchedule_contactId,
    updateContactSchedule_scheduledTime,

    -- * Destructuring the Response
    UpdateContactScheduleResponse (..),
    newUpdateContactScheduleResponse,

    -- * Response Lenses
    updateContactScheduleResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactSchedule' smart constructor.
data UpdateContactSchedule = UpdateContactSchedule'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text,
    -- | The timestamp, in Unix Epoch seconds format, at which to start running
    -- the inbound flow. The scheduled time cannot be in the past. It must be
    -- within up to 6 days in future.
    scheduledTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateContactSchedule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactId', 'updateContactSchedule_contactId' - The identifier of the contact.
--
-- 'scheduledTime', 'updateContactSchedule_scheduledTime' - The timestamp, in Unix Epoch seconds format, at which to start running
-- the inbound flow. The scheduled time cannot be in the past. It must be
-- within up to 6 days in future.
newUpdateContactSchedule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  -- | 'scheduledTime'
  Prelude.UTCTime ->
  UpdateContactSchedule
newUpdateContactSchedule
  pInstanceId_
  pContactId_
  pScheduledTime_ =
    UpdateContactSchedule'
      { instanceId = pInstanceId_,
        contactId = pContactId_,
        scheduledTime = Data._Time Lens.# pScheduledTime_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateContactSchedule_instanceId :: Lens.Lens' UpdateContactSchedule Prelude.Text
updateContactSchedule_instanceId = Lens.lens (\UpdateContactSchedule' {instanceId} -> instanceId) (\s@UpdateContactSchedule' {} a -> s {instanceId = a} :: UpdateContactSchedule)

-- | The identifier of the contact.
updateContactSchedule_contactId :: Lens.Lens' UpdateContactSchedule Prelude.Text
updateContactSchedule_contactId = Lens.lens (\UpdateContactSchedule' {contactId} -> contactId) (\s@UpdateContactSchedule' {} a -> s {contactId = a} :: UpdateContactSchedule)

-- | The timestamp, in Unix Epoch seconds format, at which to start running
-- the inbound flow. The scheduled time cannot be in the past. It must be
-- within up to 6 days in future.
updateContactSchedule_scheduledTime :: Lens.Lens' UpdateContactSchedule Prelude.UTCTime
updateContactSchedule_scheduledTime = Lens.lens (\UpdateContactSchedule' {scheduledTime} -> scheduledTime) (\s@UpdateContactSchedule' {} a -> s {scheduledTime = a} :: UpdateContactSchedule) Prelude.. Data._Time

instance Core.AWSRequest UpdateContactSchedule where
  type
    AWSResponse UpdateContactSchedule =
      UpdateContactScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContactSchedule where
  hashWithSalt _salt UpdateContactSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId
      `Prelude.hashWithSalt` scheduledTime

instance Prelude.NFData UpdateContactSchedule where
  rnf UpdateContactSchedule' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId
      `Prelude.seq` Prelude.rnf scheduledTime

instance Data.ToHeaders UpdateContactSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContactSchedule where
  toJSON UpdateContactSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId),
            Prelude.Just
              ("ScheduledTime" Data..= scheduledTime)
          ]
      )

instance Data.ToPath UpdateContactSchedule where
  toPath = Prelude.const "/contact/schedule"

instance Data.ToQuery UpdateContactSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactScheduleResponse' smart constructor.
data UpdateContactScheduleResponse = UpdateContactScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactScheduleResponse_httpStatus' - The response's http status code.
newUpdateContactScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContactScheduleResponse
newUpdateContactScheduleResponse pHttpStatus_ =
  UpdateContactScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateContactScheduleResponse_httpStatus :: Lens.Lens' UpdateContactScheduleResponse Prelude.Int
updateContactScheduleResponse_httpStatus = Lens.lens (\UpdateContactScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateContactScheduleResponse' {} a -> s {httpStatus = a} :: UpdateContactScheduleResponse)

instance Prelude.NFData UpdateContactScheduleResponse where
  rnf UpdateContactScheduleResponse' {..} =
    Prelude.rnf httpStatus
