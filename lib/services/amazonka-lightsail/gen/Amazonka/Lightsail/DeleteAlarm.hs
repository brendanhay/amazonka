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
-- Module      : Amazonka.Lightsail.DeleteAlarm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alarm.
--
-- An alarm is used to monitor a single metric for one of your resources.
-- When a metric condition is met, the alarm can notify you by email, SMS
-- text message, and a banner displayed on the Amazon Lightsail console.
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail>.
module Amazonka.Lightsail.DeleteAlarm
  ( -- * Creating a Request
    DeleteAlarm (..),
    newDeleteAlarm,

    -- * Request Lenses
    deleteAlarm_alarmName,

    -- * Destructuring the Response
    DeleteAlarmResponse (..),
    newDeleteAlarmResponse,

    -- * Response Lenses
    deleteAlarmResponse_operations,
    deleteAlarmResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlarm' smart constructor.
data DeleteAlarm = DeleteAlarm'
  { -- | The name of the alarm to delete.
    alarmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmName', 'deleteAlarm_alarmName' - The name of the alarm to delete.
newDeleteAlarm ::
  -- | 'alarmName'
  Prelude.Text ->
  DeleteAlarm
newDeleteAlarm pAlarmName_ =
  DeleteAlarm' {alarmName = pAlarmName_}

-- | The name of the alarm to delete.
deleteAlarm_alarmName :: Lens.Lens' DeleteAlarm Prelude.Text
deleteAlarm_alarmName = Lens.lens (\DeleteAlarm' {alarmName} -> alarmName) (\s@DeleteAlarm' {} a -> s {alarmName = a} :: DeleteAlarm)

instance Core.AWSRequest DeleteAlarm where
  type AWSResponse DeleteAlarm = DeleteAlarmResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAlarmResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAlarm where
  hashWithSalt _salt DeleteAlarm' {..} =
    _salt `Prelude.hashWithSalt` alarmName

instance Prelude.NFData DeleteAlarm where
  rnf DeleteAlarm' {..} = Prelude.rnf alarmName

instance Data.ToHeaders DeleteAlarm where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteAlarm" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlarm where
  toJSON DeleteAlarm' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("alarmName" Data..= alarmName)]
      )

instance Data.ToPath DeleteAlarm where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlarm where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteAlarmResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteAlarmResponse_httpStatus' - The response's http status code.
newDeleteAlarmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAlarmResponse
newDeleteAlarmResponse pHttpStatus_ =
  DeleteAlarmResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteAlarmResponse_operations :: Lens.Lens' DeleteAlarmResponse (Prelude.Maybe [Operation])
deleteAlarmResponse_operations = Lens.lens (\DeleteAlarmResponse' {operations} -> operations) (\s@DeleteAlarmResponse' {} a -> s {operations = a} :: DeleteAlarmResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteAlarmResponse_httpStatus :: Lens.Lens' DeleteAlarmResponse Prelude.Int
deleteAlarmResponse_httpStatus = Lens.lens (\DeleteAlarmResponse' {httpStatus} -> httpStatus) (\s@DeleteAlarmResponse' {} a -> s {httpStatus = a} :: DeleteAlarmResponse)

instance Prelude.NFData DeleteAlarmResponse where
  rnf DeleteAlarmResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
