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
-- Module      : Network.AWS.Lightsail.DeleteAlarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Lightsail.DeleteAlarm
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAlarm' smart constructor.
data DeleteAlarm = DeleteAlarm'
  { -- | The name of the alarm to delete.
    alarmName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteAlarm
newDeleteAlarm pAlarmName_ =
  DeleteAlarm' {alarmName = pAlarmName_}

-- | The name of the alarm to delete.
deleteAlarm_alarmName :: Lens.Lens' DeleteAlarm Core.Text
deleteAlarm_alarmName = Lens.lens (\DeleteAlarm' {alarmName} -> alarmName) (\s@DeleteAlarm' {} a -> s {alarmName = a} :: DeleteAlarm)

instance Core.AWSRequest DeleteAlarm where
  type AWSResponse DeleteAlarm = DeleteAlarmResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAlarmResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAlarm

instance Core.NFData DeleteAlarm

instance Core.ToHeaders DeleteAlarm where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteAlarm" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAlarm where
  toJSON DeleteAlarm' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("alarmName" Core..= alarmName)]
      )

instance Core.ToPath DeleteAlarm where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAlarm where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteAlarmResponse
newDeleteAlarmResponse pHttpStatus_ =
  DeleteAlarmResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteAlarmResponse_operations :: Lens.Lens' DeleteAlarmResponse (Core.Maybe [Operation])
deleteAlarmResponse_operations = Lens.lens (\DeleteAlarmResponse' {operations} -> operations) (\s@DeleteAlarmResponse' {} a -> s {operations = a} :: DeleteAlarmResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteAlarmResponse_httpStatus :: Lens.Lens' DeleteAlarmResponse Core.Int
deleteAlarmResponse_httpStatus = Lens.lens (\DeleteAlarmResponse' {httpStatus} -> httpStatus) (\s@DeleteAlarmResponse' {} a -> s {httpStatus = a} :: DeleteAlarmResponse)

instance Core.NFData DeleteAlarmResponse
