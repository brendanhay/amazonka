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
-- Module      : Amazonka.IoTEventsData.DescribeAlarm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an alarm.
module Amazonka.IoTEventsData.DescribeAlarm
  ( -- * Creating a Request
    DescribeAlarm (..),
    newDescribeAlarm,

    -- * Request Lenses
    describeAlarm_keyValue,
    describeAlarm_alarmModelName,

    -- * Destructuring the Response
    DescribeAlarmResponse (..),
    newDescribeAlarmResponse,

    -- * Response Lenses
    describeAlarmResponse_alarm,
    describeAlarmResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAlarm' smart constructor.
data DescribeAlarm = DescribeAlarm'
  { -- | The value of the key used as a filter to select only the alarms
    -- associated with the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
    keyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyValue', 'describeAlarm_keyValue' - The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
--
-- 'alarmModelName', 'describeAlarm_alarmModelName' - The name of the alarm model.
newDescribeAlarm ::
  -- | 'alarmModelName'
  Prelude.Text ->
  DescribeAlarm
newDescribeAlarm pAlarmModelName_ =
  DescribeAlarm'
    { keyValue = Prelude.Nothing,
      alarmModelName = pAlarmModelName_
    }

-- | The value of the key used as a filter to select only the alarms
-- associated with the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_CreateAlarmModel.html#iotevents-CreateAlarmModel-request-key key>.
describeAlarm_keyValue :: Lens.Lens' DescribeAlarm (Prelude.Maybe Prelude.Text)
describeAlarm_keyValue = Lens.lens (\DescribeAlarm' {keyValue} -> keyValue) (\s@DescribeAlarm' {} a -> s {keyValue = a} :: DescribeAlarm)

-- | The name of the alarm model.
describeAlarm_alarmModelName :: Lens.Lens' DescribeAlarm Prelude.Text
describeAlarm_alarmModelName = Lens.lens (\DescribeAlarm' {alarmModelName} -> alarmModelName) (\s@DescribeAlarm' {} a -> s {alarmModelName = a} :: DescribeAlarm)

instance Core.AWSRequest DescribeAlarm where
  type
    AWSResponse DescribeAlarm =
      DescribeAlarmResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAlarmResponse'
            Prelude.<$> (x Core..?> "alarm")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAlarm where
  hashWithSalt _salt DescribeAlarm' {..} =
    _salt `Prelude.hashWithSalt` keyValue
      `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData DescribeAlarm where
  rnf DescribeAlarm' {..} =
    Prelude.rnf keyValue
      `Prelude.seq` Prelude.rnf alarmModelName

instance Core.ToHeaders DescribeAlarm where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAlarm where
  toPath DescribeAlarm' {..} =
    Prelude.mconcat
      ["/alarms/", Core.toBS alarmModelName, "/keyValues/"]

instance Core.ToQuery DescribeAlarm where
  toQuery DescribeAlarm' {..} =
    Prelude.mconcat ["keyValue" Core.=: keyValue]

-- | /See:/ 'newDescribeAlarmResponse' smart constructor.
data DescribeAlarmResponse = DescribeAlarmResponse'
  { -- | Contains information about an alarm.
    alarm :: Prelude.Maybe Alarm,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarmResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarm', 'describeAlarmResponse_alarm' - Contains information about an alarm.
--
-- 'httpStatus', 'describeAlarmResponse_httpStatus' - The response's http status code.
newDescribeAlarmResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAlarmResponse
newDescribeAlarmResponse pHttpStatus_ =
  DescribeAlarmResponse'
    { alarm = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about an alarm.
describeAlarmResponse_alarm :: Lens.Lens' DescribeAlarmResponse (Prelude.Maybe Alarm)
describeAlarmResponse_alarm = Lens.lens (\DescribeAlarmResponse' {alarm} -> alarm) (\s@DescribeAlarmResponse' {} a -> s {alarm = a} :: DescribeAlarmResponse)

-- | The response's http status code.
describeAlarmResponse_httpStatus :: Lens.Lens' DescribeAlarmResponse Prelude.Int
describeAlarmResponse_httpStatus = Lens.lens (\DescribeAlarmResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmResponse' {} a -> s {httpStatus = a} :: DescribeAlarmResponse)

instance Prelude.NFData DescribeAlarmResponse where
  rnf DescribeAlarmResponse' {..} =
    Prelude.rnf alarm
      `Prelude.seq` Prelude.rnf httpStatus
