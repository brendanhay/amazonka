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
-- Module      : Amazonka.CloudWatch.SetAlarmState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Temporarily sets the state of an alarm for testing purposes. When the
-- updated state differs from the previous value, the action configured for
-- the appropriate state is invoked. For example, if your alarm is
-- configured to send an Amazon SNS message when an alarm is triggered,
-- temporarily changing the alarm state to @ALARM@ sends an SNS message.
--
-- Metric alarms returns to their actual state quickly, often within
-- seconds. Because the metric alarm state change happens quickly, it is
-- typically only visible in the alarm\'s __History__ tab in the Amazon
-- CloudWatch console or through
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarmHistory.html DescribeAlarmHistory>.
--
-- If you use @SetAlarmState@ on a composite alarm, the composite alarm is
-- not guaranteed to return to its actual state. It returns to its actual
-- state only once any of its children alarms change state. It is also
-- reevaluated if you update its configuration.
--
-- If an alarm triggers EC2 Auto Scaling policies or application Auto
-- Scaling policies, you must include information in the @StateReasonData@
-- parameter to enable the policy to take the correct action.
module Amazonka.CloudWatch.SetAlarmState
  ( -- * Creating a Request
    SetAlarmState (..),
    newSetAlarmState,

    -- * Request Lenses
    setAlarmState_stateReasonData,
    setAlarmState_alarmName,
    setAlarmState_stateValue,
    setAlarmState_stateReason,

    -- * Destructuring the Response
    SetAlarmStateResponse (..),
    newSetAlarmStateResponse,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetAlarmState' smart constructor.
data SetAlarmState = SetAlarmState'
  { -- | The reason that this alarm is set to this specific state, in JSON
    -- format.
    --
    -- For SNS or EC2 alarm actions, this is just informational. But for EC2
    -- Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling
    -- policy uses the information in this field to take the correct action.
    stateReasonData :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm.
    alarmName :: Prelude.Text,
    -- | The value of the state.
    stateValue :: StateValue,
    -- | The reason that this alarm is set to this specific state, in text
    -- format.
    stateReason :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetAlarmState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateReasonData', 'setAlarmState_stateReasonData' - The reason that this alarm is set to this specific state, in JSON
-- format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2
-- Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling
-- policy uses the information in this field to take the correct action.
--
-- 'alarmName', 'setAlarmState_alarmName' - The name of the alarm.
--
-- 'stateValue', 'setAlarmState_stateValue' - The value of the state.
--
-- 'stateReason', 'setAlarmState_stateReason' - The reason that this alarm is set to this specific state, in text
-- format.
newSetAlarmState ::
  -- | 'alarmName'
  Prelude.Text ->
  -- | 'stateValue'
  StateValue ->
  -- | 'stateReason'
  Prelude.Text ->
  SetAlarmState
newSetAlarmState
  pAlarmName_
  pStateValue_
  pStateReason_ =
    SetAlarmState'
      { stateReasonData = Prelude.Nothing,
        alarmName = pAlarmName_,
        stateValue = pStateValue_,
        stateReason = pStateReason_
      }

-- | The reason that this alarm is set to this specific state, in JSON
-- format.
--
-- For SNS or EC2 alarm actions, this is just informational. But for EC2
-- Auto Scaling or application Auto Scaling alarm actions, the Auto Scaling
-- policy uses the information in this field to take the correct action.
setAlarmState_stateReasonData :: Lens.Lens' SetAlarmState (Prelude.Maybe Prelude.Text)
setAlarmState_stateReasonData = Lens.lens (\SetAlarmState' {stateReasonData} -> stateReasonData) (\s@SetAlarmState' {} a -> s {stateReasonData = a} :: SetAlarmState)

-- | The name of the alarm.
setAlarmState_alarmName :: Lens.Lens' SetAlarmState Prelude.Text
setAlarmState_alarmName = Lens.lens (\SetAlarmState' {alarmName} -> alarmName) (\s@SetAlarmState' {} a -> s {alarmName = a} :: SetAlarmState)

-- | The value of the state.
setAlarmState_stateValue :: Lens.Lens' SetAlarmState StateValue
setAlarmState_stateValue = Lens.lens (\SetAlarmState' {stateValue} -> stateValue) (\s@SetAlarmState' {} a -> s {stateValue = a} :: SetAlarmState)

-- | The reason that this alarm is set to this specific state, in text
-- format.
setAlarmState_stateReason :: Lens.Lens' SetAlarmState Prelude.Text
setAlarmState_stateReason = Lens.lens (\SetAlarmState' {stateReason} -> stateReason) (\s@SetAlarmState' {} a -> s {stateReason = a} :: SetAlarmState)

instance Core.AWSRequest SetAlarmState where
  type
    AWSResponse SetAlarmState =
      SetAlarmStateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull SetAlarmStateResponse'

instance Prelude.Hashable SetAlarmState where
  hashWithSalt _salt SetAlarmState' {..} =
    _salt
      `Prelude.hashWithSalt` stateReasonData
      `Prelude.hashWithSalt` alarmName
      `Prelude.hashWithSalt` stateValue
      `Prelude.hashWithSalt` stateReason

instance Prelude.NFData SetAlarmState where
  rnf SetAlarmState' {..} =
    Prelude.rnf stateReasonData
      `Prelude.seq` Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf stateValue
      `Prelude.seq` Prelude.rnf stateReason

instance Data.ToHeaders SetAlarmState where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetAlarmState where
  toPath = Prelude.const "/"

instance Data.ToQuery SetAlarmState where
  toQuery SetAlarmState' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetAlarmState" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "StateReasonData" Data.=: stateReasonData,
        "AlarmName" Data.=: alarmName,
        "StateValue" Data.=: stateValue,
        "StateReason" Data.=: stateReason
      ]

-- | /See:/ 'newSetAlarmStateResponse' smart constructor.
data SetAlarmStateResponse = SetAlarmStateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetAlarmStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetAlarmStateResponse ::
  SetAlarmStateResponse
newSetAlarmStateResponse = SetAlarmStateResponse'

instance Prelude.NFData SetAlarmStateResponse where
  rnf _ = ()
