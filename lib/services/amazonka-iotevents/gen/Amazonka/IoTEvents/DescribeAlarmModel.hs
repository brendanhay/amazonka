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
-- Module      : Amazonka.IoTEvents.DescribeAlarmModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an alarm model. If you don\'t specify a
-- value for the @alarmModelVersion@ parameter, the latest version is
-- returned.
module Amazonka.IoTEvents.DescribeAlarmModel
  ( -- * Creating a Request
    DescribeAlarmModel (..),
    newDescribeAlarmModel,

    -- * Request Lenses
    describeAlarmModel_alarmModelVersion,
    describeAlarmModel_alarmModelName,

    -- * Destructuring the Response
    DescribeAlarmModelResponse (..),
    newDescribeAlarmModelResponse,

    -- * Response Lenses
    describeAlarmModelResponse_alarmCapabilities,
    describeAlarmModelResponse_alarmEventActions,
    describeAlarmModelResponse_alarmModelArn,
    describeAlarmModelResponse_alarmModelDescription,
    describeAlarmModelResponse_alarmModelName,
    describeAlarmModelResponse_alarmModelVersion,
    describeAlarmModelResponse_alarmNotification,
    describeAlarmModelResponse_alarmRule,
    describeAlarmModelResponse_creationTime,
    describeAlarmModelResponse_key,
    describeAlarmModelResponse_lastUpdateTime,
    describeAlarmModelResponse_roleArn,
    describeAlarmModelResponse_severity,
    describeAlarmModelResponse_status,
    describeAlarmModelResponse_statusMessage,
    describeAlarmModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAlarmModel' smart constructor.
data DescribeAlarmModel = DescribeAlarmModel'
  { -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarmModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelVersion', 'describeAlarmModel_alarmModelVersion' - The version of the alarm model.
--
-- 'alarmModelName', 'describeAlarmModel_alarmModelName' - The name of the alarm model.
newDescribeAlarmModel ::
  -- | 'alarmModelName'
  Prelude.Text ->
  DescribeAlarmModel
newDescribeAlarmModel pAlarmModelName_ =
  DescribeAlarmModel'
    { alarmModelVersion =
        Prelude.Nothing,
      alarmModelName = pAlarmModelName_
    }

-- | The version of the alarm model.
describeAlarmModel_alarmModelVersion :: Lens.Lens' DescribeAlarmModel (Prelude.Maybe Prelude.Text)
describeAlarmModel_alarmModelVersion = Lens.lens (\DescribeAlarmModel' {alarmModelVersion} -> alarmModelVersion) (\s@DescribeAlarmModel' {} a -> s {alarmModelVersion = a} :: DescribeAlarmModel)

-- | The name of the alarm model.
describeAlarmModel_alarmModelName :: Lens.Lens' DescribeAlarmModel Prelude.Text
describeAlarmModel_alarmModelName = Lens.lens (\DescribeAlarmModel' {alarmModelName} -> alarmModelName) (\s@DescribeAlarmModel' {} a -> s {alarmModelName = a} :: DescribeAlarmModel)

instance Core.AWSRequest DescribeAlarmModel where
  type
    AWSResponse DescribeAlarmModel =
      DescribeAlarmModelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAlarmModelResponse'
            Prelude.<$> (x Data..?> "alarmCapabilities")
            Prelude.<*> (x Data..?> "alarmEventActions")
            Prelude.<*> (x Data..?> "alarmModelArn")
            Prelude.<*> (x Data..?> "alarmModelDescription")
            Prelude.<*> (x Data..?> "alarmModelName")
            Prelude.<*> (x Data..?> "alarmModelVersion")
            Prelude.<*> (x Data..?> "alarmNotification")
            Prelude.<*> (x Data..?> "alarmRule")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "key")
            Prelude.<*> (x Data..?> "lastUpdateTime")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "severity")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAlarmModel where
  hashWithSalt _salt DescribeAlarmModel' {..} =
    _salt `Prelude.hashWithSalt` alarmModelVersion
      `Prelude.hashWithSalt` alarmModelName

instance Prelude.NFData DescribeAlarmModel where
  rnf DescribeAlarmModel' {..} =
    Prelude.rnf alarmModelVersion
      `Prelude.seq` Prelude.rnf alarmModelName

instance Data.ToHeaders DescribeAlarmModel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAlarmModel where
  toPath DescribeAlarmModel' {..} =
    Prelude.mconcat
      ["/alarm-models/", Data.toBS alarmModelName]

instance Data.ToQuery DescribeAlarmModel where
  toQuery DescribeAlarmModel' {..} =
    Prelude.mconcat
      ["version" Data.=: alarmModelVersion]

-- | /See:/ 'newDescribeAlarmModelResponse' smart constructor.
data DescribeAlarmModelResponse = DescribeAlarmModelResponse'
  { -- | Contains the configuration information of alarm state changes.
    alarmCapabilities :: Prelude.Maybe AlarmCapabilities,
    -- | Contains information about one or more alarm actions.
    alarmEventActions :: Prelude.Maybe AlarmEventActions,
    -- | The ARN of the alarm model. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    alarmModelArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the alarm model.
    alarmModelDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Maybe Prelude.Text,
    -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Contains information about one or more notification actions.
    alarmNotification :: Prelude.Maybe AlarmNotification,
    -- | Defines when your alarm is invoked.
    alarmRule :: Prelude.Maybe AlarmRule,
    -- | The time the alarm model was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | An input attribute used as a key to create an alarm. AWS IoT Events
    -- routes
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
    -- associated with this key to the alarm.
    key :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm model was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the IAM role that allows the alarm to perform actions and
    -- access AWS resources. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer that reflects the severity level of the alarm.
    severity :: Prelude.Maybe Prelude.Natural,
    -- | The status of the alarm model. The status can be one of the following
    -- values:
    --
    -- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
    --     data.
    --
    -- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
    --     Activating an alarm model can take up to a few minutes.
    --
    -- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
    --     evaluate data. Check your alarm model information and update the
    --     alarm model.
    --
    -- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
    --     your alarm model information and try again.
    status :: Prelude.Maybe AlarmModelVersionStatus,
    -- | Contains information about the status of the alarm model.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAlarmModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmCapabilities', 'describeAlarmModelResponse_alarmCapabilities' - Contains the configuration information of alarm state changes.
--
-- 'alarmEventActions', 'describeAlarmModelResponse_alarmEventActions' - Contains information about one or more alarm actions.
--
-- 'alarmModelArn', 'describeAlarmModelResponse_alarmModelArn' - The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'alarmModelDescription', 'describeAlarmModelResponse_alarmModelDescription' - The description of the alarm model.
--
-- 'alarmModelName', 'describeAlarmModelResponse_alarmModelName' - The name of the alarm model.
--
-- 'alarmModelVersion', 'describeAlarmModelResponse_alarmModelVersion' - The version of the alarm model.
--
-- 'alarmNotification', 'describeAlarmModelResponse_alarmNotification' - Contains information about one or more notification actions.
--
-- 'alarmRule', 'describeAlarmModelResponse_alarmRule' - Defines when your alarm is invoked.
--
-- 'creationTime', 'describeAlarmModelResponse_creationTime' - The time the alarm model was created, in the Unix epoch format.
--
-- 'key', 'describeAlarmModelResponse_key' - An input attribute used as a key to create an alarm. AWS IoT Events
-- routes
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
-- associated with this key to the alarm.
--
-- 'lastUpdateTime', 'describeAlarmModelResponse_lastUpdateTime' - The time the alarm model was last updated, in the Unix epoch format.
--
-- 'roleArn', 'describeAlarmModelResponse_roleArn' - The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'severity', 'describeAlarmModelResponse_severity' - A non-negative integer that reflects the severity level of the alarm.
--
-- 'status', 'describeAlarmModelResponse_status' - The status of the alarm model. The status can be one of the following
-- values:
--
-- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
--     data.
--
-- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
--     Activating an alarm model can take up to a few minutes.
--
-- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
--     evaluate data. Check your alarm model information and update the
--     alarm model.
--
-- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
--     your alarm model information and try again.
--
-- 'statusMessage', 'describeAlarmModelResponse_statusMessage' - Contains information about the status of the alarm model.
--
-- 'httpStatus', 'describeAlarmModelResponse_httpStatus' - The response's http status code.
newDescribeAlarmModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAlarmModelResponse
newDescribeAlarmModelResponse pHttpStatus_ =
  DescribeAlarmModelResponse'
    { alarmCapabilities =
        Prelude.Nothing,
      alarmEventActions = Prelude.Nothing,
      alarmModelArn = Prelude.Nothing,
      alarmModelDescription = Prelude.Nothing,
      alarmModelName = Prelude.Nothing,
      alarmModelVersion = Prelude.Nothing,
      alarmNotification = Prelude.Nothing,
      alarmRule = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      key = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      severity = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the configuration information of alarm state changes.
describeAlarmModelResponse_alarmCapabilities :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe AlarmCapabilities)
describeAlarmModelResponse_alarmCapabilities = Lens.lens (\DescribeAlarmModelResponse' {alarmCapabilities} -> alarmCapabilities) (\s@DescribeAlarmModelResponse' {} a -> s {alarmCapabilities = a} :: DescribeAlarmModelResponse)

-- | Contains information about one or more alarm actions.
describeAlarmModelResponse_alarmEventActions :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe AlarmEventActions)
describeAlarmModelResponse_alarmEventActions = Lens.lens (\DescribeAlarmModelResponse' {alarmEventActions} -> alarmEventActions) (\s@DescribeAlarmModelResponse' {} a -> s {alarmEventActions = a} :: DescribeAlarmModelResponse)

-- | The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAlarmModelResponse_alarmModelArn :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_alarmModelArn = Lens.lens (\DescribeAlarmModelResponse' {alarmModelArn} -> alarmModelArn) (\s@DescribeAlarmModelResponse' {} a -> s {alarmModelArn = a} :: DescribeAlarmModelResponse)

-- | The description of the alarm model.
describeAlarmModelResponse_alarmModelDescription :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_alarmModelDescription = Lens.lens (\DescribeAlarmModelResponse' {alarmModelDescription} -> alarmModelDescription) (\s@DescribeAlarmModelResponse' {} a -> s {alarmModelDescription = a} :: DescribeAlarmModelResponse)

-- | The name of the alarm model.
describeAlarmModelResponse_alarmModelName :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_alarmModelName = Lens.lens (\DescribeAlarmModelResponse' {alarmModelName} -> alarmModelName) (\s@DescribeAlarmModelResponse' {} a -> s {alarmModelName = a} :: DescribeAlarmModelResponse)

-- | The version of the alarm model.
describeAlarmModelResponse_alarmModelVersion :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_alarmModelVersion = Lens.lens (\DescribeAlarmModelResponse' {alarmModelVersion} -> alarmModelVersion) (\s@DescribeAlarmModelResponse' {} a -> s {alarmModelVersion = a} :: DescribeAlarmModelResponse)

-- | Contains information about one or more notification actions.
describeAlarmModelResponse_alarmNotification :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe AlarmNotification)
describeAlarmModelResponse_alarmNotification = Lens.lens (\DescribeAlarmModelResponse' {alarmNotification} -> alarmNotification) (\s@DescribeAlarmModelResponse' {} a -> s {alarmNotification = a} :: DescribeAlarmModelResponse)

-- | Defines when your alarm is invoked.
describeAlarmModelResponse_alarmRule :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe AlarmRule)
describeAlarmModelResponse_alarmRule = Lens.lens (\DescribeAlarmModelResponse' {alarmRule} -> alarmRule) (\s@DescribeAlarmModelResponse' {} a -> s {alarmRule = a} :: DescribeAlarmModelResponse)

-- | The time the alarm model was created, in the Unix epoch format.
describeAlarmModelResponse_creationTime :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
describeAlarmModelResponse_creationTime = Lens.lens (\DescribeAlarmModelResponse' {creationTime} -> creationTime) (\s@DescribeAlarmModelResponse' {} a -> s {creationTime = a} :: DescribeAlarmModelResponse) Prelude.. Lens.mapping Data._Time

-- | An input attribute used as a key to create an alarm. AWS IoT Events
-- routes
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
-- associated with this key to the alarm.
describeAlarmModelResponse_key :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_key = Lens.lens (\DescribeAlarmModelResponse' {key} -> key) (\s@DescribeAlarmModelResponse' {} a -> s {key = a} :: DescribeAlarmModelResponse)

-- | The time the alarm model was last updated, in the Unix epoch format.
describeAlarmModelResponse_lastUpdateTime :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
describeAlarmModelResponse_lastUpdateTime = Lens.lens (\DescribeAlarmModelResponse' {lastUpdateTime} -> lastUpdateTime) (\s@DescribeAlarmModelResponse' {} a -> s {lastUpdateTime = a} :: DescribeAlarmModelResponse) Prelude.. Lens.mapping Data._Time

-- | The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
describeAlarmModelResponse_roleArn :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_roleArn = Lens.lens (\DescribeAlarmModelResponse' {roleArn} -> roleArn) (\s@DescribeAlarmModelResponse' {} a -> s {roleArn = a} :: DescribeAlarmModelResponse)

-- | A non-negative integer that reflects the severity level of the alarm.
describeAlarmModelResponse_severity :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Natural)
describeAlarmModelResponse_severity = Lens.lens (\DescribeAlarmModelResponse' {severity} -> severity) (\s@DescribeAlarmModelResponse' {} a -> s {severity = a} :: DescribeAlarmModelResponse)

-- | The status of the alarm model. The status can be one of the following
-- values:
--
-- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
--     data.
--
-- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
--     Activating an alarm model can take up to a few minutes.
--
-- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
--     evaluate data. Check your alarm model information and update the
--     alarm model.
--
-- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
--     your alarm model information and try again.
describeAlarmModelResponse_status :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe AlarmModelVersionStatus)
describeAlarmModelResponse_status = Lens.lens (\DescribeAlarmModelResponse' {status} -> status) (\s@DescribeAlarmModelResponse' {} a -> s {status = a} :: DescribeAlarmModelResponse)

-- | Contains information about the status of the alarm model.
describeAlarmModelResponse_statusMessage :: Lens.Lens' DescribeAlarmModelResponse (Prelude.Maybe Prelude.Text)
describeAlarmModelResponse_statusMessage = Lens.lens (\DescribeAlarmModelResponse' {statusMessage} -> statusMessage) (\s@DescribeAlarmModelResponse' {} a -> s {statusMessage = a} :: DescribeAlarmModelResponse)

-- | The response's http status code.
describeAlarmModelResponse_httpStatus :: Lens.Lens' DescribeAlarmModelResponse Prelude.Int
describeAlarmModelResponse_httpStatus = Lens.lens (\DescribeAlarmModelResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmModelResponse' {} a -> s {httpStatus = a} :: DescribeAlarmModelResponse)

instance Prelude.NFData DescribeAlarmModelResponse where
  rnf DescribeAlarmModelResponse' {..} =
    Prelude.rnf alarmCapabilities
      `Prelude.seq` Prelude.rnf alarmEventActions
      `Prelude.seq` Prelude.rnf alarmModelArn
      `Prelude.seq` Prelude.rnf alarmModelDescription
      `Prelude.seq` Prelude.rnf alarmModelName
      `Prelude.seq` Prelude.rnf alarmModelVersion
      `Prelude.seq` Prelude.rnf alarmNotification
      `Prelude.seq` Prelude.rnf alarmRule
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
