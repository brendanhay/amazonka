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
-- Module      : Amazonka.IoTEvents.UpdateAlarmModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an alarm model. Any alarms that were created based on the
-- previous version are deleted and then created again as new data arrives.
module Amazonka.IoTEvents.UpdateAlarmModel
  ( -- * Creating a Request
    UpdateAlarmModel (..),
    newUpdateAlarmModel,

    -- * Request Lenses
    updateAlarmModel_alarmCapabilities,
    updateAlarmModel_alarmEventActions,
    updateAlarmModel_alarmModelDescription,
    updateAlarmModel_alarmNotification,
    updateAlarmModel_severity,
    updateAlarmModel_alarmModelName,
    updateAlarmModel_roleArn,
    updateAlarmModel_alarmRule,

    -- * Destructuring the Response
    UpdateAlarmModelResponse (..),
    newUpdateAlarmModelResponse,

    -- * Response Lenses
    updateAlarmModelResponse_alarmModelArn,
    updateAlarmModelResponse_alarmModelVersion,
    updateAlarmModelResponse_creationTime,
    updateAlarmModelResponse_lastUpdateTime,
    updateAlarmModelResponse_status,
    updateAlarmModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAlarmModel' smart constructor.
data UpdateAlarmModel = UpdateAlarmModel'
  { -- | Contains the configuration information of alarm state changes.
    alarmCapabilities :: Prelude.Maybe AlarmCapabilities,
    -- | Contains information about one or more alarm actions.
    alarmEventActions :: Prelude.Maybe AlarmEventActions,
    -- | The description of the alarm model.
    alarmModelDescription :: Prelude.Maybe Prelude.Text,
    -- | Contains information about one or more notification actions.
    alarmNotification :: Prelude.Maybe AlarmNotification,
    -- | A non-negative integer that reflects the severity level of the alarm.
    severity :: Prelude.Maybe Prelude.Natural,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Text,
    -- | The ARN of the IAM role that allows the alarm to perform actions and
    -- access AWS resources. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    roleArn :: Prelude.Text,
    -- | Defines when your alarm is invoked.
    alarmRule :: AlarmRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlarmModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmCapabilities', 'updateAlarmModel_alarmCapabilities' - Contains the configuration information of alarm state changes.
--
-- 'alarmEventActions', 'updateAlarmModel_alarmEventActions' - Contains information about one or more alarm actions.
--
-- 'alarmModelDescription', 'updateAlarmModel_alarmModelDescription' - The description of the alarm model.
--
-- 'alarmNotification', 'updateAlarmModel_alarmNotification' - Contains information about one or more notification actions.
--
-- 'severity', 'updateAlarmModel_severity' - A non-negative integer that reflects the severity level of the alarm.
--
-- 'alarmModelName', 'updateAlarmModel_alarmModelName' - The name of the alarm model.
--
-- 'roleArn', 'updateAlarmModel_roleArn' - The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'alarmRule', 'updateAlarmModel_alarmRule' - Defines when your alarm is invoked.
newUpdateAlarmModel ::
  -- | 'alarmModelName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'alarmRule'
  AlarmRule ->
  UpdateAlarmModel
newUpdateAlarmModel
  pAlarmModelName_
  pRoleArn_
  pAlarmRule_ =
    UpdateAlarmModel'
      { alarmCapabilities =
          Prelude.Nothing,
        alarmEventActions = Prelude.Nothing,
        alarmModelDescription = Prelude.Nothing,
        alarmNotification = Prelude.Nothing,
        severity = Prelude.Nothing,
        alarmModelName = pAlarmModelName_,
        roleArn = pRoleArn_,
        alarmRule = pAlarmRule_
      }

-- | Contains the configuration information of alarm state changes.
updateAlarmModel_alarmCapabilities :: Lens.Lens' UpdateAlarmModel (Prelude.Maybe AlarmCapabilities)
updateAlarmModel_alarmCapabilities = Lens.lens (\UpdateAlarmModel' {alarmCapabilities} -> alarmCapabilities) (\s@UpdateAlarmModel' {} a -> s {alarmCapabilities = a} :: UpdateAlarmModel)

-- | Contains information about one or more alarm actions.
updateAlarmModel_alarmEventActions :: Lens.Lens' UpdateAlarmModel (Prelude.Maybe AlarmEventActions)
updateAlarmModel_alarmEventActions = Lens.lens (\UpdateAlarmModel' {alarmEventActions} -> alarmEventActions) (\s@UpdateAlarmModel' {} a -> s {alarmEventActions = a} :: UpdateAlarmModel)

-- | The description of the alarm model.
updateAlarmModel_alarmModelDescription :: Lens.Lens' UpdateAlarmModel (Prelude.Maybe Prelude.Text)
updateAlarmModel_alarmModelDescription = Lens.lens (\UpdateAlarmModel' {alarmModelDescription} -> alarmModelDescription) (\s@UpdateAlarmModel' {} a -> s {alarmModelDescription = a} :: UpdateAlarmModel)

-- | Contains information about one or more notification actions.
updateAlarmModel_alarmNotification :: Lens.Lens' UpdateAlarmModel (Prelude.Maybe AlarmNotification)
updateAlarmModel_alarmNotification = Lens.lens (\UpdateAlarmModel' {alarmNotification} -> alarmNotification) (\s@UpdateAlarmModel' {} a -> s {alarmNotification = a} :: UpdateAlarmModel)

-- | A non-negative integer that reflects the severity level of the alarm.
updateAlarmModel_severity :: Lens.Lens' UpdateAlarmModel (Prelude.Maybe Prelude.Natural)
updateAlarmModel_severity = Lens.lens (\UpdateAlarmModel' {severity} -> severity) (\s@UpdateAlarmModel' {} a -> s {severity = a} :: UpdateAlarmModel)

-- | The name of the alarm model.
updateAlarmModel_alarmModelName :: Lens.Lens' UpdateAlarmModel Prelude.Text
updateAlarmModel_alarmModelName = Lens.lens (\UpdateAlarmModel' {alarmModelName} -> alarmModelName) (\s@UpdateAlarmModel' {} a -> s {alarmModelName = a} :: UpdateAlarmModel)

-- | The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateAlarmModel_roleArn :: Lens.Lens' UpdateAlarmModel Prelude.Text
updateAlarmModel_roleArn = Lens.lens (\UpdateAlarmModel' {roleArn} -> roleArn) (\s@UpdateAlarmModel' {} a -> s {roleArn = a} :: UpdateAlarmModel)

-- | Defines when your alarm is invoked.
updateAlarmModel_alarmRule :: Lens.Lens' UpdateAlarmModel AlarmRule
updateAlarmModel_alarmRule = Lens.lens (\UpdateAlarmModel' {alarmRule} -> alarmRule) (\s@UpdateAlarmModel' {} a -> s {alarmRule = a} :: UpdateAlarmModel)

instance Core.AWSRequest UpdateAlarmModel where
  type
    AWSResponse UpdateAlarmModel =
      UpdateAlarmModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAlarmModelResponse'
            Prelude.<$> (x Data..?> "alarmModelArn")
            Prelude.<*> (x Data..?> "alarmModelVersion")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "lastUpdateTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAlarmModel where
  hashWithSalt _salt UpdateAlarmModel' {..} =
    _salt
      `Prelude.hashWithSalt` alarmCapabilities
      `Prelude.hashWithSalt` alarmEventActions
      `Prelude.hashWithSalt` alarmModelDescription
      `Prelude.hashWithSalt` alarmNotification
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` alarmModelName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` alarmRule

instance Prelude.NFData UpdateAlarmModel where
  rnf UpdateAlarmModel' {..} =
    Prelude.rnf alarmCapabilities
      `Prelude.seq` Prelude.rnf alarmEventActions
      `Prelude.seq` Prelude.rnf alarmModelDescription
      `Prelude.seq` Prelude.rnf alarmNotification
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf alarmModelName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf alarmRule

instance Data.ToHeaders UpdateAlarmModel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateAlarmModel where
  toJSON UpdateAlarmModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alarmCapabilities" Data..=)
              Prelude.<$> alarmCapabilities,
            ("alarmEventActions" Data..=)
              Prelude.<$> alarmEventActions,
            ("alarmModelDescription" Data..=)
              Prelude.<$> alarmModelDescription,
            ("alarmNotification" Data..=)
              Prelude.<$> alarmNotification,
            ("severity" Data..=) Prelude.<$> severity,
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("alarmRule" Data..= alarmRule)
          ]
      )

instance Data.ToPath UpdateAlarmModel where
  toPath UpdateAlarmModel' {..} =
    Prelude.mconcat
      ["/alarm-models/", Data.toBS alarmModelName]

instance Data.ToQuery UpdateAlarmModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAlarmModelResponse' smart constructor.
data UpdateAlarmModelResponse = UpdateAlarmModelResponse'
  { -- | The ARN of the alarm model. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    alarmModelArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm model was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time the alarm model was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAlarmModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelArn', 'updateAlarmModelResponse_alarmModelArn' - The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'alarmModelVersion', 'updateAlarmModelResponse_alarmModelVersion' - The version of the alarm model.
--
-- 'creationTime', 'updateAlarmModelResponse_creationTime' - The time the alarm model was created, in the Unix epoch format.
--
-- 'lastUpdateTime', 'updateAlarmModelResponse_lastUpdateTime' - The time the alarm model was last updated, in the Unix epoch format.
--
-- 'status', 'updateAlarmModelResponse_status' - The status of the alarm model. The status can be one of the following
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
-- 'httpStatus', 'updateAlarmModelResponse_httpStatus' - The response's http status code.
newUpdateAlarmModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAlarmModelResponse
newUpdateAlarmModelResponse pHttpStatus_ =
  UpdateAlarmModelResponse'
    { alarmModelArn =
        Prelude.Nothing,
      alarmModelVersion = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
updateAlarmModelResponse_alarmModelArn :: Lens.Lens' UpdateAlarmModelResponse (Prelude.Maybe Prelude.Text)
updateAlarmModelResponse_alarmModelArn = Lens.lens (\UpdateAlarmModelResponse' {alarmModelArn} -> alarmModelArn) (\s@UpdateAlarmModelResponse' {} a -> s {alarmModelArn = a} :: UpdateAlarmModelResponse)

-- | The version of the alarm model.
updateAlarmModelResponse_alarmModelVersion :: Lens.Lens' UpdateAlarmModelResponse (Prelude.Maybe Prelude.Text)
updateAlarmModelResponse_alarmModelVersion = Lens.lens (\UpdateAlarmModelResponse' {alarmModelVersion} -> alarmModelVersion) (\s@UpdateAlarmModelResponse' {} a -> s {alarmModelVersion = a} :: UpdateAlarmModelResponse)

-- | The time the alarm model was created, in the Unix epoch format.
updateAlarmModelResponse_creationTime :: Lens.Lens' UpdateAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
updateAlarmModelResponse_creationTime = Lens.lens (\UpdateAlarmModelResponse' {creationTime} -> creationTime) (\s@UpdateAlarmModelResponse' {} a -> s {creationTime = a} :: UpdateAlarmModelResponse) Prelude.. Lens.mapping Data._Time

-- | The time the alarm model was last updated, in the Unix epoch format.
updateAlarmModelResponse_lastUpdateTime :: Lens.Lens' UpdateAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
updateAlarmModelResponse_lastUpdateTime = Lens.lens (\UpdateAlarmModelResponse' {lastUpdateTime} -> lastUpdateTime) (\s@UpdateAlarmModelResponse' {} a -> s {lastUpdateTime = a} :: UpdateAlarmModelResponse) Prelude.. Lens.mapping Data._Time

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
updateAlarmModelResponse_status :: Lens.Lens' UpdateAlarmModelResponse (Prelude.Maybe AlarmModelVersionStatus)
updateAlarmModelResponse_status = Lens.lens (\UpdateAlarmModelResponse' {status} -> status) (\s@UpdateAlarmModelResponse' {} a -> s {status = a} :: UpdateAlarmModelResponse)

-- | The response's http status code.
updateAlarmModelResponse_httpStatus :: Lens.Lens' UpdateAlarmModelResponse Prelude.Int
updateAlarmModelResponse_httpStatus = Lens.lens (\UpdateAlarmModelResponse' {httpStatus} -> httpStatus) (\s@UpdateAlarmModelResponse' {} a -> s {httpStatus = a} :: UpdateAlarmModelResponse)

instance Prelude.NFData UpdateAlarmModelResponse where
  rnf UpdateAlarmModelResponse' {..} =
    Prelude.rnf alarmModelArn
      `Prelude.seq` Prelude.rnf alarmModelVersion
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
