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
-- Module      : Amazonka.IoTEvents.CreateAlarmModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alarm model to monitor an AWS IoT Events input attribute. You
-- can use the alarm to get notified when the value is outside a specified
-- range. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/create-alarms.html Create an alarm model>
-- in the /AWS IoT Events Developer Guide/.
module Amazonka.IoTEvents.CreateAlarmModel
  ( -- * Creating a Request
    CreateAlarmModel (..),
    newCreateAlarmModel,

    -- * Request Lenses
    createAlarmModel_tags,
    createAlarmModel_key,
    createAlarmModel_severity,
    createAlarmModel_alarmEventActions,
    createAlarmModel_alarmCapabilities,
    createAlarmModel_alarmModelDescription,
    createAlarmModel_alarmNotification,
    createAlarmModel_alarmModelName,
    createAlarmModel_roleArn,
    createAlarmModel_alarmRule,

    -- * Destructuring the Response
    CreateAlarmModelResponse (..),
    newCreateAlarmModelResponse,

    -- * Response Lenses
    createAlarmModelResponse_alarmModelVersion,
    createAlarmModelResponse_alarmModelArn,
    createAlarmModelResponse_status,
    createAlarmModelResponse_creationTime,
    createAlarmModelResponse_lastUpdateTime,
    createAlarmModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAlarmModel' smart constructor.
data CreateAlarmModel = CreateAlarmModel'
  { -- | A list of key-value pairs that contain metadata for the alarm model. The
    -- tags help you manage the alarm model. For more information, see
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/tagging-iotevents.html Tagging your AWS IoT Events resources>
    -- in the /AWS IoT Events Developer Guide/.
    --
    -- You can create up to 50 tags for one alarm model.
    tags :: Prelude.Maybe [Tag],
    -- | An input attribute used as a key to create an alarm. AWS IoT Events
    -- routes
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
    -- associated with this key to the alarm.
    key :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer that reflects the severity level of the alarm.
    severity :: Prelude.Maybe Prelude.Natural,
    -- | Contains information about one or more alarm actions.
    alarmEventActions :: Prelude.Maybe AlarmEventActions,
    -- | Contains the configuration information of alarm state changes.
    alarmCapabilities :: Prelude.Maybe AlarmCapabilities,
    -- | A description that tells you what the alarm model detects.
    alarmModelDescription :: Prelude.Maybe Prelude.Text,
    -- | Contains information about one or more notification actions.
    alarmNotification :: Prelude.Maybe AlarmNotification,
    -- | A unique name that helps you identify the alarm model. You can\'t change
    -- this name after you create the alarm model.
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
-- Create a value of 'CreateAlarmModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAlarmModel_tags' - A list of key-value pairs that contain metadata for the alarm model. The
-- tags help you manage the alarm model. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/tagging-iotevents.html Tagging your AWS IoT Events resources>
-- in the /AWS IoT Events Developer Guide/.
--
-- You can create up to 50 tags for one alarm model.
--
-- 'key', 'createAlarmModel_key' - An input attribute used as a key to create an alarm. AWS IoT Events
-- routes
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
-- associated with this key to the alarm.
--
-- 'severity', 'createAlarmModel_severity' - A non-negative integer that reflects the severity level of the alarm.
--
-- 'alarmEventActions', 'createAlarmModel_alarmEventActions' - Contains information about one or more alarm actions.
--
-- 'alarmCapabilities', 'createAlarmModel_alarmCapabilities' - Contains the configuration information of alarm state changes.
--
-- 'alarmModelDescription', 'createAlarmModel_alarmModelDescription' - A description that tells you what the alarm model detects.
--
-- 'alarmNotification', 'createAlarmModel_alarmNotification' - Contains information about one or more notification actions.
--
-- 'alarmModelName', 'createAlarmModel_alarmModelName' - A unique name that helps you identify the alarm model. You can\'t change
-- this name after you create the alarm model.
--
-- 'roleArn', 'createAlarmModel_roleArn' - The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'alarmRule', 'createAlarmModel_alarmRule' - Defines when your alarm is invoked.
newCreateAlarmModel ::
  -- | 'alarmModelName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'alarmRule'
  AlarmRule ->
  CreateAlarmModel
newCreateAlarmModel
  pAlarmModelName_
  pRoleArn_
  pAlarmRule_ =
    CreateAlarmModel'
      { tags = Prelude.Nothing,
        key = Prelude.Nothing,
        severity = Prelude.Nothing,
        alarmEventActions = Prelude.Nothing,
        alarmCapabilities = Prelude.Nothing,
        alarmModelDescription = Prelude.Nothing,
        alarmNotification = Prelude.Nothing,
        alarmModelName = pAlarmModelName_,
        roleArn = pRoleArn_,
        alarmRule = pAlarmRule_
      }

-- | A list of key-value pairs that contain metadata for the alarm model. The
-- tags help you manage the alarm model. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/tagging-iotevents.html Tagging your AWS IoT Events resources>
-- in the /AWS IoT Events Developer Guide/.
--
-- You can create up to 50 tags for one alarm model.
createAlarmModel_tags :: Lens.Lens' CreateAlarmModel (Prelude.Maybe [Tag])
createAlarmModel_tags = Lens.lens (\CreateAlarmModel' {tags} -> tags) (\s@CreateAlarmModel' {} a -> s {tags = a} :: CreateAlarmModel) Prelude.. Lens.mapping Lens.coerced

-- | An input attribute used as a key to create an alarm. AWS IoT Events
-- routes
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Input.html inputs>
-- associated with this key to the alarm.
createAlarmModel_key :: Lens.Lens' CreateAlarmModel (Prelude.Maybe Prelude.Text)
createAlarmModel_key = Lens.lens (\CreateAlarmModel' {key} -> key) (\s@CreateAlarmModel' {} a -> s {key = a} :: CreateAlarmModel)

-- | A non-negative integer that reflects the severity level of the alarm.
createAlarmModel_severity :: Lens.Lens' CreateAlarmModel (Prelude.Maybe Prelude.Natural)
createAlarmModel_severity = Lens.lens (\CreateAlarmModel' {severity} -> severity) (\s@CreateAlarmModel' {} a -> s {severity = a} :: CreateAlarmModel)

-- | Contains information about one or more alarm actions.
createAlarmModel_alarmEventActions :: Lens.Lens' CreateAlarmModel (Prelude.Maybe AlarmEventActions)
createAlarmModel_alarmEventActions = Lens.lens (\CreateAlarmModel' {alarmEventActions} -> alarmEventActions) (\s@CreateAlarmModel' {} a -> s {alarmEventActions = a} :: CreateAlarmModel)

-- | Contains the configuration information of alarm state changes.
createAlarmModel_alarmCapabilities :: Lens.Lens' CreateAlarmModel (Prelude.Maybe AlarmCapabilities)
createAlarmModel_alarmCapabilities = Lens.lens (\CreateAlarmModel' {alarmCapabilities} -> alarmCapabilities) (\s@CreateAlarmModel' {} a -> s {alarmCapabilities = a} :: CreateAlarmModel)

-- | A description that tells you what the alarm model detects.
createAlarmModel_alarmModelDescription :: Lens.Lens' CreateAlarmModel (Prelude.Maybe Prelude.Text)
createAlarmModel_alarmModelDescription = Lens.lens (\CreateAlarmModel' {alarmModelDescription} -> alarmModelDescription) (\s@CreateAlarmModel' {} a -> s {alarmModelDescription = a} :: CreateAlarmModel)

-- | Contains information about one or more notification actions.
createAlarmModel_alarmNotification :: Lens.Lens' CreateAlarmModel (Prelude.Maybe AlarmNotification)
createAlarmModel_alarmNotification = Lens.lens (\CreateAlarmModel' {alarmNotification} -> alarmNotification) (\s@CreateAlarmModel' {} a -> s {alarmNotification = a} :: CreateAlarmModel)

-- | A unique name that helps you identify the alarm model. You can\'t change
-- this name after you create the alarm model.
createAlarmModel_alarmModelName :: Lens.Lens' CreateAlarmModel Prelude.Text
createAlarmModel_alarmModelName = Lens.lens (\CreateAlarmModel' {alarmModelName} -> alarmModelName) (\s@CreateAlarmModel' {} a -> s {alarmModelName = a} :: CreateAlarmModel)

-- | The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
createAlarmModel_roleArn :: Lens.Lens' CreateAlarmModel Prelude.Text
createAlarmModel_roleArn = Lens.lens (\CreateAlarmModel' {roleArn} -> roleArn) (\s@CreateAlarmModel' {} a -> s {roleArn = a} :: CreateAlarmModel)

-- | Defines when your alarm is invoked.
createAlarmModel_alarmRule :: Lens.Lens' CreateAlarmModel AlarmRule
createAlarmModel_alarmRule = Lens.lens (\CreateAlarmModel' {alarmRule} -> alarmRule) (\s@CreateAlarmModel' {} a -> s {alarmRule = a} :: CreateAlarmModel)

instance Core.AWSRequest CreateAlarmModel where
  type
    AWSResponse CreateAlarmModel =
      CreateAlarmModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAlarmModelResponse'
            Prelude.<$> (x Data..?> "alarmModelVersion")
            Prelude.<*> (x Data..?> "alarmModelArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "lastUpdateTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAlarmModel where
  hashWithSalt _salt CreateAlarmModel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` alarmEventActions
      `Prelude.hashWithSalt` alarmCapabilities
      `Prelude.hashWithSalt` alarmModelDescription
      `Prelude.hashWithSalt` alarmNotification
      `Prelude.hashWithSalt` alarmModelName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` alarmRule

instance Prelude.NFData CreateAlarmModel where
  rnf CreateAlarmModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf alarmEventActions
      `Prelude.seq` Prelude.rnf alarmCapabilities
      `Prelude.seq` Prelude.rnf alarmModelDescription
      `Prelude.seq` Prelude.rnf alarmNotification
      `Prelude.seq` Prelude.rnf alarmModelName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf alarmRule

instance Data.ToHeaders CreateAlarmModel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAlarmModel where
  toJSON CreateAlarmModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("key" Data..=) Prelude.<$> key,
            ("severity" Data..=) Prelude.<$> severity,
            ("alarmEventActions" Data..=)
              Prelude.<$> alarmEventActions,
            ("alarmCapabilities" Data..=)
              Prelude.<$> alarmCapabilities,
            ("alarmModelDescription" Data..=)
              Prelude.<$> alarmModelDescription,
            ("alarmNotification" Data..=)
              Prelude.<$> alarmNotification,
            Prelude.Just
              ("alarmModelName" Data..= alarmModelName),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("alarmRule" Data..= alarmRule)
          ]
      )

instance Data.ToPath CreateAlarmModel where
  toPath = Prelude.const "/alarm-models"

instance Data.ToQuery CreateAlarmModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAlarmModelResponse' smart constructor.
data CreateAlarmModelResponse = CreateAlarmModelResponse'
  { -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the alarm model. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    alarmModelArn :: Prelude.Maybe Prelude.Text,
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
    -- | The time the alarm model was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time the alarm model was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAlarmModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmModelVersion', 'createAlarmModelResponse_alarmModelVersion' - The version of the alarm model.
--
-- 'alarmModelArn', 'createAlarmModelResponse_alarmModelArn' - The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'status', 'createAlarmModelResponse_status' - The status of the alarm model. The status can be one of the following
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
-- 'creationTime', 'createAlarmModelResponse_creationTime' - The time the alarm model was created, in the Unix epoch format.
--
-- 'lastUpdateTime', 'createAlarmModelResponse_lastUpdateTime' - The time the alarm model was last updated, in the Unix epoch format.
--
-- 'httpStatus', 'createAlarmModelResponse_httpStatus' - The response's http status code.
newCreateAlarmModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAlarmModelResponse
newCreateAlarmModelResponse pHttpStatus_ =
  CreateAlarmModelResponse'
    { alarmModelVersion =
        Prelude.Nothing,
      alarmModelArn = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the alarm model.
createAlarmModelResponse_alarmModelVersion :: Lens.Lens' CreateAlarmModelResponse (Prelude.Maybe Prelude.Text)
createAlarmModelResponse_alarmModelVersion = Lens.lens (\CreateAlarmModelResponse' {alarmModelVersion} -> alarmModelVersion) (\s@CreateAlarmModelResponse' {} a -> s {alarmModelVersion = a} :: CreateAlarmModelResponse)

-- | The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
createAlarmModelResponse_alarmModelArn :: Lens.Lens' CreateAlarmModelResponse (Prelude.Maybe Prelude.Text)
createAlarmModelResponse_alarmModelArn = Lens.lens (\CreateAlarmModelResponse' {alarmModelArn} -> alarmModelArn) (\s@CreateAlarmModelResponse' {} a -> s {alarmModelArn = a} :: CreateAlarmModelResponse)

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
createAlarmModelResponse_status :: Lens.Lens' CreateAlarmModelResponse (Prelude.Maybe AlarmModelVersionStatus)
createAlarmModelResponse_status = Lens.lens (\CreateAlarmModelResponse' {status} -> status) (\s@CreateAlarmModelResponse' {} a -> s {status = a} :: CreateAlarmModelResponse)

-- | The time the alarm model was created, in the Unix epoch format.
createAlarmModelResponse_creationTime :: Lens.Lens' CreateAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
createAlarmModelResponse_creationTime = Lens.lens (\CreateAlarmModelResponse' {creationTime} -> creationTime) (\s@CreateAlarmModelResponse' {} a -> s {creationTime = a} :: CreateAlarmModelResponse) Prelude.. Lens.mapping Data._Time

-- | The time the alarm model was last updated, in the Unix epoch format.
createAlarmModelResponse_lastUpdateTime :: Lens.Lens' CreateAlarmModelResponse (Prelude.Maybe Prelude.UTCTime)
createAlarmModelResponse_lastUpdateTime = Lens.lens (\CreateAlarmModelResponse' {lastUpdateTime} -> lastUpdateTime) (\s@CreateAlarmModelResponse' {} a -> s {lastUpdateTime = a} :: CreateAlarmModelResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
createAlarmModelResponse_httpStatus :: Lens.Lens' CreateAlarmModelResponse Prelude.Int
createAlarmModelResponse_httpStatus = Lens.lens (\CreateAlarmModelResponse' {httpStatus} -> httpStatus) (\s@CreateAlarmModelResponse' {} a -> s {httpStatus = a} :: CreateAlarmModelResponse)

instance Prelude.NFData CreateAlarmModelResponse where
  rnf CreateAlarmModelResponse' {..} =
    Prelude.rnf alarmModelVersion
      `Prelude.seq` Prelude.rnf alarmModelArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf httpStatus
