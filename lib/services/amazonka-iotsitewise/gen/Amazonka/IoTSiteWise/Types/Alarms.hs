{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.Types.Alarms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Alarms where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of an alarm created in an IoT
-- SiteWise Monitor portal. You can use the alarm to monitor an asset
-- property and get notified when the asset property value is outside a
-- specified range. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/appguide/monitor-alarms.html Monitoring with alarms>
-- in the /IoT SiteWise Application Guide/.
--
-- /See:/ 'newAlarms' smart constructor.
data Alarms = Alarms'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the Lambda function that manages alarm notifications. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html Managing alarm notifications>
    -- in the /IoT Events Developer Guide/.
    notificationLambdaArn :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IAM role that allows the alarm to perform actions and access
    -- Amazon Web Services resources and services, such as IoT Events.
    alarmRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alarms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationLambdaArn', 'alarms_notificationLambdaArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the Lambda function that manages alarm notifications. For more
-- information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html Managing alarm notifications>
-- in the /IoT Events Developer Guide/.
--
-- 'alarmRoleArn', 'alarms_alarmRoleArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows the alarm to perform actions and access
-- Amazon Web Services resources and services, such as IoT Events.
newAlarms ::
  -- | 'alarmRoleArn'
  Prelude.Text ->
  Alarms
newAlarms pAlarmRoleArn_ =
  Alarms'
    { notificationLambdaArn = Prelude.Nothing,
      alarmRoleArn = pAlarmRoleArn_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the Lambda function that manages alarm notifications. For more
-- information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/lambda-support.html Managing alarm notifications>
-- in the /IoT Events Developer Guide/.
alarms_notificationLambdaArn :: Lens.Lens' Alarms (Prelude.Maybe Prelude.Text)
alarms_notificationLambdaArn = Lens.lens (\Alarms' {notificationLambdaArn} -> notificationLambdaArn) (\s@Alarms' {} a -> s {notificationLambdaArn = a} :: Alarms)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IAM role that allows the alarm to perform actions and access
-- Amazon Web Services resources and services, such as IoT Events.
alarms_alarmRoleArn :: Lens.Lens' Alarms Prelude.Text
alarms_alarmRoleArn = Lens.lens (\Alarms' {alarmRoleArn} -> alarmRoleArn) (\s@Alarms' {} a -> s {alarmRoleArn = a} :: Alarms)

instance Data.FromJSON Alarms where
  parseJSON =
    Data.withObject
      "Alarms"
      ( \x ->
          Alarms'
            Prelude.<$> (x Data..:? "notificationLambdaArn")
            Prelude.<*> (x Data..: "alarmRoleArn")
      )

instance Prelude.Hashable Alarms where
  hashWithSalt _salt Alarms' {..} =
    _salt
      `Prelude.hashWithSalt` notificationLambdaArn
      `Prelude.hashWithSalt` alarmRoleArn

instance Prelude.NFData Alarms where
  rnf Alarms' {..} =
    Prelude.rnf notificationLambdaArn `Prelude.seq`
      Prelude.rnf alarmRoleArn

instance Data.ToJSON Alarms where
  toJSON Alarms' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("notificationLambdaArn" Data..=)
              Prelude.<$> notificationLambdaArn,
            Prelude.Just ("alarmRoleArn" Data..= alarmRoleArn)
          ]
      )
