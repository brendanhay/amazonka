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
-- Module      : Amazonka.IoTEvents.Types.AlarmAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.DynamoDBAction
import Amazonka.IoTEvents.Types.DynamoDBv2Action
import Amazonka.IoTEvents.Types.FirehoseAction
import Amazonka.IoTEvents.Types.IotEventsAction
import Amazonka.IoTEvents.Types.IotSiteWiseAction
import Amazonka.IoTEvents.Types.IotTopicPublishAction
import Amazonka.IoTEvents.Types.LambdaAction
import Amazonka.IoTEvents.Types.SNSTopicPublishAction
import Amazonka.IoTEvents.Types.SqsAction
import qualified Amazonka.Prelude as Prelude

-- | Specifies one of the following actions to receive notifications when the
-- alarm state changes.
--
-- /See:/ 'newAlarmAction' smart constructor.
data AlarmAction = AlarmAction'
  { dynamoDB :: Prelude.Maybe DynamoDBAction,
    dynamoDBv2 :: Prelude.Maybe DynamoDBv2Action,
    firehose :: Prelude.Maybe FirehoseAction,
    iotEvents :: Prelude.Maybe IotEventsAction,
    iotSiteWise :: Prelude.Maybe IotSiteWiseAction,
    iotTopicPublish :: Prelude.Maybe IotTopicPublishAction,
    lambda :: Prelude.Maybe LambdaAction,
    sns :: Prelude.Maybe SNSTopicPublishAction,
    sqs :: Prelude.Maybe SqsAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamoDB', 'alarmAction_dynamoDB' - Undocumented member.
--
-- 'dynamoDBv2', 'alarmAction_dynamoDBv2' - Undocumented member.
--
-- 'firehose', 'alarmAction_firehose' - Undocumented member.
--
-- 'iotEvents', 'alarmAction_iotEvents' - Undocumented member.
--
-- 'iotSiteWise', 'alarmAction_iotSiteWise' - Undocumented member.
--
-- 'iotTopicPublish', 'alarmAction_iotTopicPublish' - Undocumented member.
--
-- 'lambda', 'alarmAction_lambda' - Undocumented member.
--
-- 'sns', 'alarmAction_sns' - Undocumented member.
--
-- 'sqs', 'alarmAction_sqs' - Undocumented member.
newAlarmAction ::
  AlarmAction
newAlarmAction =
  AlarmAction'
    { dynamoDB = Prelude.Nothing,
      dynamoDBv2 = Prelude.Nothing,
      firehose = Prelude.Nothing,
      iotEvents = Prelude.Nothing,
      iotSiteWise = Prelude.Nothing,
      iotTopicPublish = Prelude.Nothing,
      lambda = Prelude.Nothing,
      sns = Prelude.Nothing,
      sqs = Prelude.Nothing
    }

-- | Undocumented member.
alarmAction_dynamoDB :: Lens.Lens' AlarmAction (Prelude.Maybe DynamoDBAction)
alarmAction_dynamoDB = Lens.lens (\AlarmAction' {dynamoDB} -> dynamoDB) (\s@AlarmAction' {} a -> s {dynamoDB = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_dynamoDBv2 :: Lens.Lens' AlarmAction (Prelude.Maybe DynamoDBv2Action)
alarmAction_dynamoDBv2 = Lens.lens (\AlarmAction' {dynamoDBv2} -> dynamoDBv2) (\s@AlarmAction' {} a -> s {dynamoDBv2 = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_firehose :: Lens.Lens' AlarmAction (Prelude.Maybe FirehoseAction)
alarmAction_firehose = Lens.lens (\AlarmAction' {firehose} -> firehose) (\s@AlarmAction' {} a -> s {firehose = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_iotEvents :: Lens.Lens' AlarmAction (Prelude.Maybe IotEventsAction)
alarmAction_iotEvents = Lens.lens (\AlarmAction' {iotEvents} -> iotEvents) (\s@AlarmAction' {} a -> s {iotEvents = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_iotSiteWise :: Lens.Lens' AlarmAction (Prelude.Maybe IotSiteWiseAction)
alarmAction_iotSiteWise = Lens.lens (\AlarmAction' {iotSiteWise} -> iotSiteWise) (\s@AlarmAction' {} a -> s {iotSiteWise = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_iotTopicPublish :: Lens.Lens' AlarmAction (Prelude.Maybe IotTopicPublishAction)
alarmAction_iotTopicPublish = Lens.lens (\AlarmAction' {iotTopicPublish} -> iotTopicPublish) (\s@AlarmAction' {} a -> s {iotTopicPublish = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_lambda :: Lens.Lens' AlarmAction (Prelude.Maybe LambdaAction)
alarmAction_lambda = Lens.lens (\AlarmAction' {lambda} -> lambda) (\s@AlarmAction' {} a -> s {lambda = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_sns :: Lens.Lens' AlarmAction (Prelude.Maybe SNSTopicPublishAction)
alarmAction_sns = Lens.lens (\AlarmAction' {sns} -> sns) (\s@AlarmAction' {} a -> s {sns = a} :: AlarmAction)

-- | Undocumented member.
alarmAction_sqs :: Lens.Lens' AlarmAction (Prelude.Maybe SqsAction)
alarmAction_sqs = Lens.lens (\AlarmAction' {sqs} -> sqs) (\s@AlarmAction' {} a -> s {sqs = a} :: AlarmAction)

instance Data.FromJSON AlarmAction where
  parseJSON =
    Data.withObject
      "AlarmAction"
      ( \x ->
          AlarmAction'
            Prelude.<$> (x Data..:? "dynamoDB")
            Prelude.<*> (x Data..:? "dynamoDBv2")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "iotEvents")
            Prelude.<*> (x Data..:? "iotSiteWise")
            Prelude.<*> (x Data..:? "iotTopicPublish")
            Prelude.<*> (x Data..:? "lambda")
            Prelude.<*> (x Data..:? "sns")
            Prelude.<*> (x Data..:? "sqs")
      )

instance Prelude.Hashable AlarmAction where
  hashWithSalt _salt AlarmAction' {..} =
    _salt
      `Prelude.hashWithSalt` dynamoDB
      `Prelude.hashWithSalt` dynamoDBv2
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` iotEvents
      `Prelude.hashWithSalt` iotSiteWise
      `Prelude.hashWithSalt` iotTopicPublish
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` sns
      `Prelude.hashWithSalt` sqs

instance Prelude.NFData AlarmAction where
  rnf AlarmAction' {..} =
    Prelude.rnf dynamoDB
      `Prelude.seq` Prelude.rnf dynamoDBv2
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf iotEvents
      `Prelude.seq` Prelude.rnf iotSiteWise
      `Prelude.seq` Prelude.rnf iotTopicPublish
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf sns
      `Prelude.seq` Prelude.rnf sqs

instance Data.ToJSON AlarmAction where
  toJSON AlarmAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dynamoDB" Data..=) Prelude.<$> dynamoDB,
            ("dynamoDBv2" Data..=) Prelude.<$> dynamoDBv2,
            ("firehose" Data..=) Prelude.<$> firehose,
            ("iotEvents" Data..=) Prelude.<$> iotEvents,
            ("iotSiteWise" Data..=) Prelude.<$> iotSiteWise,
            ("iotTopicPublish" Data..=)
              Prelude.<$> iotTopicPublish,
            ("lambda" Data..=) Prelude.<$> lambda,
            ("sns" Data..=) Prelude.<$> sns,
            ("sqs" Data..=) Prelude.<$> sqs
          ]
      )
