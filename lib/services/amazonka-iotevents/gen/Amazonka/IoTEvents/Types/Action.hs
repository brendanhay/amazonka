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
-- Module      : Amazonka.IoTEvents.Types.Action
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.ClearTimerAction
import Amazonka.IoTEvents.Types.DynamoDBAction
import Amazonka.IoTEvents.Types.DynamoDBv2Action
import Amazonka.IoTEvents.Types.FirehoseAction
import Amazonka.IoTEvents.Types.IotEventsAction
import Amazonka.IoTEvents.Types.IotSiteWiseAction
import Amazonka.IoTEvents.Types.IotTopicPublishAction
import Amazonka.IoTEvents.Types.LambdaAction
import Amazonka.IoTEvents.Types.ResetTimerAction
import Amazonka.IoTEvents.Types.SNSTopicPublishAction
import Amazonka.IoTEvents.Types.SetTimerAction
import Amazonka.IoTEvents.Types.SetVariableAction
import Amazonka.IoTEvents.Types.SqsAction
import qualified Amazonka.Prelude as Prelude

-- | An action to be performed when the @condition@ is TRUE.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Information needed to set the timer.
    setTimer :: Prelude.Maybe SetTimerAction,
    -- | Sends information about the detector model instance and the event that
    -- triggered the action to an Amazon Kinesis Data Firehose delivery stream.
    firehose :: Prelude.Maybe FirehoseAction,
    -- | Information needed to reset the timer.
    resetTimer :: Prelude.Maybe ResetTimerAction,
    -- | Writes to the DynamoDB table that you created. The default action
    -- payload contains all attribute-value pairs that have the information
    -- about the detector model instance and the event that triggered the
    -- action. You can customize the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
    -- A separate column of the DynamoDB table receives one attribute-value
    -- pair in the payload that you specify. For more information, see
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
    -- in /AWS IoT Events Developer Guide/.
    dynamoDBv2 :: Prelude.Maybe DynamoDBv2Action,
    -- | Writes to the DynamoDB table that you created. The default action
    -- payload contains all attribute-value pairs that have the information
    -- about the detector model instance and the event that triggered the
    -- action. You can customize the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
    -- One column of the DynamoDB table receives all attribute-value pairs in
    -- the payload that you specify. For more information, see
    -- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
    -- in /AWS IoT Events Developer Guide/.
    dynamoDB :: Prelude.Maybe DynamoDBAction,
    -- | Information needed to clear the timer.
    clearTimer :: Prelude.Maybe ClearTimerAction,
    -- | Calls a Lambda function, passing in information about the detector model
    -- instance and the event that triggered the action.
    lambda :: Prelude.Maybe LambdaAction,
    -- | Sends AWS IoT Events input, which passes information about the detector
    -- model instance and the event that triggered the action.
    iotEvents :: Prelude.Maybe IotEventsAction,
    -- | Sends information about the detector model instance and the event that
    -- triggered the action to an Amazon SQS queue.
    sqs :: Prelude.Maybe SqsAction,
    -- | Sends information about the detector model instance and the event that
    -- triggered the action to an asset property in AWS IoT SiteWise .
    iotSiteWise :: Prelude.Maybe IotSiteWiseAction,
    -- | Sets a variable to a specified value.
    setVariable :: Prelude.Maybe SetVariableAction,
    -- | Sends an Amazon SNS message.
    sns :: Prelude.Maybe SNSTopicPublishAction,
    -- | Publishes an MQTT message with the given topic to the AWS IoT message
    -- broker.
    iotTopicPublish :: Prelude.Maybe IotTopicPublishAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setTimer', 'action_setTimer' - Information needed to set the timer.
--
-- 'firehose', 'action_firehose' - Sends information about the detector model instance and the event that
-- triggered the action to an Amazon Kinesis Data Firehose delivery stream.
--
-- 'resetTimer', 'action_resetTimer' - Information needed to reset the timer.
--
-- 'dynamoDBv2', 'action_dynamoDBv2' - Writes to the DynamoDB table that you created. The default action
-- payload contains all attribute-value pairs that have the information
-- about the detector model instance and the event that triggered the
-- action. You can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- A separate column of the DynamoDB table receives one attribute-value
-- pair in the payload that you specify. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
-- in /AWS IoT Events Developer Guide/.
--
-- 'dynamoDB', 'action_dynamoDB' - Writes to the DynamoDB table that you created. The default action
-- payload contains all attribute-value pairs that have the information
-- about the detector model instance and the event that triggered the
-- action. You can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- One column of the DynamoDB table receives all attribute-value pairs in
-- the payload that you specify. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
-- in /AWS IoT Events Developer Guide/.
--
-- 'clearTimer', 'action_clearTimer' - Information needed to clear the timer.
--
-- 'lambda', 'action_lambda' - Calls a Lambda function, passing in information about the detector model
-- instance and the event that triggered the action.
--
-- 'iotEvents', 'action_iotEvents' - Sends AWS IoT Events input, which passes information about the detector
-- model instance and the event that triggered the action.
--
-- 'sqs', 'action_sqs' - Sends information about the detector model instance and the event that
-- triggered the action to an Amazon SQS queue.
--
-- 'iotSiteWise', 'action_iotSiteWise' - Sends information about the detector model instance and the event that
-- triggered the action to an asset property in AWS IoT SiteWise .
--
-- 'setVariable', 'action_setVariable' - Sets a variable to a specified value.
--
-- 'sns', 'action_sns' - Sends an Amazon SNS message.
--
-- 'iotTopicPublish', 'action_iotTopicPublish' - Publishes an MQTT message with the given topic to the AWS IoT message
-- broker.
newAction ::
  Action
newAction =
  Action'
    { setTimer = Prelude.Nothing,
      firehose = Prelude.Nothing,
      resetTimer = Prelude.Nothing,
      dynamoDBv2 = Prelude.Nothing,
      dynamoDB = Prelude.Nothing,
      clearTimer = Prelude.Nothing,
      lambda = Prelude.Nothing,
      iotEvents = Prelude.Nothing,
      sqs = Prelude.Nothing,
      iotSiteWise = Prelude.Nothing,
      setVariable = Prelude.Nothing,
      sns = Prelude.Nothing,
      iotTopicPublish = Prelude.Nothing
    }

-- | Information needed to set the timer.
action_setTimer :: Lens.Lens' Action (Prelude.Maybe SetTimerAction)
action_setTimer = Lens.lens (\Action' {setTimer} -> setTimer) (\s@Action' {} a -> s {setTimer = a} :: Action)

-- | Sends information about the detector model instance and the event that
-- triggered the action to an Amazon Kinesis Data Firehose delivery stream.
action_firehose :: Lens.Lens' Action (Prelude.Maybe FirehoseAction)
action_firehose = Lens.lens (\Action' {firehose} -> firehose) (\s@Action' {} a -> s {firehose = a} :: Action)

-- | Information needed to reset the timer.
action_resetTimer :: Lens.Lens' Action (Prelude.Maybe ResetTimerAction)
action_resetTimer = Lens.lens (\Action' {resetTimer} -> resetTimer) (\s@Action' {} a -> s {resetTimer = a} :: Action)

-- | Writes to the DynamoDB table that you created. The default action
-- payload contains all attribute-value pairs that have the information
-- about the detector model instance and the event that triggered the
-- action. You can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- A separate column of the DynamoDB table receives one attribute-value
-- pair in the payload that you specify. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
-- in /AWS IoT Events Developer Guide/.
action_dynamoDBv2 :: Lens.Lens' Action (Prelude.Maybe DynamoDBv2Action)
action_dynamoDBv2 = Lens.lens (\Action' {dynamoDBv2} -> dynamoDBv2) (\s@Action' {} a -> s {dynamoDBv2 = a} :: Action)

-- | Writes to the DynamoDB table that you created. The default action
-- payload contains all attribute-value pairs that have the information
-- about the detector model instance and the event that triggered the
-- action. You can customize the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_Payload.html payload>.
-- One column of the DynamoDB table receives all attribute-value pairs in
-- the payload that you specify. For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-event-actions.html Actions>
-- in /AWS IoT Events Developer Guide/.
action_dynamoDB :: Lens.Lens' Action (Prelude.Maybe DynamoDBAction)
action_dynamoDB = Lens.lens (\Action' {dynamoDB} -> dynamoDB) (\s@Action' {} a -> s {dynamoDB = a} :: Action)

-- | Information needed to clear the timer.
action_clearTimer :: Lens.Lens' Action (Prelude.Maybe ClearTimerAction)
action_clearTimer = Lens.lens (\Action' {clearTimer} -> clearTimer) (\s@Action' {} a -> s {clearTimer = a} :: Action)

-- | Calls a Lambda function, passing in information about the detector model
-- instance and the event that triggered the action.
action_lambda :: Lens.Lens' Action (Prelude.Maybe LambdaAction)
action_lambda = Lens.lens (\Action' {lambda} -> lambda) (\s@Action' {} a -> s {lambda = a} :: Action)

-- | Sends AWS IoT Events input, which passes information about the detector
-- model instance and the event that triggered the action.
action_iotEvents :: Lens.Lens' Action (Prelude.Maybe IotEventsAction)
action_iotEvents = Lens.lens (\Action' {iotEvents} -> iotEvents) (\s@Action' {} a -> s {iotEvents = a} :: Action)

-- | Sends information about the detector model instance and the event that
-- triggered the action to an Amazon SQS queue.
action_sqs :: Lens.Lens' Action (Prelude.Maybe SqsAction)
action_sqs = Lens.lens (\Action' {sqs} -> sqs) (\s@Action' {} a -> s {sqs = a} :: Action)

-- | Sends information about the detector model instance and the event that
-- triggered the action to an asset property in AWS IoT SiteWise .
action_iotSiteWise :: Lens.Lens' Action (Prelude.Maybe IotSiteWiseAction)
action_iotSiteWise = Lens.lens (\Action' {iotSiteWise} -> iotSiteWise) (\s@Action' {} a -> s {iotSiteWise = a} :: Action)

-- | Sets a variable to a specified value.
action_setVariable :: Lens.Lens' Action (Prelude.Maybe SetVariableAction)
action_setVariable = Lens.lens (\Action' {setVariable} -> setVariable) (\s@Action' {} a -> s {setVariable = a} :: Action)

-- | Sends an Amazon SNS message.
action_sns :: Lens.Lens' Action (Prelude.Maybe SNSTopicPublishAction)
action_sns = Lens.lens (\Action' {sns} -> sns) (\s@Action' {} a -> s {sns = a} :: Action)

-- | Publishes an MQTT message with the given topic to the AWS IoT message
-- broker.
action_iotTopicPublish :: Lens.Lens' Action (Prelude.Maybe IotTopicPublishAction)
action_iotTopicPublish = Lens.lens (\Action' {iotTopicPublish} -> iotTopicPublish) (\s@Action' {} a -> s {iotTopicPublish = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Data..:? "setTimer")
            Prelude.<*> (x Data..:? "firehose")
            Prelude.<*> (x Data..:? "resetTimer")
            Prelude.<*> (x Data..:? "dynamoDBv2")
            Prelude.<*> (x Data..:? "dynamoDB")
            Prelude.<*> (x Data..:? "clearTimer")
            Prelude.<*> (x Data..:? "lambda")
            Prelude.<*> (x Data..:? "iotEvents")
            Prelude.<*> (x Data..:? "sqs")
            Prelude.<*> (x Data..:? "iotSiteWise")
            Prelude.<*> (x Data..:? "setVariable")
            Prelude.<*> (x Data..:? "sns")
            Prelude.<*> (x Data..:? "iotTopicPublish")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` setTimer
      `Prelude.hashWithSalt` firehose
      `Prelude.hashWithSalt` resetTimer
      `Prelude.hashWithSalt` dynamoDBv2
      `Prelude.hashWithSalt` dynamoDB
      `Prelude.hashWithSalt` clearTimer
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` iotEvents
      `Prelude.hashWithSalt` sqs
      `Prelude.hashWithSalt` iotSiteWise
      `Prelude.hashWithSalt` setVariable
      `Prelude.hashWithSalt` sns
      `Prelude.hashWithSalt` iotTopicPublish

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf setTimer
      `Prelude.seq` Prelude.rnf firehose
      `Prelude.seq` Prelude.rnf resetTimer
      `Prelude.seq` Prelude.rnf dynamoDBv2
      `Prelude.seq` Prelude.rnf dynamoDB
      `Prelude.seq` Prelude.rnf clearTimer
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf iotEvents
      `Prelude.seq` Prelude.rnf sqs
      `Prelude.seq` Prelude.rnf iotSiteWise
      `Prelude.seq` Prelude.rnf setVariable
      `Prelude.seq` Prelude.rnf sns
      `Prelude.seq` Prelude.rnf iotTopicPublish

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("setTimer" Data..=) Prelude.<$> setTimer,
            ("firehose" Data..=) Prelude.<$> firehose,
            ("resetTimer" Data..=) Prelude.<$> resetTimer,
            ("dynamoDBv2" Data..=) Prelude.<$> dynamoDBv2,
            ("dynamoDB" Data..=) Prelude.<$> dynamoDB,
            ("clearTimer" Data..=) Prelude.<$> clearTimer,
            ("lambda" Data..=) Prelude.<$> lambda,
            ("iotEvents" Data..=) Prelude.<$> iotEvents,
            ("sqs" Data..=) Prelude.<$> sqs,
            ("iotSiteWise" Data..=) Prelude.<$> iotSiteWise,
            ("setVariable" Data..=) Prelude.<$> setVariable,
            ("sns" Data..=) Prelude.<$> sns,
            ("iotTopicPublish" Data..=)
              Prelude.<$> iotTopicPublish
          ]
      )
