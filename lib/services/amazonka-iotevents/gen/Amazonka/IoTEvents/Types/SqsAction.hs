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
-- Module      : Amazonka.IoTEvents.Types.SqsAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SqsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Prelude as Prelude

-- | Sends information about the detector model instance and the event that
-- triggered the action to an Amazon SQS queue.
--
-- /See:/ 'newSqsAction' smart constructor.
data SqsAction = SqsAction'
  { -- | Set this to TRUE if you want the data to be base-64 encoded before it is
    -- written to the queue. Otherwise, set this to FALSE.
    useBase64 :: Prelude.Maybe Prelude.Bool,
    -- | You can configure the action payload when you send a message to an
    -- Amazon SQS queue.
    payload :: Prelude.Maybe Payload,
    -- | The URL of the SQS queue where the data is written.
    queueUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'useBase64', 'sqsAction_useBase64' - Set this to TRUE if you want the data to be base-64 encoded before it is
-- written to the queue. Otherwise, set this to FALSE.
--
-- 'payload', 'sqsAction_payload' - You can configure the action payload when you send a message to an
-- Amazon SQS queue.
--
-- 'queueUrl', 'sqsAction_queueUrl' - The URL of the SQS queue where the data is written.
newSqsAction ::
  -- | 'queueUrl'
  Prelude.Text ->
  SqsAction
newSqsAction pQueueUrl_ =
  SqsAction'
    { useBase64 = Prelude.Nothing,
      payload = Prelude.Nothing,
      queueUrl = pQueueUrl_
    }

-- | Set this to TRUE if you want the data to be base-64 encoded before it is
-- written to the queue. Otherwise, set this to FALSE.
sqsAction_useBase64 :: Lens.Lens' SqsAction (Prelude.Maybe Prelude.Bool)
sqsAction_useBase64 = Lens.lens (\SqsAction' {useBase64} -> useBase64) (\s@SqsAction' {} a -> s {useBase64 = a} :: SqsAction)

-- | You can configure the action payload when you send a message to an
-- Amazon SQS queue.
sqsAction_payload :: Lens.Lens' SqsAction (Prelude.Maybe Payload)
sqsAction_payload = Lens.lens (\SqsAction' {payload} -> payload) (\s@SqsAction' {} a -> s {payload = a} :: SqsAction)

-- | The URL of the SQS queue where the data is written.
sqsAction_queueUrl :: Lens.Lens' SqsAction Prelude.Text
sqsAction_queueUrl = Lens.lens (\SqsAction' {queueUrl} -> queueUrl) (\s@SqsAction' {} a -> s {queueUrl = a} :: SqsAction)

instance Data.FromJSON SqsAction where
  parseJSON =
    Data.withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            Prelude.<$> (x Data..:? "useBase64")
            Prelude.<*> (x Data..:? "payload")
            Prelude.<*> (x Data..: "queueUrl")
      )

instance Prelude.Hashable SqsAction where
  hashWithSalt _salt SqsAction' {..} =
    _salt `Prelude.hashWithSalt` useBase64
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` queueUrl

instance Prelude.NFData SqsAction where
  rnf SqsAction' {..} =
    Prelude.rnf useBase64
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf queueUrl

instance Data.ToJSON SqsAction where
  toJSON SqsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("useBase64" Data..=) Prelude.<$> useBase64,
            ("payload" Data..=) Prelude.<$> payload,
            Prelude.Just ("queueUrl" Data..= queueUrl)
          ]
      )
