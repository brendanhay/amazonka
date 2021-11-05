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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SqsAction where

import qualified Amazonka.Core as Core
import Amazonka.IoTEvents.Types.Payload
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Sends information about the detector model instance and the event that
-- triggered the action to an Amazon SQS queue.
--
-- /See:/ 'newSqsAction' smart constructor.
data SqsAction = SqsAction'
  { -- | You can configure the action payload when you send a message to an
    -- Amazon SQS queue.
    payload :: Prelude.Maybe Payload,
    -- | Set this to TRUE if you want the data to be base-64 encoded before it is
    -- written to the queue. Otherwise, set this to FALSE.
    useBase64 :: Prelude.Maybe Prelude.Bool,
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
-- 'payload', 'sqsAction_payload' - You can configure the action payload when you send a message to an
-- Amazon SQS queue.
--
-- 'useBase64', 'sqsAction_useBase64' - Set this to TRUE if you want the data to be base-64 encoded before it is
-- written to the queue. Otherwise, set this to FALSE.
--
-- 'queueUrl', 'sqsAction_queueUrl' - The URL of the SQS queue where the data is written.
newSqsAction ::
  -- | 'queueUrl'
  Prelude.Text ->
  SqsAction
newSqsAction pQueueUrl_ =
  SqsAction'
    { payload = Prelude.Nothing,
      useBase64 = Prelude.Nothing,
      queueUrl = pQueueUrl_
    }

-- | You can configure the action payload when you send a message to an
-- Amazon SQS queue.
sqsAction_payload :: Lens.Lens' SqsAction (Prelude.Maybe Payload)
sqsAction_payload = Lens.lens (\SqsAction' {payload} -> payload) (\s@SqsAction' {} a -> s {payload = a} :: SqsAction)

-- | Set this to TRUE if you want the data to be base-64 encoded before it is
-- written to the queue. Otherwise, set this to FALSE.
sqsAction_useBase64 :: Lens.Lens' SqsAction (Prelude.Maybe Prelude.Bool)
sqsAction_useBase64 = Lens.lens (\SqsAction' {useBase64} -> useBase64) (\s@SqsAction' {} a -> s {useBase64 = a} :: SqsAction)

-- | The URL of the SQS queue where the data is written.
sqsAction_queueUrl :: Lens.Lens' SqsAction Prelude.Text
sqsAction_queueUrl = Lens.lens (\SqsAction' {queueUrl} -> queueUrl) (\s@SqsAction' {} a -> s {queueUrl = a} :: SqsAction)

instance Core.FromJSON SqsAction where
  parseJSON =
    Core.withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            Prelude.<$> (x Core..:? "payload")
            Prelude.<*> (x Core..:? "useBase64")
            Prelude.<*> (x Core..: "queueUrl")
      )

instance Prelude.Hashable SqsAction

instance Prelude.NFData SqsAction

instance Core.ToJSON SqsAction where
  toJSON SqsAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("payload" Core..=) Prelude.<$> payload,
            ("useBase64" Core..=) Prelude.<$> useBase64,
            Prelude.Just ("queueUrl" Core..= queueUrl)
          ]
      )
