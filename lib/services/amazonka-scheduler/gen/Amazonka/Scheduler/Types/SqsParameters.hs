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
-- Module      : Amazonka.Scheduler.Types.SqsParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.SqsParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The templated target type for the Amazon SQS
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html SendMessage>
-- API operation. Contains the message group ID to use when the target is a
-- FIFO queue. If you specify an Amazon SQS FIFO queue as a target, the
-- queue must have content-based deduplication enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/using-messagededuplicationid-property.html Using the Amazon SQS message deduplication ID>
-- in the /Amazon SQS Developer Guide/.
--
-- /See:/ 'newSqsParameters' smart constructor.
data SqsParameters = SqsParameters'
  { -- | The FIFO message group ID to use as the target.
    messageGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageGroupId', 'sqsParameters_messageGroupId' - The FIFO message group ID to use as the target.
newSqsParameters ::
  SqsParameters
newSqsParameters =
  SqsParameters' {messageGroupId = Prelude.Nothing}

-- | The FIFO message group ID to use as the target.
sqsParameters_messageGroupId :: Lens.Lens' SqsParameters (Prelude.Maybe Prelude.Text)
sqsParameters_messageGroupId = Lens.lens (\SqsParameters' {messageGroupId} -> messageGroupId) (\s@SqsParameters' {} a -> s {messageGroupId = a} :: SqsParameters)

instance Data.FromJSON SqsParameters where
  parseJSON =
    Data.withObject
      "SqsParameters"
      ( \x ->
          SqsParameters'
            Prelude.<$> (x Data..:? "MessageGroupId")
      )

instance Prelude.Hashable SqsParameters where
  hashWithSalt _salt SqsParameters' {..} =
    _salt `Prelude.hashWithSalt` messageGroupId

instance Prelude.NFData SqsParameters where
  rnf SqsParameters' {..} = Prelude.rnf messageGroupId

instance Data.ToJSON SqsParameters where
  toJSON SqsParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageGroupId" Data..=)
              Prelude.<$> messageGroupId
          ]
      )
