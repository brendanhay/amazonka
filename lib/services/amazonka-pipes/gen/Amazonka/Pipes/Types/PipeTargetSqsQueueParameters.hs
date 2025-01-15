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
-- Module      : Amazonka.Pipes.Types.PipeTargetSqsQueueParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetSqsQueueParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Amazon SQS stream as a source.
--
-- /See:/ 'newPipeTargetSqsQueueParameters' smart constructor.
data PipeTargetSqsQueueParameters = PipeTargetSqsQueueParameters'
  { -- | This parameter applies only to FIFO (first-in-first-out) queues.
    --
    -- The token used for deduplication of sent messages.
    messageDeduplicationId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The FIFO message group ID to use as the target.
    messageGroupId :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetSqsQueueParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageDeduplicationId', 'pipeTargetSqsQueueParameters_messageDeduplicationId' - This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages.
--
-- 'messageGroupId', 'pipeTargetSqsQueueParameters_messageGroupId' - The FIFO message group ID to use as the target.
newPipeTargetSqsQueueParameters ::
  PipeTargetSqsQueueParameters
newPipeTargetSqsQueueParameters =
  PipeTargetSqsQueueParameters'
    { messageDeduplicationId =
        Prelude.Nothing,
      messageGroupId = Prelude.Nothing
    }

-- | This parameter applies only to FIFO (first-in-first-out) queues.
--
-- The token used for deduplication of sent messages.
pipeTargetSqsQueueParameters_messageDeduplicationId :: Lens.Lens' PipeTargetSqsQueueParameters (Prelude.Maybe Prelude.Text)
pipeTargetSqsQueueParameters_messageDeduplicationId = Lens.lens (\PipeTargetSqsQueueParameters' {messageDeduplicationId} -> messageDeduplicationId) (\s@PipeTargetSqsQueueParameters' {} a -> s {messageDeduplicationId = a} :: PipeTargetSqsQueueParameters) Prelude.. Lens.mapping Data._Sensitive

-- | The FIFO message group ID to use as the target.
pipeTargetSqsQueueParameters_messageGroupId :: Lens.Lens' PipeTargetSqsQueueParameters (Prelude.Maybe Prelude.Text)
pipeTargetSqsQueueParameters_messageGroupId = Lens.lens (\PipeTargetSqsQueueParameters' {messageGroupId} -> messageGroupId) (\s@PipeTargetSqsQueueParameters' {} a -> s {messageGroupId = a} :: PipeTargetSqsQueueParameters) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON PipeTargetSqsQueueParameters where
  parseJSON =
    Data.withObject
      "PipeTargetSqsQueueParameters"
      ( \x ->
          PipeTargetSqsQueueParameters'
            Prelude.<$> (x Data..:? "MessageDeduplicationId")
            Prelude.<*> (x Data..:? "MessageGroupId")
      )

instance
  Prelude.Hashable
    PipeTargetSqsQueueParameters
  where
  hashWithSalt _salt PipeTargetSqsQueueParameters' {..} =
    _salt
      `Prelude.hashWithSalt` messageDeduplicationId
      `Prelude.hashWithSalt` messageGroupId

instance Prelude.NFData PipeTargetSqsQueueParameters where
  rnf PipeTargetSqsQueueParameters' {..} =
    Prelude.rnf messageDeduplicationId `Prelude.seq`
      Prelude.rnf messageGroupId

instance Data.ToJSON PipeTargetSqsQueueParameters where
  toJSON PipeTargetSqsQueueParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageDeduplicationId" Data..=)
              Prelude.<$> messageDeduplicationId,
            ("MessageGroupId" Data..=)
              Prelude.<$> messageGroupId
          ]
      )
