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
-- Module      : Amazonka.MediaConvert.Types.QueueTransition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.QueueTransition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Description of the source and destination queues between which the job
-- has moved, along with the timestamp of the move
--
-- /See:/ 'newQueueTransition' smart constructor.
data QueueTransition = QueueTransition'
  { -- | The queue that the job was on after the transition.
    destinationQueue :: Prelude.Maybe Prelude.Text,
    -- | The queue that the job was on before the transition.
    sourceQueue :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix epoch format, that the job moved from the source queue
    -- to the destination queue.
    timestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueueTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationQueue', 'queueTransition_destinationQueue' - The queue that the job was on after the transition.
--
-- 'sourceQueue', 'queueTransition_sourceQueue' - The queue that the job was on before the transition.
--
-- 'timestamp', 'queueTransition_timestamp' - The time, in Unix epoch format, that the job moved from the source queue
-- to the destination queue.
newQueueTransition ::
  QueueTransition
newQueueTransition =
  QueueTransition'
    { destinationQueue =
        Prelude.Nothing,
      sourceQueue = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The queue that the job was on after the transition.
queueTransition_destinationQueue :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.Text)
queueTransition_destinationQueue = Lens.lens (\QueueTransition' {destinationQueue} -> destinationQueue) (\s@QueueTransition' {} a -> s {destinationQueue = a} :: QueueTransition)

-- | The queue that the job was on before the transition.
queueTransition_sourceQueue :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.Text)
queueTransition_sourceQueue = Lens.lens (\QueueTransition' {sourceQueue} -> sourceQueue) (\s@QueueTransition' {} a -> s {sourceQueue = a} :: QueueTransition)

-- | The time, in Unix epoch format, that the job moved from the source queue
-- to the destination queue.
queueTransition_timestamp :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.UTCTime)
queueTransition_timestamp = Lens.lens (\QueueTransition' {timestamp} -> timestamp) (\s@QueueTransition' {} a -> s {timestamp = a} :: QueueTransition) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON QueueTransition where
  parseJSON =
    Data.withObject
      "QueueTransition"
      ( \x ->
          QueueTransition'
            Prelude.<$> (x Data..:? "destinationQueue")
            Prelude.<*> (x Data..:? "sourceQueue")
            Prelude.<*> (x Data..:? "timestamp")
      )

instance Prelude.Hashable QueueTransition where
  hashWithSalt _salt QueueTransition' {..} =
    _salt
      `Prelude.hashWithSalt` destinationQueue
      `Prelude.hashWithSalt` sourceQueue
      `Prelude.hashWithSalt` timestamp

instance Prelude.NFData QueueTransition where
  rnf QueueTransition' {..} =
    Prelude.rnf destinationQueue
      `Prelude.seq` Prelude.rnf sourceQueue
      `Prelude.seq` Prelude.rnf timestamp
