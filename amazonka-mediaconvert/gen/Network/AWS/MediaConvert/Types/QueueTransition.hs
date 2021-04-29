{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.QueueTransition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.QueueTransition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Description of the source and destination queues between which the job
-- has moved, along with the timestamp of the move
--
-- /See:/ 'newQueueTransition' smart constructor.
data QueueTransition = QueueTransition'
  { -- | The queue that the job was on before the transition.
    sourceQueue :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix epoch format, that the job moved from the source queue
    -- to the destination queue.
    timestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The queue that the job was on after the transition.
    destinationQueue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueueTransition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceQueue', 'queueTransition_sourceQueue' - The queue that the job was on before the transition.
--
-- 'timestamp', 'queueTransition_timestamp' - The time, in Unix epoch format, that the job moved from the source queue
-- to the destination queue.
--
-- 'destinationQueue', 'queueTransition_destinationQueue' - The queue that the job was on after the transition.
newQueueTransition ::
  QueueTransition
newQueueTransition =
  QueueTransition'
    { sourceQueue = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      destinationQueue = Prelude.Nothing
    }

-- | The queue that the job was on before the transition.
queueTransition_sourceQueue :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.Text)
queueTransition_sourceQueue = Lens.lens (\QueueTransition' {sourceQueue} -> sourceQueue) (\s@QueueTransition' {} a -> s {sourceQueue = a} :: QueueTransition)

-- | The time, in Unix epoch format, that the job moved from the source queue
-- to the destination queue.
queueTransition_timestamp :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.UTCTime)
queueTransition_timestamp = Lens.lens (\QueueTransition' {timestamp} -> timestamp) (\s@QueueTransition' {} a -> s {timestamp = a} :: QueueTransition) Prelude.. Lens.mapping Prelude._Time

-- | The queue that the job was on after the transition.
queueTransition_destinationQueue :: Lens.Lens' QueueTransition (Prelude.Maybe Prelude.Text)
queueTransition_destinationQueue = Lens.lens (\QueueTransition' {destinationQueue} -> destinationQueue) (\s@QueueTransition' {} a -> s {destinationQueue = a} :: QueueTransition)

instance Prelude.FromJSON QueueTransition where
  parseJSON =
    Prelude.withObject
      "QueueTransition"
      ( \x ->
          QueueTransition'
            Prelude.<$> (x Prelude..:? "sourceQueue")
            Prelude.<*> (x Prelude..:? "timestamp")
            Prelude.<*> (x Prelude..:? "destinationQueue")
      )

instance Prelude.Hashable QueueTransition

instance Prelude.NFData QueueTransition
