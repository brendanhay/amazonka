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
-- Module      : Network.AWS.S3.Types.SelectObjectContentEventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SelectObjectContentEventStream where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ContinuationEvent
import Network.AWS.S3.Types.EndEvent
import Network.AWS.S3.Types.ProgressEvent
import Network.AWS.S3.Types.RecordsEvent
import Network.AWS.S3.Types.StatsEvent

-- | The container for selecting objects from a content event stream.
--
-- /See:/ 'newSelectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { -- | The End Event.
    end :: Core.Maybe EndEvent,
    -- | The Records Event.
    records :: Core.Maybe RecordsEvent,
    -- | The Stats Event.
    stats :: Core.Maybe StatsEvent,
    -- | The Continuation Event.
    cont :: Core.Maybe ContinuationEvent,
    -- | The Progress Event.
    progress :: Core.Maybe ProgressEvent
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SelectObjectContentEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'selectObjectContentEventStream_end' - The End Event.
--
-- 'records', 'selectObjectContentEventStream_records' - The Records Event.
--
-- 'stats', 'selectObjectContentEventStream_stats' - The Stats Event.
--
-- 'cont', 'selectObjectContentEventStream_cont' - The Continuation Event.
--
-- 'progress', 'selectObjectContentEventStream_progress' - The Progress Event.
newSelectObjectContentEventStream ::
  SelectObjectContentEventStream
newSelectObjectContentEventStream =
  SelectObjectContentEventStream'
    { end = Core.Nothing,
      records = Core.Nothing,
      stats = Core.Nothing,
      cont = Core.Nothing,
      progress = Core.Nothing
    }

-- | The End Event.
selectObjectContentEventStream_end :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe EndEvent)
selectObjectContentEventStream_end = Lens.lens (\SelectObjectContentEventStream' {end} -> end) (\s@SelectObjectContentEventStream' {} a -> s {end = a} :: SelectObjectContentEventStream)

-- | The Records Event.
selectObjectContentEventStream_records :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe RecordsEvent)
selectObjectContentEventStream_records = Lens.lens (\SelectObjectContentEventStream' {records} -> records) (\s@SelectObjectContentEventStream' {} a -> s {records = a} :: SelectObjectContentEventStream)

-- | The Stats Event.
selectObjectContentEventStream_stats :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe StatsEvent)
selectObjectContentEventStream_stats = Lens.lens (\SelectObjectContentEventStream' {stats} -> stats) (\s@SelectObjectContentEventStream' {} a -> s {stats = a} :: SelectObjectContentEventStream)

-- | The Continuation Event.
selectObjectContentEventStream_cont :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe ContinuationEvent)
selectObjectContentEventStream_cont = Lens.lens (\SelectObjectContentEventStream' {cont} -> cont) (\s@SelectObjectContentEventStream' {} a -> s {cont = a} :: SelectObjectContentEventStream)

-- | The Progress Event.
selectObjectContentEventStream_progress :: Lens.Lens' SelectObjectContentEventStream (Core.Maybe ProgressEvent)
selectObjectContentEventStream_progress = Lens.lens (\SelectObjectContentEventStream' {progress} -> progress) (\s@SelectObjectContentEventStream' {} a -> s {progress = a} :: SelectObjectContentEventStream)

instance Core.FromXML SelectObjectContentEventStream where
  parseXML x =
    SelectObjectContentEventStream'
      Core.<$> (x Core..@? "End")
      Core.<*> (x Core..@? "Records")
      Core.<*> (x Core..@? "Stats")
      Core.<*> (x Core..@? "Cont")
      Core.<*> (x Core..@? "Progress")

instance Core.Hashable SelectObjectContentEventStream

instance Core.NFData SelectObjectContentEventStream
