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
-- Module      : Amazonka.S3.Types.SelectObjectContentEventStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.SelectObjectContentEventStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ContinuationEvent
import Amazonka.S3.Types.EndEvent
import Amazonka.S3.Types.ProgressEvent
import Amazonka.S3.Types.RecordsEvent
import Amazonka.S3.Types.StatsEvent

-- | The container for selecting objects from a content event stream.
--
-- /See:/ 'newSelectObjectContentEventStream' smart constructor.
data SelectObjectContentEventStream = SelectObjectContentEventStream'
  { -- | The Continuation Event.
    cont :: Prelude.Maybe ContinuationEvent,
    -- | The End Event.
    end :: Prelude.Maybe EndEvent,
    -- | The Progress Event.
    progress :: Prelude.Maybe ProgressEvent,
    -- | The Records Event.
    records :: Prelude.Maybe RecordsEvent,
    -- | The Stats Event.
    stats :: Prelude.Maybe StatsEvent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelectObjectContentEventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cont', 'selectObjectContentEventStream_cont' - The Continuation Event.
--
-- 'end', 'selectObjectContentEventStream_end' - The End Event.
--
-- 'progress', 'selectObjectContentEventStream_progress' - The Progress Event.
--
-- 'records', 'selectObjectContentEventStream_records' - The Records Event.
--
-- 'stats', 'selectObjectContentEventStream_stats' - The Stats Event.
newSelectObjectContentEventStream ::
  SelectObjectContentEventStream
newSelectObjectContentEventStream =
  SelectObjectContentEventStream'
    { cont =
        Prelude.Nothing,
      end = Prelude.Nothing,
      progress = Prelude.Nothing,
      records = Prelude.Nothing,
      stats = Prelude.Nothing
    }

-- | The Continuation Event.
selectObjectContentEventStream_cont :: Lens.Lens' SelectObjectContentEventStream (Prelude.Maybe ContinuationEvent)
selectObjectContentEventStream_cont = Lens.lens (\SelectObjectContentEventStream' {cont} -> cont) (\s@SelectObjectContentEventStream' {} a -> s {cont = a} :: SelectObjectContentEventStream)

-- | The End Event.
selectObjectContentEventStream_end :: Lens.Lens' SelectObjectContentEventStream (Prelude.Maybe EndEvent)
selectObjectContentEventStream_end = Lens.lens (\SelectObjectContentEventStream' {end} -> end) (\s@SelectObjectContentEventStream' {} a -> s {end = a} :: SelectObjectContentEventStream)

-- | The Progress Event.
selectObjectContentEventStream_progress :: Lens.Lens' SelectObjectContentEventStream (Prelude.Maybe ProgressEvent)
selectObjectContentEventStream_progress = Lens.lens (\SelectObjectContentEventStream' {progress} -> progress) (\s@SelectObjectContentEventStream' {} a -> s {progress = a} :: SelectObjectContentEventStream)

-- | The Records Event.
selectObjectContentEventStream_records :: Lens.Lens' SelectObjectContentEventStream (Prelude.Maybe RecordsEvent)
selectObjectContentEventStream_records = Lens.lens (\SelectObjectContentEventStream' {records} -> records) (\s@SelectObjectContentEventStream' {} a -> s {records = a} :: SelectObjectContentEventStream)

-- | The Stats Event.
selectObjectContentEventStream_stats :: Lens.Lens' SelectObjectContentEventStream (Prelude.Maybe StatsEvent)
selectObjectContentEventStream_stats = Lens.lens (\SelectObjectContentEventStream' {stats} -> stats) (\s@SelectObjectContentEventStream' {} a -> s {stats = a} :: SelectObjectContentEventStream)

instance Data.FromXML SelectObjectContentEventStream where
  parseXML x =
    SelectObjectContentEventStream'
      Prelude.<$> (x Data..@? "Cont")
      Prelude.<*> (x Data..@? "End")
      Prelude.<*> (x Data..@? "Progress")
      Prelude.<*> (x Data..@? "Records")
      Prelude.<*> (x Data..@? "Stats")

instance
  Prelude.Hashable
    SelectObjectContentEventStream
  where
  hashWithSalt
    _salt
    SelectObjectContentEventStream' {..} =
      _salt `Prelude.hashWithSalt` cont
        `Prelude.hashWithSalt` end
        `Prelude.hashWithSalt` progress
        `Prelude.hashWithSalt` records
        `Prelude.hashWithSalt` stats

instance
  Prelude.NFData
    SelectObjectContentEventStream
  where
  rnf SelectObjectContentEventStream' {..} =
    Prelude.rnf cont
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf stats
