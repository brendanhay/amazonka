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
-- Module      : Amazonka.TimeStreamWrite.Types.BatchLoadProgressReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.BatchLoadProgressReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the progress of a batch load task.
--
-- /See:/ 'newBatchLoadProgressReport' smart constructor.
data BatchLoadProgressReport = BatchLoadProgressReport'
  { bytesMetered :: Prelude.Maybe Prelude.Integer,
    fileFailures :: Prelude.Maybe Prelude.Integer,
    parseFailures :: Prelude.Maybe Prelude.Integer,
    recordIngestionFailures :: Prelude.Maybe Prelude.Integer,
    recordsIngested :: Prelude.Maybe Prelude.Integer,
    recordsProcessed :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchLoadProgressReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesMetered', 'batchLoadProgressReport_bytesMetered' -
--
-- 'fileFailures', 'batchLoadProgressReport_fileFailures' -
--
-- 'parseFailures', 'batchLoadProgressReport_parseFailures' -
--
-- 'recordIngestionFailures', 'batchLoadProgressReport_recordIngestionFailures' -
--
-- 'recordsIngested', 'batchLoadProgressReport_recordsIngested' -
--
-- 'recordsProcessed', 'batchLoadProgressReport_recordsProcessed' -
newBatchLoadProgressReport ::
  BatchLoadProgressReport
newBatchLoadProgressReport =
  BatchLoadProgressReport'
    { bytesMetered =
        Prelude.Nothing,
      fileFailures = Prelude.Nothing,
      parseFailures = Prelude.Nothing,
      recordIngestionFailures = Prelude.Nothing,
      recordsIngested = Prelude.Nothing,
      recordsProcessed = Prelude.Nothing
    }

batchLoadProgressReport_bytesMetered :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_bytesMetered = Lens.lens (\BatchLoadProgressReport' {bytesMetered} -> bytesMetered) (\s@BatchLoadProgressReport' {} a -> s {bytesMetered = a} :: BatchLoadProgressReport)

batchLoadProgressReport_fileFailures :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_fileFailures = Lens.lens (\BatchLoadProgressReport' {fileFailures} -> fileFailures) (\s@BatchLoadProgressReport' {} a -> s {fileFailures = a} :: BatchLoadProgressReport)

batchLoadProgressReport_parseFailures :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_parseFailures = Lens.lens (\BatchLoadProgressReport' {parseFailures} -> parseFailures) (\s@BatchLoadProgressReport' {} a -> s {parseFailures = a} :: BatchLoadProgressReport)

batchLoadProgressReport_recordIngestionFailures :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_recordIngestionFailures = Lens.lens (\BatchLoadProgressReport' {recordIngestionFailures} -> recordIngestionFailures) (\s@BatchLoadProgressReport' {} a -> s {recordIngestionFailures = a} :: BatchLoadProgressReport)

batchLoadProgressReport_recordsIngested :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_recordsIngested = Lens.lens (\BatchLoadProgressReport' {recordsIngested} -> recordsIngested) (\s@BatchLoadProgressReport' {} a -> s {recordsIngested = a} :: BatchLoadProgressReport)

batchLoadProgressReport_recordsProcessed :: Lens.Lens' BatchLoadProgressReport (Prelude.Maybe Prelude.Integer)
batchLoadProgressReport_recordsProcessed = Lens.lens (\BatchLoadProgressReport' {recordsProcessed} -> recordsProcessed) (\s@BatchLoadProgressReport' {} a -> s {recordsProcessed = a} :: BatchLoadProgressReport)

instance Data.FromJSON BatchLoadProgressReport where
  parseJSON =
    Data.withObject
      "BatchLoadProgressReport"
      ( \x ->
          BatchLoadProgressReport'
            Prelude.<$> (x Data..:? "BytesMetered")
            Prelude.<*> (x Data..:? "FileFailures")
            Prelude.<*> (x Data..:? "ParseFailures")
            Prelude.<*> (x Data..:? "RecordIngestionFailures")
            Prelude.<*> (x Data..:? "RecordsIngested")
            Prelude.<*> (x Data..:? "RecordsProcessed")
      )

instance Prelude.Hashable BatchLoadProgressReport where
  hashWithSalt _salt BatchLoadProgressReport' {..} =
    _salt
      `Prelude.hashWithSalt` bytesMetered
      `Prelude.hashWithSalt` fileFailures
      `Prelude.hashWithSalt` parseFailures
      `Prelude.hashWithSalt` recordIngestionFailures
      `Prelude.hashWithSalt` recordsIngested
      `Prelude.hashWithSalt` recordsProcessed

instance Prelude.NFData BatchLoadProgressReport where
  rnf BatchLoadProgressReport' {..} =
    Prelude.rnf bytesMetered
      `Prelude.seq` Prelude.rnf fileFailures
      `Prelude.seq` Prelude.rnf parseFailures
      `Prelude.seq` Prelude.rnf recordIngestionFailures
      `Prelude.seq` Prelude.rnf recordsIngested
      `Prelude.seq` Prelude.rnf recordsProcessed
