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
-- Module      : Amazonka.LookoutEquipment.Types.IngestedFilesSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.IngestedFilesSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Gives statistics about how many files have been ingested, and which
-- files have not been ingested, for a particular ingestion job.
--
-- /See:/ 'newIngestedFilesSummary' smart constructor.
data IngestedFilesSummary = IngestedFilesSummary'
  { -- | Indicates the number of files that were discarded. A file could be
    -- discarded because its format is invalid (for example, a jpg or pdf) or
    -- not readable.
    discardedFiles :: Prelude.Maybe [S3Object],
    -- | Indicates the total number of files that were submitted for ingestion.
    totalNumberOfFiles :: Prelude.Int,
    -- | Indicates the number of files that were successfully ingested.
    ingestedNumberOfFiles :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestedFilesSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discardedFiles', 'ingestedFilesSummary_discardedFiles' - Indicates the number of files that were discarded. A file could be
-- discarded because its format is invalid (for example, a jpg or pdf) or
-- not readable.
--
-- 'totalNumberOfFiles', 'ingestedFilesSummary_totalNumberOfFiles' - Indicates the total number of files that were submitted for ingestion.
--
-- 'ingestedNumberOfFiles', 'ingestedFilesSummary_ingestedNumberOfFiles' - Indicates the number of files that were successfully ingested.
newIngestedFilesSummary ::
  -- | 'totalNumberOfFiles'
  Prelude.Int ->
  -- | 'ingestedNumberOfFiles'
  Prelude.Int ->
  IngestedFilesSummary
newIngestedFilesSummary
  pTotalNumberOfFiles_
  pIngestedNumberOfFiles_ =
    IngestedFilesSummary'
      { discardedFiles =
          Prelude.Nothing,
        totalNumberOfFiles = pTotalNumberOfFiles_,
        ingestedNumberOfFiles = pIngestedNumberOfFiles_
      }

-- | Indicates the number of files that were discarded. A file could be
-- discarded because its format is invalid (for example, a jpg or pdf) or
-- not readable.
ingestedFilesSummary_discardedFiles :: Lens.Lens' IngestedFilesSummary (Prelude.Maybe [S3Object])
ingestedFilesSummary_discardedFiles = Lens.lens (\IngestedFilesSummary' {discardedFiles} -> discardedFiles) (\s@IngestedFilesSummary' {} a -> s {discardedFiles = a} :: IngestedFilesSummary) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the total number of files that were submitted for ingestion.
ingestedFilesSummary_totalNumberOfFiles :: Lens.Lens' IngestedFilesSummary Prelude.Int
ingestedFilesSummary_totalNumberOfFiles = Lens.lens (\IngestedFilesSummary' {totalNumberOfFiles} -> totalNumberOfFiles) (\s@IngestedFilesSummary' {} a -> s {totalNumberOfFiles = a} :: IngestedFilesSummary)

-- | Indicates the number of files that were successfully ingested.
ingestedFilesSummary_ingestedNumberOfFiles :: Lens.Lens' IngestedFilesSummary Prelude.Int
ingestedFilesSummary_ingestedNumberOfFiles = Lens.lens (\IngestedFilesSummary' {ingestedNumberOfFiles} -> ingestedNumberOfFiles) (\s@IngestedFilesSummary' {} a -> s {ingestedNumberOfFiles = a} :: IngestedFilesSummary)

instance Data.FromJSON IngestedFilesSummary where
  parseJSON =
    Data.withObject
      "IngestedFilesSummary"
      ( \x ->
          IngestedFilesSummary'
            Prelude.<$> (x Data..:? "DiscardedFiles" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "TotalNumberOfFiles")
            Prelude.<*> (x Data..: "IngestedNumberOfFiles")
      )

instance Prelude.Hashable IngestedFilesSummary where
  hashWithSalt _salt IngestedFilesSummary' {..} =
    _salt `Prelude.hashWithSalt` discardedFiles
      `Prelude.hashWithSalt` totalNumberOfFiles
      `Prelude.hashWithSalt` ingestedNumberOfFiles

instance Prelude.NFData IngestedFilesSummary where
  rnf IngestedFilesSummary' {..} =
    Prelude.rnf discardedFiles
      `Prelude.seq` Prelude.rnf totalNumberOfFiles
      `Prelude.seq` Prelude.rnf ingestedNumberOfFiles
