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
-- Module      : Amazonka.DMS.Types.BatchStartRecommendationsErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.BatchStartRecommendationsErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the errors that occurred during the analysis
-- of the source database.
--
-- /See:/ 'newBatchStartRecommendationsErrorEntry' smart constructor.
data BatchStartRecommendationsErrorEntry = BatchStartRecommendationsErrorEntry'
  { -- | The code of an error that occurred during the analysis of the source
    -- database.
    code :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the source database.
    databaseId :: Prelude.Maybe Prelude.Text,
    -- | The information about the error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStartRecommendationsErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'batchStartRecommendationsErrorEntry_code' - The code of an error that occurred during the analysis of the source
-- database.
--
-- 'databaseId', 'batchStartRecommendationsErrorEntry_databaseId' - The identifier of the source database.
--
-- 'message', 'batchStartRecommendationsErrorEntry_message' - The information about the error.
newBatchStartRecommendationsErrorEntry ::
  BatchStartRecommendationsErrorEntry
newBatchStartRecommendationsErrorEntry =
  BatchStartRecommendationsErrorEntry'
    { code =
        Prelude.Nothing,
      databaseId = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The code of an error that occurred during the analysis of the source
-- database.
batchStartRecommendationsErrorEntry_code :: Lens.Lens' BatchStartRecommendationsErrorEntry (Prelude.Maybe Prelude.Text)
batchStartRecommendationsErrorEntry_code = Lens.lens (\BatchStartRecommendationsErrorEntry' {code} -> code) (\s@BatchStartRecommendationsErrorEntry' {} a -> s {code = a} :: BatchStartRecommendationsErrorEntry)

-- | The identifier of the source database.
batchStartRecommendationsErrorEntry_databaseId :: Lens.Lens' BatchStartRecommendationsErrorEntry (Prelude.Maybe Prelude.Text)
batchStartRecommendationsErrorEntry_databaseId = Lens.lens (\BatchStartRecommendationsErrorEntry' {databaseId} -> databaseId) (\s@BatchStartRecommendationsErrorEntry' {} a -> s {databaseId = a} :: BatchStartRecommendationsErrorEntry)

-- | The information about the error.
batchStartRecommendationsErrorEntry_message :: Lens.Lens' BatchStartRecommendationsErrorEntry (Prelude.Maybe Prelude.Text)
batchStartRecommendationsErrorEntry_message = Lens.lens (\BatchStartRecommendationsErrorEntry' {message} -> message) (\s@BatchStartRecommendationsErrorEntry' {} a -> s {message = a} :: BatchStartRecommendationsErrorEntry)

instance
  Data.FromJSON
    BatchStartRecommendationsErrorEntry
  where
  parseJSON =
    Data.withObject
      "BatchStartRecommendationsErrorEntry"
      ( \x ->
          BatchStartRecommendationsErrorEntry'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "DatabaseId")
            Prelude.<*> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    BatchStartRecommendationsErrorEntry
  where
  hashWithSalt
    _salt
    BatchStartRecommendationsErrorEntry' {..} =
      _salt
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` databaseId
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    BatchStartRecommendationsErrorEntry
  where
  rnf BatchStartRecommendationsErrorEntry' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf databaseId
      `Prelude.seq` Prelude.rnf message
