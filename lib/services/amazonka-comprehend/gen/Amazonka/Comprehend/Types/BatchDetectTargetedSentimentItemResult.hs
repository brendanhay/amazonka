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
-- Module      : Amazonka.Comprehend.Types.BatchDetectTargetedSentimentItemResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectTargetedSentimentItemResult where

import Amazonka.Comprehend.Types.TargetedSentimentEntity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Analysis results for one of the documents in the batch.
--
-- /See:/ 'newBatchDetectTargetedSentimentItemResult' smart constructor.
data BatchDetectTargetedSentimentItemResult = BatchDetectTargetedSentimentItemResult'
  { -- | An array of targeted sentiment entities.
    entities :: Prelude.Maybe [TargetedSentimentEntity],
    -- | The zero-based index of this result in the input list.
    index :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectTargetedSentimentItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'batchDetectTargetedSentimentItemResult_entities' - An array of targeted sentiment entities.
--
-- 'index', 'batchDetectTargetedSentimentItemResult_index' - The zero-based index of this result in the input list.
newBatchDetectTargetedSentimentItemResult ::
  BatchDetectTargetedSentimentItemResult
newBatchDetectTargetedSentimentItemResult =
  BatchDetectTargetedSentimentItemResult'
    { entities =
        Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | An array of targeted sentiment entities.
batchDetectTargetedSentimentItemResult_entities :: Lens.Lens' BatchDetectTargetedSentimentItemResult (Prelude.Maybe [TargetedSentimentEntity])
batchDetectTargetedSentimentItemResult_entities = Lens.lens (\BatchDetectTargetedSentimentItemResult' {entities} -> entities) (\s@BatchDetectTargetedSentimentItemResult' {} a -> s {entities = a} :: BatchDetectTargetedSentimentItemResult) Prelude.. Lens.mapping Lens.coerced

-- | The zero-based index of this result in the input list.
batchDetectTargetedSentimentItemResult_index :: Lens.Lens' BatchDetectTargetedSentimentItemResult (Prelude.Maybe Prelude.Int)
batchDetectTargetedSentimentItemResult_index = Lens.lens (\BatchDetectTargetedSentimentItemResult' {index} -> index) (\s@BatchDetectTargetedSentimentItemResult' {} a -> s {index = a} :: BatchDetectTargetedSentimentItemResult)

instance
  Data.FromJSON
    BatchDetectTargetedSentimentItemResult
  where
  parseJSON =
    Data.withObject
      "BatchDetectTargetedSentimentItemResult"
      ( \x ->
          BatchDetectTargetedSentimentItemResult'
            Prelude.<$> (x Data..:? "Entities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Index")
      )

instance
  Prelude.Hashable
    BatchDetectTargetedSentimentItemResult
  where
  hashWithSalt
    _salt
    BatchDetectTargetedSentimentItemResult' {..} =
      _salt
        `Prelude.hashWithSalt` entities
        `Prelude.hashWithSalt` index

instance
  Prelude.NFData
    BatchDetectTargetedSentimentItemResult
  where
  rnf BatchDetectTargetedSentimentItemResult' {..} =
    Prelude.rnf entities
      `Prelude.seq` Prelude.rnf index
