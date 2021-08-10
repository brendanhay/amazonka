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
-- Module      : Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult where

import Network.AWS.Comprehend.Types.Entity
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectEntitiesItemResult' smart constructor.
data BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | One or more Entity objects, one for each entity detected in the
    -- document.
    entities :: Prelude.Maybe [Entity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectEntitiesItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchDetectEntitiesItemResult_index' - The zero-based index of the document in the input list.
--
-- 'entities', 'batchDetectEntitiesItemResult_entities' - One or more Entity objects, one for each entity detected in the
-- document.
newBatchDetectEntitiesItemResult ::
  BatchDetectEntitiesItemResult
newBatchDetectEntitiesItemResult =
  BatchDetectEntitiesItemResult'
    { index =
        Prelude.Nothing,
      entities = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchDetectEntitiesItemResult_index :: Lens.Lens' BatchDetectEntitiesItemResult (Prelude.Maybe Prelude.Int)
batchDetectEntitiesItemResult_index = Lens.lens (\BatchDetectEntitiesItemResult' {index} -> index) (\s@BatchDetectEntitiesItemResult' {} a -> s {index = a} :: BatchDetectEntitiesItemResult)

-- | One or more Entity objects, one for each entity detected in the
-- document.
batchDetectEntitiesItemResult_entities :: Lens.Lens' BatchDetectEntitiesItemResult (Prelude.Maybe [Entity])
batchDetectEntitiesItemResult_entities = Lens.lens (\BatchDetectEntitiesItemResult' {entities} -> entities) (\s@BatchDetectEntitiesItemResult' {} a -> s {entities = a} :: BatchDetectEntitiesItemResult) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON BatchDetectEntitiesItemResult where
  parseJSON =
    Core.withObject
      "BatchDetectEntitiesItemResult"
      ( \x ->
          BatchDetectEntitiesItemResult'
            Prelude.<$> (x Core..:? "Index")
            Prelude.<*> (x Core..:? "Entities" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    BatchDetectEntitiesItemResult

instance Prelude.NFData BatchDetectEntitiesItemResult
