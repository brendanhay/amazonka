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
-- Module      : Amazonka.CloudDirectory.Types.BatchListIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchListIndex where

import Amazonka.CloudDirectory.Types.ObjectAttributeRange
import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Lists objects attached to the specified index inside a BatchRead
-- operation. For more information, see ListIndex and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Prelude.Maybe [ObjectAttributeRange],
    -- | The reference to the index to list.
    indexReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchListIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'batchListIndex_nextToken' - The pagination token.
--
-- 'maxResults', 'batchListIndex_maxResults' - The maximum number of results to retrieve.
--
-- 'rangesOnIndexedValues', 'batchListIndex_rangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
--
-- 'indexReference', 'batchListIndex_indexReference' - The reference to the index to list.
newBatchListIndex ::
  -- | 'indexReference'
  ObjectReference ->
  BatchListIndex
newBatchListIndex pIndexReference_ =
  BatchListIndex'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      rangesOnIndexedValues = Prelude.Nothing,
      indexReference = pIndexReference_
    }

-- | The pagination token.
batchListIndex_nextToken :: Lens.Lens' BatchListIndex (Prelude.Maybe Prelude.Text)
batchListIndex_nextToken = Lens.lens (\BatchListIndex' {nextToken} -> nextToken) (\s@BatchListIndex' {} a -> s {nextToken = a} :: BatchListIndex)

-- | The maximum number of results to retrieve.
batchListIndex_maxResults :: Lens.Lens' BatchListIndex (Prelude.Maybe Prelude.Natural)
batchListIndex_maxResults = Lens.lens (\BatchListIndex' {maxResults} -> maxResults) (\s@BatchListIndex' {} a -> s {maxResults = a} :: BatchListIndex)

-- | Specifies the ranges of indexed values that you want to query.
batchListIndex_rangesOnIndexedValues :: Lens.Lens' BatchListIndex (Prelude.Maybe [ObjectAttributeRange])
batchListIndex_rangesOnIndexedValues = Lens.lens (\BatchListIndex' {rangesOnIndexedValues} -> rangesOnIndexedValues) (\s@BatchListIndex' {} a -> s {rangesOnIndexedValues = a} :: BatchListIndex) Prelude.. Lens.mapping Lens.coerced

-- | The reference to the index to list.
batchListIndex_indexReference :: Lens.Lens' BatchListIndex ObjectReference
batchListIndex_indexReference = Lens.lens (\BatchListIndex' {indexReference} -> indexReference) (\s@BatchListIndex' {} a -> s {indexReference = a} :: BatchListIndex)

instance Prelude.Hashable BatchListIndex where
  hashWithSalt _salt BatchListIndex' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` rangesOnIndexedValues
      `Prelude.hashWithSalt` indexReference

instance Prelude.NFData BatchListIndex where
  rnf BatchListIndex' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf rangesOnIndexedValues
      `Prelude.seq` Prelude.rnf indexReference

instance Core.ToJSON BatchListIndex where
  toJSON BatchListIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("RangesOnIndexedValues" Core..=)
              Prelude.<$> rangesOnIndexedValues,
            Prelude.Just
              ("IndexReference" Core..= indexReference)
          ]
      )
