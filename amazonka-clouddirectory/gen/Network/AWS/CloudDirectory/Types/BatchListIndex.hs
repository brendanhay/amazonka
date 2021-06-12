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
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndex where

import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Lists objects attached to the specified index inside a BatchRead
-- operation. For more information, see ListIndex and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { -- | The pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Core.Maybe [ObjectAttributeRange],
    -- | The reference to the index to list.
    indexReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      rangesOnIndexedValues = Core.Nothing,
      indexReference = pIndexReference_
    }

-- | The pagination token.
batchListIndex_nextToken :: Lens.Lens' BatchListIndex (Core.Maybe Core.Text)
batchListIndex_nextToken = Lens.lens (\BatchListIndex' {nextToken} -> nextToken) (\s@BatchListIndex' {} a -> s {nextToken = a} :: BatchListIndex)

-- | The maximum number of results to retrieve.
batchListIndex_maxResults :: Lens.Lens' BatchListIndex (Core.Maybe Core.Natural)
batchListIndex_maxResults = Lens.lens (\BatchListIndex' {maxResults} -> maxResults) (\s@BatchListIndex' {} a -> s {maxResults = a} :: BatchListIndex)

-- | Specifies the ranges of indexed values that you want to query.
batchListIndex_rangesOnIndexedValues :: Lens.Lens' BatchListIndex (Core.Maybe [ObjectAttributeRange])
batchListIndex_rangesOnIndexedValues = Lens.lens (\BatchListIndex' {rangesOnIndexedValues} -> rangesOnIndexedValues) (\s@BatchListIndex' {} a -> s {rangesOnIndexedValues = a} :: BatchListIndex) Core.. Lens.mapping Lens._Coerce

-- | The reference to the index to list.
batchListIndex_indexReference :: Lens.Lens' BatchListIndex ObjectReference
batchListIndex_indexReference = Lens.lens (\BatchListIndex' {indexReference} -> indexReference) (\s@BatchListIndex' {} a -> s {indexReference = a} :: BatchListIndex)

instance Core.Hashable BatchListIndex

instance Core.NFData BatchListIndex

instance Core.ToJSON BatchListIndex where
  toJSON BatchListIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("RangesOnIndexedValues" Core..=)
              Core.<$> rangesOnIndexedValues,
            Core.Just ("IndexReference" Core..= indexReference)
          ]
      )
