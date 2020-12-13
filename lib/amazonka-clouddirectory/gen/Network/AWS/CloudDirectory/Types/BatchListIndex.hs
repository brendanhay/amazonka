{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIndex
  ( BatchListIndex (..),

    -- * Smart constructor
    mkBatchListIndex,

    -- * Lenses
    blifRangesOnIndexedValues,
    blifIndexReference,
    blifNextToken,
    blifMaxResults,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Lists objects attached to the specified index inside a 'BatchRead' operation. For more information, see 'ListIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { -- | Specifies the ranges of indexed values that you want to query.
    rangesOnIndexedValues :: Lude.Maybe [ObjectAttributeRange],
    -- | The reference to the index to list.
    indexReference :: ObjectReference,
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListIndex' with the minimum fields required to make a request.
--
-- * 'rangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
-- * 'indexReference' - The reference to the index to list.
-- * 'nextToken' - The pagination token.
-- * 'maxResults' - The maximum number of results to retrieve.
mkBatchListIndex ::
  -- | 'indexReference'
  ObjectReference ->
  BatchListIndex
mkBatchListIndex pIndexReference_ =
  BatchListIndex'
    { rangesOnIndexedValues = Lude.Nothing,
      indexReference = pIndexReference_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies the ranges of indexed values that you want to query.
--
-- /Note:/ Consider using 'rangesOnIndexedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blifRangesOnIndexedValues :: Lens.Lens' BatchListIndex (Lude.Maybe [ObjectAttributeRange])
blifRangesOnIndexedValues = Lens.lens (rangesOnIndexedValues :: BatchListIndex -> Lude.Maybe [ObjectAttributeRange]) (\s a -> s {rangesOnIndexedValues = a} :: BatchListIndex)
{-# DEPRECATED blifRangesOnIndexedValues "Use generic-lens or generic-optics with 'rangesOnIndexedValues' instead." #-}

-- | The reference to the index to list.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blifIndexReference :: Lens.Lens' BatchListIndex ObjectReference
blifIndexReference = Lens.lens (indexReference :: BatchListIndex -> ObjectReference) (\s a -> s {indexReference = a} :: BatchListIndex)
{-# DEPRECATED blifIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blifNextToken :: Lens.Lens' BatchListIndex (Lude.Maybe Lude.Text)
blifNextToken = Lens.lens (nextToken :: BatchListIndex -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListIndex)
{-# DEPRECATED blifNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blifMaxResults :: Lens.Lens' BatchListIndex (Lude.Maybe Lude.Natural)
blifMaxResults = Lens.lens (maxResults :: BatchListIndex -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListIndex)
{-# DEPRECATED blifMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.ToJSON BatchListIndex where
  toJSON BatchListIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RangesOnIndexedValues" Lude..=) Lude.<$> rangesOnIndexedValues,
            Lude.Just ("IndexReference" Lude..= indexReference),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )
