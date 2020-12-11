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
    batRangesOnIndexedValues,
    batNextToken,
    batMaxResults,
    batIndexReference,
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
  { rangesOnIndexedValues ::
      Lude.Maybe [ObjectAttributeRange],
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    indexReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListIndex' with the minimum fields required to make a request.
--
-- * 'indexReference' - The reference to the index to list.
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'rangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
mkBatchListIndex ::
  -- | 'indexReference'
  ObjectReference ->
  BatchListIndex
mkBatchListIndex pIndexReference_ =
  BatchListIndex'
    { rangesOnIndexedValues = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      indexReference = pIndexReference_
    }

-- | Specifies the ranges of indexed values that you want to query.
--
-- /Note:/ Consider using 'rangesOnIndexedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batRangesOnIndexedValues :: Lens.Lens' BatchListIndex (Lude.Maybe [ObjectAttributeRange])
batRangesOnIndexedValues = Lens.lens (rangesOnIndexedValues :: BatchListIndex -> Lude.Maybe [ObjectAttributeRange]) (\s a -> s {rangesOnIndexedValues = a} :: BatchListIndex)
{-# DEPRECATED batRangesOnIndexedValues "Use generic-lens or generic-optics with 'rangesOnIndexedValues' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batNextToken :: Lens.Lens' BatchListIndex (Lude.Maybe Lude.Text)
batNextToken = Lens.lens (nextToken :: BatchListIndex -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListIndex)
{-# DEPRECATED batNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batMaxResults :: Lens.Lens' BatchListIndex (Lude.Maybe Lude.Natural)
batMaxResults = Lens.lens (maxResults :: BatchListIndex -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListIndex)
{-# DEPRECATED batMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The reference to the index to list.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batIndexReference :: Lens.Lens' BatchListIndex ObjectReference
batIndexReference = Lens.lens (indexReference :: BatchListIndex -> ObjectReference) (\s a -> s {indexReference = a} :: BatchListIndex)
{-# DEPRECATED batIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

instance Lude.ToJSON BatchListIndex where
  toJSON BatchListIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RangesOnIndexedValues" Lude..=) Lude.<$> rangesOnIndexedValues,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("IndexReference" Lude..= indexReference)
          ]
      )
