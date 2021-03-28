{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListIndex
  ( BatchListIndex (..)
  -- * Smart constructor
  , mkBatchListIndex
  -- * Lenses
  , bliIndexReference
  , bliMaxResults
  , bliNextToken
  , bliRangesOnIndexedValues
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectAttributeRange as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Lists objects attached to the specified index inside a 'BatchRead' operation. For more information, see 'ListIndex' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { indexReference :: Types.ObjectReference
    -- ^ The reference to the index to list.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  , rangesOnIndexedValues :: Core.Maybe [Types.ObjectAttributeRange]
    -- ^ Specifies the ranges of indexed values that you want to query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchListIndex' value with any optional fields omitted.
mkBatchListIndex
    :: Types.ObjectReference -- ^ 'indexReference'
    -> BatchListIndex
mkBatchListIndex indexReference
  = BatchListIndex'{indexReference, maxResults = Core.Nothing,
                    nextToken = Core.Nothing, rangesOnIndexedValues = Core.Nothing}

-- | The reference to the index to list.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliIndexReference :: Lens.Lens' BatchListIndex Types.ObjectReference
bliIndexReference = Lens.field @"indexReference"
{-# INLINEABLE bliIndexReference #-}
{-# DEPRECATED indexReference "Use generic-lens or generic-optics with 'indexReference' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliMaxResults :: Lens.Lens' BatchListIndex (Core.Maybe Core.Natural)
bliMaxResults = Lens.field @"maxResults"
{-# INLINEABLE bliMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliNextToken :: Lens.Lens' BatchListIndex (Core.Maybe Types.NextToken)
bliNextToken = Lens.field @"nextToken"
{-# INLINEABLE bliNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specifies the ranges of indexed values that you want to query.
--
-- /Note:/ Consider using 'rangesOnIndexedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bliRangesOnIndexedValues :: Lens.Lens' BatchListIndex (Core.Maybe [Types.ObjectAttributeRange])
bliRangesOnIndexedValues = Lens.field @"rangesOnIndexedValues"
{-# INLINEABLE bliRangesOnIndexedValues #-}
{-# DEPRECATED rangesOnIndexedValues "Use generic-lens or generic-optics with 'rangesOnIndexedValues' instead"  #-}

instance Core.FromJSON BatchListIndex where
        toJSON BatchListIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IndexReference" Core..= indexReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("RangesOnIndexedValues" Core..=) Core.<$> rangesOnIndexedValues])
