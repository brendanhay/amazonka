{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
  ( BatchListAttachedIndices (..)
  -- * Smart constructor
  , mkBatchListAttachedIndices
  -- * Lenses
  , blaiTargetReference
  , blaiMaxResults
  , blaiNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Lists indices attached to an object inside a 'BatchRead' operation. For more information, see 'ListAttachedIndices' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListAttachedIndices' smart constructor.
data BatchListAttachedIndices = BatchListAttachedIndices'
  { targetReference :: Types.ObjectReference
    -- ^ A reference to the object that has indices attached.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListAttachedIndices' value with any optional fields omitted.
mkBatchListAttachedIndices
    :: Types.ObjectReference -- ^ 'targetReference'
    -> BatchListAttachedIndices
mkBatchListAttachedIndices targetReference
  = BatchListAttachedIndices'{targetReference,
                              maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | A reference to the object that has indices attached.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaiTargetReference :: Lens.Lens' BatchListAttachedIndices Types.ObjectReference
blaiTargetReference = Lens.field @"targetReference"
{-# INLINEABLE blaiTargetReference #-}
{-# DEPRECATED targetReference "Use generic-lens or generic-optics with 'targetReference' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaiMaxResults :: Lens.Lens' BatchListAttachedIndices (Core.Maybe Core.Natural)
blaiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blaiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blaiNextToken :: Lens.Lens' BatchListAttachedIndices (Core.Maybe Types.NextToken)
blaiNextToken = Lens.field @"nextToken"
{-# INLINEABLE blaiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListAttachedIndices where
        toJSON BatchListAttachedIndices{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TargetReference" Core..= targetReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
