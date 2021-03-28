{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectChildren
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectChildren
  ( BatchListObjectChildren (..)
  -- * Smart constructor
  , mkBatchListObjectChildren
  -- * Lenses
  , blocObjectReference
  , blocMaxResults
  , blocNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectChildren' operation.
--
-- /See:/ 'mkBatchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { objectReference :: Types.ObjectReference
    -- ^ Reference of the object for which child objects are being listed.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of items to be retrieved in a single call. This is an approximate number.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectChildren' value with any optional fields omitted.
mkBatchListObjectChildren
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchListObjectChildren
mkBatchListObjectChildren objectReference
  = BatchListObjectChildren'{objectReference,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Reference of the object for which child objects are being listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocObjectReference :: Lens.Lens' BatchListObjectChildren Types.ObjectReference
blocObjectReference = Lens.field @"objectReference"
{-# INLINEABLE blocObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocMaxResults :: Lens.Lens' BatchListObjectChildren (Core.Maybe Core.Natural)
blocMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blocMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blocNextToken :: Lens.Lens' BatchListObjectChildren (Core.Maybe Types.NextToken)
blocNextToken = Lens.field @"nextToken"
{-# INLINEABLE blocNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectChildren where
        toJSON BatchListObjectChildren{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
