{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
  ( BatchListObjectPolicies (..)
  -- * Smart constructor
  , mkBatchListObjectPolicies
  -- * Lenses
  , blopsObjectReference
  , blopsMaxResults
  , blopsNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object whose attributes will be listed.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectPolicies' value with any optional fields omitted.
mkBatchListObjectPolicies
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchListObjectPolicies
mkBatchListObjectPolicies objectReference
  = BatchListObjectPolicies'{objectReference,
                             maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsObjectReference :: Lens.Lens' BatchListObjectPolicies Types.ObjectReference
blopsObjectReference = Lens.field @"objectReference"
{-# INLINEABLE blopsObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsMaxResults :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Core.Natural)
blopsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blopsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsNextToken :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Types.NextToken)
blopsNextToken = Lens.field @"nextToken"
{-# INLINEABLE blopsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectPolicies where
        toJSON BatchListObjectPolicies{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
