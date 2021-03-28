{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectParents
  ( BatchListObjectParents (..)
  -- * Smart constructor
  , mkBatchListObjectParents
  -- * Lenses
  , blopObjectReference
  , blopMaxResults
  , blopNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkBatchListObjectParents' smart constructor.
data BatchListObjectParents = BatchListObjectParents'
  { objectReference :: Types.ObjectReference
  , maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectParents' value with any optional fields omitted.
mkBatchListObjectParents
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchListObjectParents
mkBatchListObjectParents objectReference
  = BatchListObjectParents'{objectReference,
                            maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopObjectReference :: Lens.Lens' BatchListObjectParents Types.ObjectReference
blopObjectReference = Lens.field @"objectReference"
{-# INLINEABLE blopObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopMaxResults :: Lens.Lens' BatchListObjectParents (Core.Maybe Core.Natural)
blopMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blopMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopNextToken :: Lens.Lens' BatchListObjectParents (Core.Maybe Types.NextToken)
blopNextToken = Lens.field @"nextToken"
{-# INLINEABLE blopNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectParents where
        toJSON BatchListObjectParents{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
