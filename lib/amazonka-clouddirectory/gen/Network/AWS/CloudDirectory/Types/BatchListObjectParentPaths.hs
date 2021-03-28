{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
  ( BatchListObjectParentPaths (..)
  -- * Smart constructor
  , mkBatchListObjectParentPaths
  -- * Lenses
  , bloppObjectReference
  , bloppMaxResults
  , bloppNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a 'BatchRead' operation. For more information, see 'ListObjectParentPaths' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListObjectParentPaths' smart constructor.
data BatchListObjectParentPaths = BatchListObjectParentPaths'
  { objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object whose attributes will be listed.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectParentPaths' value with any optional fields omitted.
mkBatchListObjectParentPaths
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchListObjectParentPaths
mkBatchListObjectParentPaths objectReference
  = BatchListObjectParentPaths'{objectReference,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloppObjectReference :: Lens.Lens' BatchListObjectParentPaths Types.ObjectReference
bloppObjectReference = Lens.field @"objectReference"
{-# INLINEABLE bloppObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloppMaxResults :: Lens.Lens' BatchListObjectParentPaths (Core.Maybe Core.Natural)
bloppMaxResults = Lens.field @"maxResults"
{-# INLINEABLE bloppMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloppNextToken :: Lens.Lens' BatchListObjectParentPaths (Core.Maybe Types.NextToken)
bloppNextToken = Lens.field @"nextToken"
{-# INLINEABLE bloppNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectParentPaths where
        toJSON BatchListObjectParentPaths{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
