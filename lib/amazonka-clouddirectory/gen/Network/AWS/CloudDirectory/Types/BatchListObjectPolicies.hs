{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
  ( BatchListObjectPolicies (..),

    -- * Smart constructor
    mkBatchListObjectPolicies,

    -- * Lenses
    blopsObjectReference,
    blopsMaxResults,
    blopsNextToken,
  )
where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { -- | The reference that identifies the object whose attributes will be listed.
    objectReference :: Types.ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectPolicies' value with any optional fields omitted.
mkBatchListObjectPolicies ::
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchListObjectPolicies
mkBatchListObjectPolicies objectReference =
  BatchListObjectPolicies'
    { objectReference,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsObjectReference :: Lens.Lens' BatchListObjectPolicies Types.ObjectReference
blopsObjectReference = Lens.field @"objectReference"
{-# DEPRECATED blopsObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsMaxResults :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Core.Natural)
blopsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED blopsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blopsNextToken :: Lens.Lens' BatchListObjectPolicies (Core.Maybe Types.NextToken)
blopsNextToken = Lens.field @"nextToken"
{-# DEPRECATED blopsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchListObjectPolicies where
  toJSON BatchListObjectPolicies {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )
