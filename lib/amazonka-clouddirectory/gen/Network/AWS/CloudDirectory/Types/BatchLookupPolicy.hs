{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchLookupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchLookupPolicy
  ( BatchLookupPolicy (..),

    -- * Smart constructor
    mkBatchLookupPolicy,

    -- * Lenses
    blpObjectReference,
    blpMaxResults,
    blpNextToken,
  )
where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Lists all policies from the root of the Directory to the object specified inside a 'BatchRead' operation. For more information, see 'LookupPolicy' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { -- | Reference that identifies the object whose policies will be looked up.
    objectReference :: Types.ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchLookupPolicy' value with any optional fields omitted.
mkBatchLookupPolicy ::
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchLookupPolicy
mkBatchLookupPolicy objectReference =
  BatchLookupPolicy'
    { objectReference,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Reference that identifies the object whose policies will be looked up.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpObjectReference :: Lens.Lens' BatchLookupPolicy Types.ObjectReference
blpObjectReference = Lens.field @"objectReference"
{-# DEPRECATED blpObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpMaxResults :: Lens.Lens' BatchLookupPolicy (Core.Maybe Core.Natural)
blpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED blpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blpNextToken :: Lens.Lens' BatchLookupPolicy (Core.Maybe Types.NextToken)
blpNextToken = Lens.field @"nextToken"
{-# DEPRECATED blpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchLookupPolicy where
  toJSON BatchLookupPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )
