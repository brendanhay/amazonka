{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
  ( BatchListObjectAttributes (..),

    -- * Smart constructor
    mkBatchListObjectAttributes,

    -- * Lenses
    bloaObjectReference,
    bloaFacetFilter,
    bloaMaxResults,
    bloaNextToken,
  )
where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaFacet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectAttributes' operation.
--
-- /See:/ 'mkBatchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { -- | Reference of the object whose attributes need to be listed.
    objectReference :: Types.ObjectReference,
    -- | Used to filter the list of object attributes that are associated with a certain facet.
    facetFilter :: Core.Maybe Types.SchemaFacet,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchListObjectAttributes' value with any optional fields omitted.
mkBatchListObjectAttributes ::
  -- | 'objectReference'
  Types.ObjectReference ->
  BatchListObjectAttributes
mkBatchListObjectAttributes objectReference =
  BatchListObjectAttributes'
    { objectReference,
      facetFilter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Reference of the object whose attributes need to be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaObjectReference :: Lens.Lens' BatchListObjectAttributes Types.ObjectReference
bloaObjectReference = Lens.field @"objectReference"
{-# DEPRECATED bloaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Used to filter the list of object attributes that are associated with a certain facet.
--
-- /Note:/ Consider using 'facetFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaFacetFilter :: Lens.Lens' BatchListObjectAttributes (Core.Maybe Types.SchemaFacet)
bloaFacetFilter = Lens.field @"facetFilter"
{-# DEPRECATED bloaFacetFilter "Use generic-lens or generic-optics with 'facetFilter' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaMaxResults :: Lens.Lens' BatchListObjectAttributes (Core.Maybe Core.Natural)
bloaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED bloaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaNextToken :: Lens.Lens' BatchListObjectAttributes (Core.Maybe Types.NextToken)
bloaNextToken = Lens.field @"nextToken"
{-# DEPRECATED bloaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON BatchListObjectAttributes where
  toJSON BatchListObjectAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("FacetFilter" Core..=) Core.<$> facetFilter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )
