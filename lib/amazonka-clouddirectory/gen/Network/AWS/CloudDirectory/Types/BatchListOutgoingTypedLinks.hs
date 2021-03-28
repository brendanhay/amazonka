{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
  ( BatchListOutgoingTypedLinks (..)
  -- * Smart constructor
  , mkBatchListOutgoingTypedLinks
  -- * Lenses
  , blotlObjectReference
  , blotlFilterAttributeRanges
  , blotlFilterTypedLink
  , blotlMaxResults
  , blotlNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectReference as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange as Types
import qualified Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListOutgoingTypedLinks' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListOutgoingTypedLinks' smart constructor.
data BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks'
  { objectReference :: Types.ObjectReference
    -- ^ The reference that identifies the object whose attributes will be listed.
  , filterAttributeRanges :: Core.Maybe [Types.TypedLinkAttributeRange]
    -- ^ Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
  , filterTypedLink :: Core.Maybe Types.TypedLinkSchemaAndFacetName
    -- ^ Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchListOutgoingTypedLinks' value with any optional fields omitted.
mkBatchListOutgoingTypedLinks
    :: Types.ObjectReference -- ^ 'objectReference'
    -> BatchListOutgoingTypedLinks
mkBatchListOutgoingTypedLinks objectReference
  = BatchListOutgoingTypedLinks'{objectReference,
                                 filterAttributeRanges = Core.Nothing,
                                 filterTypedLink = Core.Nothing, maxResults = Core.Nothing,
                                 nextToken = Core.Nothing}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlObjectReference :: Lens.Lens' BatchListOutgoingTypedLinks Types.ObjectReference
blotlObjectReference = Lens.field @"objectReference"
{-# INLINEABLE blotlObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlFilterAttributeRanges :: Lens.Lens' BatchListOutgoingTypedLinks (Core.Maybe [Types.TypedLinkAttributeRange])
blotlFilterAttributeRanges = Lens.field @"filterAttributeRanges"
{-# INLINEABLE blotlFilterAttributeRanges #-}
{-# DEPRECATED filterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead"  #-}

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlFilterTypedLink :: Lens.Lens' BatchListOutgoingTypedLinks (Core.Maybe Types.TypedLinkSchemaAndFacetName)
blotlFilterTypedLink = Lens.field @"filterTypedLink"
{-# INLINEABLE blotlFilterTypedLink #-}
{-# DEPRECATED filterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead"  #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlMaxResults :: Lens.Lens' BatchListOutgoingTypedLinks (Core.Maybe Core.Natural)
blotlMaxResults = Lens.field @"maxResults"
{-# INLINEABLE blotlMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlNextToken :: Lens.Lens' BatchListOutgoingTypedLinks (Core.Maybe Types.NextToken)
blotlNextToken = Lens.field @"nextToken"
{-# INLINEABLE blotlNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListOutgoingTypedLinks where
        toJSON BatchListOutgoingTypedLinks{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  ("FilterAttributeRanges" Core..=) Core.<$> filterAttributeRanges,
                  ("FilterTypedLink" Core..=) Core.<$> filterTypedLink,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])
