{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
  ( BatchListOutgoingTypedLinks (..),

    -- * Smart constructor
    mkBatchListOutgoingTypedLinks,

    -- * Lenses
    blotlsFilterAttributeRanges,
    blotlsNextToken,
    blotlsFilterTypedLink,
    blotlsObjectReference,
    blotlsMaxResults,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListOutgoingTypedLinks' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListOutgoingTypedLinks' smart constructor.
data BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks'
  { -- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
    filterAttributeRanges :: Lude.Maybe [TypedLinkAttributeRange],
    -- | The pagination token.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
    filterTypedLink :: Lude.Maybe TypedLinkSchemaAndFacetName,
    -- | The reference that identifies the object whose attributes will be listed.
    objectReference :: ObjectReference,
    -- | The maximum number of results to retrieve.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListOutgoingTypedLinks' with the minimum fields required to make a request.
--
-- * 'filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
-- * 'nextToken' - The pagination token.
-- * 'filterTypedLink' - Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
-- * 'objectReference' - The reference that identifies the object whose attributes will be listed.
-- * 'maxResults' - The maximum number of results to retrieve.
mkBatchListOutgoingTypedLinks ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListOutgoingTypedLinks
mkBatchListOutgoingTypedLinks pObjectReference_ =
  BatchListOutgoingTypedLinks'
    { filterAttributeRanges =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      filterTypedLink = Lude.Nothing,
      objectReference = pObjectReference_,
      maxResults = Lude.Nothing
    }

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlsFilterAttributeRanges :: Lens.Lens' BatchListOutgoingTypedLinks (Lude.Maybe [TypedLinkAttributeRange])
blotlsFilterAttributeRanges = Lens.lens (filterAttributeRanges :: BatchListOutgoingTypedLinks -> Lude.Maybe [TypedLinkAttributeRange]) (\s a -> s {filterAttributeRanges = a} :: BatchListOutgoingTypedLinks)
{-# DEPRECATED blotlsFilterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlsNextToken :: Lens.Lens' BatchListOutgoingTypedLinks (Lude.Maybe Lude.Text)
blotlsNextToken = Lens.lens (nextToken :: BatchListOutgoingTypedLinks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListOutgoingTypedLinks)
{-# DEPRECATED blotlsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlsFilterTypedLink :: Lens.Lens' BatchListOutgoingTypedLinks (Lude.Maybe TypedLinkSchemaAndFacetName)
blotlsFilterTypedLink = Lens.lens (filterTypedLink :: BatchListOutgoingTypedLinks -> Lude.Maybe TypedLinkSchemaAndFacetName) (\s a -> s {filterTypedLink = a} :: BatchListOutgoingTypedLinks)
{-# DEPRECATED blotlsFilterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead." #-}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlsObjectReference :: Lens.Lens' BatchListOutgoingTypedLinks ObjectReference
blotlsObjectReference = Lens.lens (objectReference :: BatchListOutgoingTypedLinks -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListOutgoingTypedLinks)
{-# DEPRECATED blotlsObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blotlsMaxResults :: Lens.Lens' BatchListOutgoingTypedLinks (Lude.Maybe Lude.Natural)
blotlsMaxResults = Lens.lens (maxResults :: BatchListOutgoingTypedLinks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListOutgoingTypedLinks)
{-# DEPRECATED blotlsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.ToJSON BatchListOutgoingTypedLinks where
  toJSON BatchListOutgoingTypedLinks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterAttributeRanges" Lude..=) Lude.<$> filterAttributeRanges,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("FilterTypedLink" Lude..=) Lude.<$> filterTypedLink,
            Lude.Just ("ObjectReference" Lude..= objectReference),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )
