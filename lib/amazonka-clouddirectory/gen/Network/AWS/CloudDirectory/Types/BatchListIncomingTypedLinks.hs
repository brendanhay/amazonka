{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
  ( BatchListIncomingTypedLinks (..),

    -- * Smart constructor
    mkBatchListIncomingTypedLinks,

    -- * Lenses
    blitlsFilterAttributeRanges,
    blitlsNextToken,
    blitlsFilterTypedLink,
    blitlsMaxResults,
    blitlsObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListIncomingTypedLinks' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchListIncomingTypedLinks' smart constructor.
data BatchListIncomingTypedLinks = BatchListIncomingTypedLinks'
  { filterAttributeRanges ::
      Lude.Maybe
        [TypedLinkAttributeRange],
    nextToken :: Lude.Maybe Lude.Text,
    filterTypedLink ::
      Lude.Maybe
        TypedLinkSchemaAndFacetName,
    maxResults ::
      Lude.Maybe Lude.Natural,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchListIncomingTypedLinks' with the minimum fields required to make a request.
--
-- * 'filterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
-- * 'filterTypedLink' - Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
-- * 'maxResults' - The maximum number of results to retrieve.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - The reference that identifies the object whose attributes will be listed.
mkBatchListIncomingTypedLinks ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListIncomingTypedLinks
mkBatchListIncomingTypedLinks pObjectReference_ =
  BatchListIncomingTypedLinks'
    { filterAttributeRanges =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      filterTypedLink = Lude.Nothing,
      maxResults = Lude.Nothing,
      objectReference = pObjectReference_
    }

-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- /Note:/ Consider using 'filterAttributeRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlsFilterAttributeRanges :: Lens.Lens' BatchListIncomingTypedLinks (Lude.Maybe [TypedLinkAttributeRange])
blitlsFilterAttributeRanges = Lens.lens (filterAttributeRanges :: BatchListIncomingTypedLinks -> Lude.Maybe [TypedLinkAttributeRange]) (\s a -> s {filterAttributeRanges = a} :: BatchListIncomingTypedLinks)
{-# DEPRECATED blitlsFilterAttributeRanges "Use generic-lens or generic-optics with 'filterAttributeRanges' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlsNextToken :: Lens.Lens' BatchListIncomingTypedLinks (Lude.Maybe Lude.Text)
blitlsNextToken = Lens.lens (nextToken :: BatchListIncomingTypedLinks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListIncomingTypedLinks)
{-# DEPRECATED blitlsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- /Note:/ Consider using 'filterTypedLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlsFilterTypedLink :: Lens.Lens' BatchListIncomingTypedLinks (Lude.Maybe TypedLinkSchemaAndFacetName)
blitlsFilterTypedLink = Lens.lens (filterTypedLink :: BatchListIncomingTypedLinks -> Lude.Maybe TypedLinkSchemaAndFacetName) (\s a -> s {filterTypedLink = a} :: BatchListIncomingTypedLinks)
{-# DEPRECATED blitlsFilterTypedLink "Use generic-lens or generic-optics with 'filterTypedLink' instead." #-}

-- | The maximum number of results to retrieve.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlsMaxResults :: Lens.Lens' BatchListIncomingTypedLinks (Lude.Maybe Lude.Natural)
blitlsMaxResults = Lens.lens (maxResults :: BatchListIncomingTypedLinks -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListIncomingTypedLinks)
{-# DEPRECATED blitlsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
blitlsObjectReference :: Lens.Lens' BatchListIncomingTypedLinks ObjectReference
blitlsObjectReference = Lens.lens (objectReference :: BatchListIncomingTypedLinks -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListIncomingTypedLinks)
{-# DEPRECATED blitlsObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchListIncomingTypedLinks where
  toJSON BatchListIncomingTypedLinks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterAttributeRanges" Lude..=) Lude.<$> filterAttributeRanges,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("FilterTypedLink" Lude..=) Lude.<$> filterTypedLink,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
