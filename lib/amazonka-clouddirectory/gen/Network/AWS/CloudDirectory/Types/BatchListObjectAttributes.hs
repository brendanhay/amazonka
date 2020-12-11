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
    bloaFacetFilter,
    bloaNextToken,
    bloaMaxResults,
    bloaObjectReference,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'ListObjectAttributes' operation.
--
-- /See:/ 'mkBatchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { facetFilter ::
      Lude.Maybe SchemaFacet,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'BatchListObjectAttributes' with the minimum fields required to make a request.
--
-- * 'facetFilter' - Used to filter the list of object attributes that are associated with a certain facet.
-- * 'maxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
-- * 'nextToken' - The pagination token.
-- * 'objectReference' - Reference of the object whose attributes need to be listed.
mkBatchListObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  BatchListObjectAttributes
mkBatchListObjectAttributes pObjectReference_ =
  BatchListObjectAttributes'
    { facetFilter = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      objectReference = pObjectReference_
    }

-- | Used to filter the list of object attributes that are associated with a certain facet.
--
-- /Note:/ Consider using 'facetFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaFacetFilter :: Lens.Lens' BatchListObjectAttributes (Lude.Maybe SchemaFacet)
bloaFacetFilter = Lens.lens (facetFilter :: BatchListObjectAttributes -> Lude.Maybe SchemaFacet) (\s a -> s {facetFilter = a} :: BatchListObjectAttributes)
{-# DEPRECATED bloaFacetFilter "Use generic-lens or generic-optics with 'facetFilter' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaNextToken :: Lens.Lens' BatchListObjectAttributes (Lude.Maybe Lude.Text)
bloaNextToken = Lens.lens (nextToken :: BatchListObjectAttributes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: BatchListObjectAttributes)
{-# DEPRECATED bloaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaMaxResults :: Lens.Lens' BatchListObjectAttributes (Lude.Maybe Lude.Natural)
bloaMaxResults = Lens.lens (maxResults :: BatchListObjectAttributes -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: BatchListObjectAttributes)
{-# DEPRECATED bloaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Reference of the object whose attributes need to be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloaObjectReference :: Lens.Lens' BatchListObjectAttributes ObjectReference
bloaObjectReference = Lens.lens (objectReference :: BatchListObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: BatchListObjectAttributes)
{-# DEPRECATED bloaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.ToJSON BatchListObjectAttributes where
  toJSON BatchListObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FacetFilter" Lude..=) Lude.<$> facetFilter,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )
