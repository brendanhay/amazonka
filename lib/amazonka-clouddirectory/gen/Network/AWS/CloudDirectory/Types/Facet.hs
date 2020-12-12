{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Facet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Facet
  ( Facet (..),

    -- * Smart constructor
    mkFacet,

    -- * Lenses
    fFacetStyle,
    fObjectType,
    fName,
  )
where

import Network.AWS.CloudDirectory.Types.FacetStyle
import Network.AWS.CloudDirectory.Types.ObjectType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that contains @Name@ , @ARN@ , @Attributes@ , @'Rule' s@ , and @ObjectTypes@ . See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_whatarefacets.html Facets> for more information.
--
-- /See:/ 'mkFacet' smart constructor.
data Facet = Facet'
  { facetStyle :: Lude.Maybe FacetStyle,
    objectType :: Lude.Maybe ObjectType,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Facet' with the minimum fields required to make a request.
--
-- * 'facetStyle' - There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
-- * 'name' - The name of the 'Facet' .
-- * 'objectType' - The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
mkFacet ::
  Facet
mkFacet =
  Facet'
    { facetStyle = Lude.Nothing,
      objectType = Lude.Nothing,
      name = Lude.Nothing
    }

-- | There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
--
-- /Note:/ Consider using 'facetStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFacetStyle :: Lens.Lens' Facet (Lude.Maybe FacetStyle)
fFacetStyle = Lens.lens (facetStyle :: Facet -> Lude.Maybe FacetStyle) (\s a -> s {facetStyle = a} :: Facet)
{-# DEPRECATED fFacetStyle "Use generic-lens or generic-optics with 'facetStyle' instead." #-}

-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- /Note:/ Consider using 'objectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fObjectType :: Lens.Lens' Facet (Lude.Maybe ObjectType)
fObjectType = Lens.lens (objectType :: Facet -> Lude.Maybe ObjectType) (\s a -> s {objectType = a} :: Facet)
{-# DEPRECATED fObjectType "Use generic-lens or generic-optics with 'objectType' instead." #-}

-- | The name of the 'Facet' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Facet (Lude.Maybe Lude.Text)
fName = Lens.lens (name :: Facet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Facet)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Facet where
  parseJSON =
    Lude.withObject
      "Facet"
      ( \x ->
          Facet'
            Lude.<$> (x Lude..:? "FacetStyle")
            Lude.<*> (x Lude..:? "ObjectType")
            Lude.<*> (x Lude..:? "Name")
      )
