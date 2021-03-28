{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Facet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.Facet
  ( Facet (..)
  -- * Smart constructor
  , mkFacet
  -- * Lenses
  , fFacetStyle
  , fName
  , fObjectType
  ) where

import qualified Network.AWS.CloudDirectory.Types.FacetName as Types
import qualified Network.AWS.CloudDirectory.Types.FacetStyle as Types
import qualified Network.AWS.CloudDirectory.Types.ObjectType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that contains @Name@ , @ARN@ , @Attributes@ , @'Rule' s@ , and @ObjectTypes@ . See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_whatarefacets.html Facets> for more information.
--
-- /See:/ 'mkFacet' smart constructor.
data Facet = Facet'
  { facetStyle :: Core.Maybe Types.FacetStyle
    -- ^ There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
  , name :: Core.Maybe Types.FacetName
    -- ^ The name of the 'Facet' .
  , objectType :: Core.Maybe Types.ObjectType
    -- ^ The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Facet' value with any optional fields omitted.
mkFacet
    :: Facet
mkFacet
  = Facet'{facetStyle = Core.Nothing, name = Core.Nothing,
           objectType = Core.Nothing}

-- | There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
--
-- /Note:/ Consider using 'facetStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fFacetStyle :: Lens.Lens' Facet (Core.Maybe Types.FacetStyle)
fFacetStyle = Lens.field @"facetStyle"
{-# INLINEABLE fFacetStyle #-}
{-# DEPRECATED facetStyle "Use generic-lens or generic-optics with 'facetStyle' instead"  #-}

-- | The name of the 'Facet' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Facet (Core.Maybe Types.FacetName)
fName = Lens.field @"name"
{-# INLINEABLE fName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- /Note:/ Consider using 'objectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fObjectType :: Lens.Lens' Facet (Core.Maybe Types.ObjectType)
fObjectType = Lens.field @"objectType"
{-# INLINEABLE fObjectType #-}
{-# DEPRECATED objectType "Use generic-lens or generic-optics with 'objectType' instead"  #-}

instance Core.FromJSON Facet where
        parseJSON
          = Core.withObject "Facet" Core.$
              \ x ->
                Facet' Core.<$>
                  (x Core..:? "FacetStyle") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "ObjectType"
