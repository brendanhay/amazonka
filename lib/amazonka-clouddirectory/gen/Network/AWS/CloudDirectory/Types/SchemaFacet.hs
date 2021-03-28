{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.SchemaFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.SchemaFacet
  ( SchemaFacet (..)
  -- * Smart constructor
  , mkSchemaFacet
  -- * Lenses
  , sfFacetName
  , sfSchemaArn
  ) where

import qualified Network.AWS.CloudDirectory.Types.FacetName as Types
import qualified Network.AWS.CloudDirectory.Types.SchemaArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A facet.
--
-- /See:/ 'mkSchemaFacet' smart constructor.
data SchemaFacet = SchemaFacet'
  { facetName :: Core.Maybe Types.FacetName
    -- ^ The name of the facet.
  , schemaArn :: Core.Maybe Types.SchemaArn
    -- ^ The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaFacet' value with any optional fields omitted.
mkSchemaFacet
    :: SchemaFacet
mkSchemaFacet
  = SchemaFacet'{facetName = Core.Nothing, schemaArn = Core.Nothing}

-- | The name of the facet.
--
-- /Note:/ Consider using 'facetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFacetName :: Lens.Lens' SchemaFacet (Core.Maybe Types.FacetName)
sfFacetName = Lens.field @"facetName"
{-# INLINEABLE sfFacetName #-}
{-# DEPRECATED facetName "Use generic-lens or generic-optics with 'facetName' instead"  #-}

-- | The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSchemaArn :: Lens.Lens' SchemaFacet (Core.Maybe Types.SchemaArn)
sfSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE sfSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

instance Core.FromJSON SchemaFacet where
        toJSON SchemaFacet{..}
          = Core.object
              (Core.catMaybes
                 [("FacetName" Core..=) Core.<$> facetName,
                  ("SchemaArn" Core..=) Core.<$> schemaArn])

instance Core.FromJSON SchemaFacet where
        parseJSON
          = Core.withObject "SchemaFacet" Core.$
              \ x ->
                SchemaFacet' Core.<$>
                  (x Core..:? "FacetName") Core.<*> x Core..:? "SchemaArn"
