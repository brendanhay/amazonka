{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKey
  ( AttributeKey (..),

    -- * Smart constructor
    mkAttributeKey,

    -- * Lenses
    akSchemaArn,
    akFacetName,
    akName,
  )
where

import qualified Network.AWS.CloudDirectory.Types.Arn as Types
import qualified Network.AWS.CloudDirectory.Types.FacetName as Types
import qualified Network.AWS.CloudDirectory.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A unique identifier for an attribute.
--
-- /See:/ 'mkAttributeKey' smart constructor.
data AttributeKey = AttributeKey'
  { -- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
    schemaArn :: Types.Arn,
    -- | The name of the facet that the attribute exists within.
    facetName :: Types.FacetName,
    -- | The name of the attribute.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttributeKey' value with any optional fields omitted.
mkAttributeKey ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'facetName'
  Types.FacetName ->
  -- | 'name'
  Types.Name ->
  AttributeKey
mkAttributeKey schemaArn facetName name =
  AttributeKey' {schemaArn, facetName, name}

-- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akSchemaArn :: Lens.Lens' AttributeKey Types.Arn
akSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED akSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the facet that the attribute exists within.
--
-- /Note:/ Consider using 'facetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akFacetName :: Lens.Lens' AttributeKey Types.FacetName
akFacetName = Lens.field @"facetName"
{-# DEPRECATED akFacetName "Use generic-lens or generic-optics with 'facetName' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akName :: Lens.Lens' AttributeKey Types.Name
akName = Lens.field @"name"
{-# DEPRECATED akName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON AttributeKey where
  toJSON AttributeKey {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaArn" Core..= schemaArn),
            Core.Just ("FacetName" Core..= facetName),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.FromJSON AttributeKey where
  parseJSON =
    Core.withObject "AttributeKey" Core.$
      \x ->
        AttributeKey'
          Core.<$> (x Core..: "SchemaArn")
          Core.<*> (x Core..: "FacetName")
          Core.<*> (x Core..: "Name")
