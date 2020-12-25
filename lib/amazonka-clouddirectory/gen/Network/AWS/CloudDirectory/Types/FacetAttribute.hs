{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttribute
  ( FacetAttribute (..),

    -- * Smart constructor
    mkFacetAttribute,

    -- * Lenses
    faName,
    faAttributeDefinition,
    faAttributeReference,
    faRequiredBehavior,
  )
where

import qualified Network.AWS.CloudDirectory.Types.FacetAttributeDefinition as Types
import qualified Network.AWS.CloudDirectory.Types.FacetAttributeReference as Types
import qualified Network.AWS.CloudDirectory.Types.Name as Types
import qualified Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An attribute that is associated with the 'Facet' .
--
-- /See:/ 'mkFacetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
  { -- | The name of the facet attribute.
    name :: Types.Name,
    -- | A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
    attributeDefinition :: Core.Maybe Types.FacetAttributeDefinition,
    -- | An attribute reference that is associated with the attribute. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
    attributeReference :: Core.Maybe Types.FacetAttributeReference,
    -- | The required behavior of the @FacetAttribute@ .
    requiredBehavior :: Core.Maybe Types.RequiredAttributeBehavior
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FacetAttribute' value with any optional fields omitted.
mkFacetAttribute ::
  -- | 'name'
  Types.Name ->
  FacetAttribute
mkFacetAttribute name =
  FacetAttribute'
    { name,
      attributeDefinition = Core.Nothing,
      attributeReference = Core.Nothing,
      requiredBehavior = Core.Nothing
    }

-- | The name of the facet attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FacetAttribute Types.Name
faName = Lens.field @"name"
{-# DEPRECATED faName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'attributeDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faAttributeDefinition :: Lens.Lens' FacetAttribute (Core.Maybe Types.FacetAttributeDefinition)
faAttributeDefinition = Lens.field @"attributeDefinition"
{-# DEPRECATED faAttributeDefinition "Use generic-lens or generic-optics with 'attributeDefinition' instead." #-}

-- | An attribute reference that is associated with the attribute. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'attributeReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faAttributeReference :: Lens.Lens' FacetAttribute (Core.Maybe Types.FacetAttributeReference)
faAttributeReference = Lens.field @"attributeReference"
{-# DEPRECATED faAttributeReference "Use generic-lens or generic-optics with 'attributeReference' instead." #-}

-- | The required behavior of the @FacetAttribute@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faRequiredBehavior :: Lens.Lens' FacetAttribute (Core.Maybe Types.RequiredAttributeBehavior)
faRequiredBehavior = Lens.field @"requiredBehavior"
{-# DEPRECATED faRequiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead." #-}

instance Core.FromJSON FacetAttribute where
  toJSON FacetAttribute {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("AttributeDefinition" Core..=) Core.<$> attributeDefinition,
            ("AttributeReference" Core..=) Core.<$> attributeReference,
            ("RequiredBehavior" Core..=) Core.<$> requiredBehavior
          ]
      )

instance Core.FromJSON FacetAttribute where
  parseJSON =
    Core.withObject "FacetAttribute" Core.$
      \x ->
        FacetAttribute'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "AttributeDefinition")
          Core.<*> (x Core..:? "AttributeReference")
          Core.<*> (x Core..:? "RequiredBehavior")
