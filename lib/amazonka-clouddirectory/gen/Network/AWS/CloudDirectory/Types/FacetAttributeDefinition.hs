{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
  ( FacetAttributeDefinition (..),

    -- * Smart constructor
    mkFacetAttributeDefinition,

    -- * Lenses
    fadType,
    fadDefaultValue,
    fadIsImmutable,
    fadRules,
  )
where

import qualified Network.AWS.CloudDirectory.Types.FacetAttributeType as Types
import qualified Network.AWS.CloudDirectory.Types.Rule as Types
import qualified Network.AWS.CloudDirectory.Types.RuleKey as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A facet attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /See:/ 'mkFacetAttributeDefinition' smart constructor.
data FacetAttributeDefinition = FacetAttributeDefinition'
  { -- | The type of the attribute.
    type' :: Types.FacetAttributeType,
    -- | The default value of the attribute (if configured).
    defaultValue :: Core.Maybe Types.TypedAttributeValue,
    -- | Whether the attribute is mutable or not.
    isImmutable :: Core.Maybe Core.Bool,
    -- | Validation rules attached to the attribute definition.
    rules :: Core.Maybe (Core.HashMap Types.RuleKey Types.Rule)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FacetAttributeDefinition' value with any optional fields omitted.
mkFacetAttributeDefinition ::
  -- | 'type\''
  Types.FacetAttributeType ->
  FacetAttributeDefinition
mkFacetAttributeDefinition type' =
  FacetAttributeDefinition'
    { type',
      defaultValue = Core.Nothing,
      isImmutable = Core.Nothing,
      rules = Core.Nothing
    }

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadType :: Lens.Lens' FacetAttributeDefinition Types.FacetAttributeType
fadType = Lens.field @"type'"
{-# DEPRECATED fadType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The default value of the attribute (if configured).
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadDefaultValue :: Lens.Lens' FacetAttributeDefinition (Core.Maybe Types.TypedAttributeValue)
fadDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED fadDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether the attribute is mutable or not.
--
-- /Note:/ Consider using 'isImmutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadIsImmutable :: Lens.Lens' FacetAttributeDefinition (Core.Maybe Core.Bool)
fadIsImmutable = Lens.field @"isImmutable"
{-# DEPRECATED fadIsImmutable "Use generic-lens or generic-optics with 'isImmutable' instead." #-}

-- | Validation rules attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadRules :: Lens.Lens' FacetAttributeDefinition (Core.Maybe (Core.HashMap Types.RuleKey Types.Rule))
fadRules = Lens.field @"rules"
{-# DEPRECATED fadRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.FromJSON FacetAttributeDefinition where
  toJSON FacetAttributeDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            ("DefaultValue" Core..=) Core.<$> defaultValue,
            ("IsImmutable" Core..=) Core.<$> isImmutable,
            ("Rules" Core..=) Core.<$> rules
          ]
      )

instance Core.FromJSON FacetAttributeDefinition where
  parseJSON =
    Core.withObject "FacetAttributeDefinition" Core.$
      \x ->
        FacetAttributeDefinition'
          Core.<$> (x Core..: "Type")
          Core.<*> (x Core..:? "DefaultValue")
          Core.<*> (x Core..:? "IsImmutable")
          Core.<*> (x Core..:? "Rules")
