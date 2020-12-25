{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
  ( TypedLinkAttributeDefinition (..),

    -- * Smart constructor
    mkTypedLinkAttributeDefinition,

    -- * Lenses
    tladName,
    tladType,
    tladRequiredBehavior,
    tladDefaultValue,
    tladIsImmutable,
    tladRules,
  )
where

import qualified Network.AWS.CloudDirectory.Types.FacetAttributeType as Types
import qualified Network.AWS.CloudDirectory.Types.Name as Types
import qualified Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior as Types
import qualified Network.AWS.CloudDirectory.Types.Rule as Types
import qualified Network.AWS.CloudDirectory.Types.RuleKey as Types
import qualified Network.AWS.CloudDirectory.Types.TypedAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A typed link attribute definition.
--
-- /See:/ 'mkTypedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { -- | The unique name of the typed link attribute.
    name :: Types.Name,
    -- | The type of the attribute.
    type' :: Types.FacetAttributeType,
    -- | The required behavior of the @TypedLinkAttributeDefinition@ .
    requiredBehavior :: Types.RequiredAttributeBehavior,
    -- | The default value of the attribute (if configured).
    defaultValue :: Core.Maybe Types.TypedAttributeValue,
    -- | Whether the attribute is mutable or not.
    isImmutable :: Core.Maybe Core.Bool,
    -- | Validation rules that are attached to the attribute definition.
    rules :: Core.Maybe (Core.HashMap Types.RuleKey Types.Rule)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TypedLinkAttributeDefinition' value with any optional fields omitted.
mkTypedLinkAttributeDefinition ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.FacetAttributeType ->
  -- | 'requiredBehavior'
  Types.RequiredAttributeBehavior ->
  TypedLinkAttributeDefinition
mkTypedLinkAttributeDefinition name type' requiredBehavior =
  TypedLinkAttributeDefinition'
    { name,
      type',
      requiredBehavior,
      defaultValue = Core.Nothing,
      isImmutable = Core.Nothing,
      rules = Core.Nothing
    }

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladName :: Lens.Lens' TypedLinkAttributeDefinition Types.Name
tladName = Lens.field @"name"
{-# DEPRECATED tladName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladType :: Lens.Lens' TypedLinkAttributeDefinition Types.FacetAttributeType
tladType = Lens.field @"type'"
{-# DEPRECATED tladType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRequiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition Types.RequiredAttributeBehavior
tladRequiredBehavior = Lens.field @"requiredBehavior"
{-# DEPRECATED tladRequiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead." #-}

-- | The default value of the attribute (if configured).
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladDefaultValue :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe Types.TypedAttributeValue)
tladDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED tladDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether the attribute is mutable or not.
--
-- /Note:/ Consider using 'isImmutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladIsImmutable :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe Core.Bool)
tladIsImmutable = Lens.field @"isImmutable"
{-# DEPRECATED tladIsImmutable "Use generic-lens or generic-optics with 'isImmutable' instead." #-}

-- | Validation rules that are attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRules :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe (Core.HashMap Types.RuleKey Types.Rule))
tladRules = Lens.field @"rules"
{-# DEPRECATED tladRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.FromJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            Core.Just ("RequiredBehavior" Core..= requiredBehavior),
            ("DefaultValue" Core..=) Core.<$> defaultValue,
            ("IsImmutable" Core..=) Core.<$> isImmutable,
            ("Rules" Core..=) Core.<$> rules
          ]
      )

instance Core.FromJSON TypedLinkAttributeDefinition where
  parseJSON =
    Core.withObject "TypedLinkAttributeDefinition" Core.$
      \x ->
        TypedLinkAttributeDefinition'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..: "RequiredBehavior")
          Core.<*> (x Core..:? "DefaultValue")
          Core.<*> (x Core..:? "IsImmutable")
          Core.<*> (x Core..:? "Rules")
