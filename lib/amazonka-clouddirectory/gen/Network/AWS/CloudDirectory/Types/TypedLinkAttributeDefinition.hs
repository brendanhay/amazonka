{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
  ( TypedLinkAttributeDefinition (..)
  -- * Smart constructor
  , mkTypedLinkAttributeDefinition
  -- * Lenses
  , tladName
  , tladType
  , tladRequiredBehavior
  , tladDefaultValue
  , tladIsImmutable
  , tladRules
  ) where

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
  { name :: Types.Name
    -- ^ The unique name of the typed link attribute.
  , type' :: Types.FacetAttributeType
    -- ^ The type of the attribute.
  , requiredBehavior :: Types.RequiredAttributeBehavior
    -- ^ The required behavior of the @TypedLinkAttributeDefinition@ .
  , defaultValue :: Core.Maybe Types.TypedAttributeValue
    -- ^ The default value of the attribute (if configured).
  , isImmutable :: Core.Maybe Core.Bool
    -- ^ Whether the attribute is mutable or not.
  , rules :: Core.Maybe (Core.HashMap Types.RuleKey Types.Rule)
    -- ^ Validation rules that are attached to the attribute definition.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TypedLinkAttributeDefinition' value with any optional fields omitted.
mkTypedLinkAttributeDefinition
    :: Types.Name -- ^ 'name'
    -> Types.FacetAttributeType -- ^ 'type\''
    -> Types.RequiredAttributeBehavior -- ^ 'requiredBehavior'
    -> TypedLinkAttributeDefinition
mkTypedLinkAttributeDefinition name type' requiredBehavior
  = TypedLinkAttributeDefinition'{name, type', requiredBehavior,
                                  defaultValue = Core.Nothing, isImmutable = Core.Nothing,
                                  rules = Core.Nothing}

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladName :: Lens.Lens' TypedLinkAttributeDefinition Types.Name
tladName = Lens.field @"name"
{-# INLINEABLE tladName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladType :: Lens.Lens' TypedLinkAttributeDefinition Types.FacetAttributeType
tladType = Lens.field @"type'"
{-# INLINEABLE tladType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRequiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition Types.RequiredAttributeBehavior
tladRequiredBehavior = Lens.field @"requiredBehavior"
{-# INLINEABLE tladRequiredBehavior #-}
{-# DEPRECATED requiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead"  #-}

-- | The default value of the attribute (if configured).
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladDefaultValue :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe Types.TypedAttributeValue)
tladDefaultValue = Lens.field @"defaultValue"
{-# INLINEABLE tladDefaultValue #-}
{-# DEPRECATED defaultValue "Use generic-lens or generic-optics with 'defaultValue' instead"  #-}

-- | Whether the attribute is mutable or not.
--
-- /Note:/ Consider using 'isImmutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladIsImmutable :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe Core.Bool)
tladIsImmutable = Lens.field @"isImmutable"
{-# INLINEABLE tladIsImmutable #-}
{-# DEPRECATED isImmutable "Use generic-lens or generic-optics with 'isImmutable' instead"  #-}

-- | Validation rules that are attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRules :: Lens.Lens' TypedLinkAttributeDefinition (Core.Maybe (Core.HashMap Types.RuleKey Types.Rule))
tladRules = Lens.field @"rules"
{-# INLINEABLE tladRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.FromJSON TypedLinkAttributeDefinition where
        toJSON TypedLinkAttributeDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name), Core.Just ("Type" Core..= type'),
                  Core.Just ("RequiredBehavior" Core..= requiredBehavior),
                  ("DefaultValue" Core..=) Core.<$> defaultValue,
                  ("IsImmutable" Core..=) Core.<$> isImmutable,
                  ("Rules" Core..=) Core.<$> rules])

instance Core.FromJSON TypedLinkAttributeDefinition where
        parseJSON
          = Core.withObject "TypedLinkAttributeDefinition" Core.$
              \ x ->
                TypedLinkAttributeDefinition' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Type" Core.<*>
                    x Core..: "RequiredBehavior"
                    Core.<*> x Core..:? "DefaultValue"
                    Core.<*> x Core..:? "IsImmutable"
                    Core.<*> x Core..:? "Rules"
