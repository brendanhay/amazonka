{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyAttributeTypeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttributeTypeDescription
  ( PolicyAttributeTypeDescription (..),

    -- * Smart constructor
    mkPolicyAttributeTypeDescription,

    -- * Lenses
    patdAttributeName,
    patdAttributeType,
    patdCardinality,
    patdDefaultValue,
    patdDescription,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.AttributeName as Types
import qualified Network.AWS.ELB.Types.AttributeType as Types
import qualified Network.AWS.ELB.Types.Cardinality as Types
import qualified Network.AWS.ELB.Types.DefaultValue as Types
import qualified Network.AWS.ELB.Types.Description as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy attribute type.
--
-- /See:/ 'mkPolicyAttributeTypeDescription' smart constructor.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription'
  { -- | The name of the attribute.
    attributeName :: Core.Maybe Types.AttributeName,
    -- | The type of the attribute. For example, @Boolean@ or @Integer@ .
    attributeType :: Core.Maybe Types.AttributeType,
    -- | The cardinality of the attribute.
    --
    -- Valid values:
    --
    --     * ONE(1) : Single value required
    --
    --
    --     * ZERO_OR_ONE(0..1) : Up to one value is allowed
    --
    --
    --     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
    --
    --
    --     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
    cardinality :: Core.Maybe Types.Cardinality,
    -- | The default value of the attribute, if applicable.
    defaultValue :: Core.Maybe Types.DefaultValue,
    -- | A description of the attribute.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyAttributeTypeDescription' value with any optional fields omitted.
mkPolicyAttributeTypeDescription ::
  PolicyAttributeTypeDescription
mkPolicyAttributeTypeDescription =
  PolicyAttributeTypeDescription'
    { attributeName = Core.Nothing,
      attributeType = Core.Nothing,
      cardinality = Core.Nothing,
      defaultValue = Core.Nothing,
      description = Core.Nothing
    }

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdAttributeName :: Lens.Lens' PolicyAttributeTypeDescription (Core.Maybe Types.AttributeName)
patdAttributeName = Lens.field @"attributeName"
{-# DEPRECATED patdAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The type of the attribute. For example, @Boolean@ or @Integer@ .
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdAttributeType :: Lens.Lens' PolicyAttributeTypeDescription (Core.Maybe Types.AttributeType)
patdAttributeType = Lens.field @"attributeType"
{-# DEPRECATED patdAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The cardinality of the attribute.
--
-- Valid values:
--
--     * ONE(1) : Single value required
--
--
--     * ZERO_OR_ONE(0..1) : Up to one value is allowed
--
--
--     * ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
--
--
--     * ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
--
--
--
-- /Note:/ Consider using 'cardinality' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdCardinality :: Lens.Lens' PolicyAttributeTypeDescription (Core.Maybe Types.Cardinality)
patdCardinality = Lens.field @"cardinality"
{-# DEPRECATED patdCardinality "Use generic-lens or generic-optics with 'cardinality' instead." #-}

-- | The default value of the attribute, if applicable.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdDefaultValue :: Lens.Lens' PolicyAttributeTypeDescription (Core.Maybe Types.DefaultValue)
patdDefaultValue = Lens.field @"defaultValue"
{-# DEPRECATED patdDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | A description of the attribute.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
patdDescription :: Lens.Lens' PolicyAttributeTypeDescription (Core.Maybe Types.Description)
patdDescription = Lens.field @"description"
{-# DEPRECATED patdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromXML PolicyAttributeTypeDescription where
  parseXML x =
    PolicyAttributeTypeDescription'
      Core.<$> (x Core..@? "AttributeName")
      Core.<*> (x Core..@? "AttributeType")
      Core.<*> (x Core..@? "Cardinality")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "Description")
