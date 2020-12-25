{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Scope
  ( Scope (..),

    -- * Smart constructor
    mkScope,

    -- * Lenses
    sComplianceResourceId,
    sComplianceResourceTypes,
    sTagKey,
    sTagValue,
  )
where

import qualified Network.AWS.Config.Types.BaseResourceId as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit128 as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.
--
-- /See:/ 'mkScope' smart constructor.
data Scope = Scope'
  { -- | The ID of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
    complianceResourceId :: Core.Maybe Types.BaseResourceId,
    -- | The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
    complianceResourceTypes :: Core.Maybe [Types.StringWithCharLimit256],
    -- | The tag key that is applied to only those AWS resources that you want to trigger an evaluation for the rule.
    tagKey :: Core.Maybe Types.StringWithCharLimit128,
    -- | The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
    tagValue :: Core.Maybe Types.StringWithCharLimit256
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scope' value with any optional fields omitted.
mkScope ::
  Scope
mkScope =
  Scope'
    { complianceResourceId = Core.Nothing,
      complianceResourceTypes = Core.Nothing,
      tagKey = Core.Nothing,
      tagValue = Core.Nothing
    }

-- | The ID of the only AWS resource that you want to trigger an evaluation for the rule. If you specify a resource ID, you must specify one resource type for @ComplianceResourceTypes@ .
--
-- /Note:/ Consider using 'complianceResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sComplianceResourceId :: Lens.Lens' Scope (Core.Maybe Types.BaseResourceId)
sComplianceResourceId = Lens.field @"complianceResourceId"
{-# DEPRECATED sComplianceResourceId "Use generic-lens or generic-optics with 'complianceResourceId' instead." #-}

-- | The resource types of only those AWS resources that you want to trigger an evaluation for the rule. You can only specify one type if you also specify a resource ID for @ComplianceResourceId@ .
--
-- /Note:/ Consider using 'complianceResourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sComplianceResourceTypes :: Lens.Lens' Scope (Core.Maybe [Types.StringWithCharLimit256])
sComplianceResourceTypes = Lens.field @"complianceResourceTypes"
{-# DEPRECATED sComplianceResourceTypes "Use generic-lens or generic-optics with 'complianceResourceTypes' instead." #-}

-- | The tag key that is applied to only those AWS resources that you want to trigger an evaluation for the rule.
--
-- /Note:/ Consider using 'tagKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTagKey :: Lens.Lens' Scope (Core.Maybe Types.StringWithCharLimit128)
sTagKey = Lens.field @"tagKey"
{-# DEPRECATED sTagKey "Use generic-lens or generic-optics with 'tagKey' instead." #-}

-- | The tag value applied to only those AWS resources that you want to trigger an evaluation for the rule. If you specify a value for @TagValue@ , you must also specify a value for @TagKey@ .
--
-- /Note:/ Consider using 'tagValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTagValue :: Lens.Lens' Scope (Core.Maybe Types.StringWithCharLimit256)
sTagValue = Lens.field @"tagValue"
{-# DEPRECATED sTagValue "Use generic-lens or generic-optics with 'tagValue' instead." #-}

instance Core.FromJSON Scope where
  toJSON Scope {..} =
    Core.object
      ( Core.catMaybes
          [ ("ComplianceResourceId" Core..=) Core.<$> complianceResourceId,
            ("ComplianceResourceTypes" Core..=)
              Core.<$> complianceResourceTypes,
            ("TagKey" Core..=) Core.<$> tagKey,
            ("TagValue" Core..=) Core.<$> tagValue
          ]
      )

instance Core.FromJSON Scope where
  parseJSON =
    Core.withObject "Scope" Core.$
      \x ->
        Scope'
          Core.<$> (x Core..:? "ComplianceResourceId")
          Core.<*> (x Core..:? "ComplianceResourceTypes")
          Core.<*> (x Core..:? "TagKey")
          Core.<*> (x Core..:? "TagValue")
