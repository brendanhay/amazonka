{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.SsmActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SsmActionDefinition
  ( SsmActionDefinition (..),

    -- * Smart constructor
    mkSsmActionDefinition,

    -- * Lenses
    sadActionSubType,
    sadRegion,
    sadInstanceIds,
  )
where

import qualified Network.AWS.Budgets.Types.ActionSubType as Types
import qualified Network.AWS.Budgets.Types.InstanceId as Types
import qualified Network.AWS.Budgets.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS Systems Manager (SSM) action definition details.
--
-- /See:/ 'mkSsmActionDefinition' smart constructor.
data SsmActionDefinition = SsmActionDefinition'
  { -- | The action subType.
    actionSubType :: Types.ActionSubType,
    -- | The Region to run the SSM document.
    region :: Types.Region,
    -- | The EC2 and RDS instance IDs.
    instanceIds :: Core.NonEmpty Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SsmActionDefinition' value with any optional fields omitted.
mkSsmActionDefinition ::
  -- | 'actionSubType'
  Types.ActionSubType ->
  -- | 'region'
  Types.Region ->
  -- | 'instanceIds'
  Core.NonEmpty Types.InstanceId ->
  SsmActionDefinition
mkSsmActionDefinition actionSubType region instanceIds =
  SsmActionDefinition' {actionSubType, region, instanceIds}

-- | The action subType.
--
-- /Note:/ Consider using 'actionSubType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadActionSubType :: Lens.Lens' SsmActionDefinition Types.ActionSubType
sadActionSubType = Lens.field @"actionSubType"
{-# DEPRECATED sadActionSubType "Use generic-lens or generic-optics with 'actionSubType' instead." #-}

-- | The Region to run the SSM document.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadRegion :: Lens.Lens' SsmActionDefinition Types.Region
sadRegion = Lens.field @"region"
{-# DEPRECATED sadRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The EC2 and RDS instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadInstanceIds :: Lens.Lens' SsmActionDefinition (Core.NonEmpty Types.InstanceId)
sadInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED sadInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

instance Core.FromJSON SsmActionDefinition where
  toJSON SsmActionDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ActionSubType" Core..= actionSubType),
            Core.Just ("Region" Core..= region),
            Core.Just ("InstanceIds" Core..= instanceIds)
          ]
      )

instance Core.FromJSON SsmActionDefinition where
  parseJSON =
    Core.withObject "SsmActionDefinition" Core.$
      \x ->
        SsmActionDefinition'
          Core.<$> (x Core..: "ActionSubType")
          Core.<*> (x Core..: "Region")
          Core.<*> (x Core..: "InstanceIds")
