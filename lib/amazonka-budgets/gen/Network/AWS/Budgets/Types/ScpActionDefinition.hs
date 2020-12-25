{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ScpActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ScpActionDefinition
  ( ScpActionDefinition (..),

    -- * Smart constructor
    mkScpActionDefinition,

    -- * Lenses
    sadPolicyId,
    sadTargetIds,
  )
where

import qualified Network.AWS.Budgets.Types.PolicyId as Types
import qualified Network.AWS.Budgets.Types.TargetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The service control policies (SCP) action definition details.
--
-- /See:/ 'mkScpActionDefinition' smart constructor.
data ScpActionDefinition = ScpActionDefinition'
  { -- | The policy ID attached.
    policyId :: Types.PolicyId,
    -- | A list of target IDs.
    targetIds :: Core.NonEmpty Types.TargetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScpActionDefinition' value with any optional fields omitted.
mkScpActionDefinition ::
  -- | 'policyId'
  Types.PolicyId ->
  -- | 'targetIds'
  Core.NonEmpty Types.TargetId ->
  ScpActionDefinition
mkScpActionDefinition policyId targetIds =
  ScpActionDefinition' {policyId, targetIds}

-- | The policy ID attached.
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadPolicyId :: Lens.Lens' ScpActionDefinition Types.PolicyId
sadPolicyId = Lens.field @"policyId"
{-# DEPRECATED sadPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | A list of target IDs.
--
-- /Note:/ Consider using 'targetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadTargetIds :: Lens.Lens' ScpActionDefinition (Core.NonEmpty Types.TargetId)
sadTargetIds = Lens.field @"targetIds"
{-# DEPRECATED sadTargetIds "Use generic-lens or generic-optics with 'targetIds' instead." #-}

instance Core.FromJSON ScpActionDefinition where
  toJSON ScpActionDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            Core.Just ("TargetIds" Core..= targetIds)
          ]
      )

instance Core.FromJSON ScpActionDefinition where
  parseJSON =
    Core.withObject "ScpActionDefinition" Core.$
      \x ->
        ScpActionDefinition'
          Core.<$> (x Core..: "PolicyId") Core.<*> (x Core..: "TargetIds")
