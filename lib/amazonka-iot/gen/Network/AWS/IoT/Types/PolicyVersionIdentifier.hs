{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PolicyVersionIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PolicyVersionIdentifier
  ( PolicyVersionIdentifier (..),

    -- * Smart constructor
    mkPolicyVersionIdentifier,

    -- * Lenses
    pviPolicyName,
    pviPolicyVersionId,
  )
where

import qualified Network.AWS.IoT.Types.PolicyName as Types
import qualified Network.AWS.IoT.Types.PolicyVersionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the version of the policy associated with the resource.
--
-- /See:/ 'mkPolicyVersionIdentifier' smart constructor.
data PolicyVersionIdentifier = PolicyVersionIdentifier'
  { -- | The name of the policy.
    policyName :: Core.Maybe Types.PolicyName,
    -- | The ID of the version of the policy associated with the resource.
    policyVersionId :: Core.Maybe Types.PolicyVersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyVersionIdentifier' value with any optional fields omitted.
mkPolicyVersionIdentifier ::
  PolicyVersionIdentifier
mkPolicyVersionIdentifier =
  PolicyVersionIdentifier'
    { policyName = Core.Nothing,
      policyVersionId = Core.Nothing
    }

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pviPolicyName :: Lens.Lens' PolicyVersionIdentifier (Core.Maybe Types.PolicyName)
pviPolicyName = Lens.field @"policyName"
{-# DEPRECATED pviPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The ID of the version of the policy associated with the resource.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pviPolicyVersionId :: Lens.Lens' PolicyVersionIdentifier (Core.Maybe Types.PolicyVersionId)
pviPolicyVersionId = Lens.field @"policyVersionId"
{-# DEPRECATED pviPolicyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead." #-}

instance Core.FromJSON PolicyVersionIdentifier where
  toJSON PolicyVersionIdentifier {..} =
    Core.object
      ( Core.catMaybes
          [ ("policyName" Core..=) Core.<$> policyName,
            ("policyVersionId" Core..=) Core.<$> policyVersionId
          ]
      )

instance Core.FromJSON PolicyVersionIdentifier where
  parseJSON =
    Core.withObject "PolicyVersionIdentifier" Core.$
      \x ->
        PolicyVersionIdentifier'
          Core.<$> (x Core..:? "policyName") Core.<*> (x Core..:? "policyVersionId")
