{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Policy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Policy
  ( Policy (..),

    -- * Smart constructor
    mkPolicy,

    -- * Lenses
    pPolicyArn,
    pPolicyName,
  )
where

import qualified Network.AWS.IoT.Types.PolicyArn as Types
import qualified Network.AWS.IoT.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an AWS IoT policy.
--
-- /See:/ 'mkPolicy' smart constructor.
data Policy = Policy'
  { -- | The policy ARN.
    policyArn :: Core.Maybe Types.PolicyArn,
    -- | The policy name.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Policy' value with any optional fields omitted.
mkPolicy ::
  Policy
mkPolicy =
  Policy' {policyArn = Core.Nothing, policyName = Core.Nothing}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyArn :: Lens.Lens' Policy (Core.Maybe Types.PolicyArn)
pPolicyArn = Lens.field @"policyArn"
{-# DEPRECATED pPolicyArn "Use generic-lens or generic-optics with 'policyArn' instead." #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPolicyName :: Lens.Lens' Policy (Core.Maybe Types.PolicyName)
pPolicyName = Lens.field @"policyName"
{-# DEPRECATED pPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject "Policy" Core.$
      \x ->
        Policy'
          Core.<$> (x Core..:? "policyArn") Core.<*> (x Core..:? "policyName")
