{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.EffectivePolicy
  ( EffectivePolicy (..)
  -- * Smart constructor
  , mkEffectivePolicy
  -- * Lenses
  , epPolicyArn
  , epPolicyDocument
  , epPolicyName
  ) where

import qualified Network.AWS.IoT.Types.PolicyArn as Types
import qualified Network.AWS.IoT.Types.PolicyDocument as Types
import qualified Network.AWS.IoT.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The policy that has the effect on the authorization results.
--
-- /See:/ 'mkEffectivePolicy' smart constructor.
data EffectivePolicy = EffectivePolicy'
  { policyArn :: Core.Maybe Types.PolicyArn
    -- ^ The policy ARN.
  , policyDocument :: Core.Maybe Types.PolicyDocument
    -- ^ The IAM policy document.
  , policyName :: Core.Maybe Types.PolicyName
    -- ^ The policy name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EffectivePolicy' value with any optional fields omitted.
mkEffectivePolicy
    :: EffectivePolicy
mkEffectivePolicy
  = EffectivePolicy'{policyArn = Core.Nothing,
                     policyDocument = Core.Nothing, policyName = Core.Nothing}

-- | The policy ARN.
--
-- /Note:/ Consider using 'policyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyArn :: Lens.Lens' EffectivePolicy (Core.Maybe Types.PolicyArn)
epPolicyArn = Lens.field @"policyArn"
{-# INLINEABLE epPolicyArn #-}
{-# DEPRECATED policyArn "Use generic-lens or generic-optics with 'policyArn' instead"  #-}

-- | The IAM policy document.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyDocument :: Lens.Lens' EffectivePolicy (Core.Maybe Types.PolicyDocument)
epPolicyDocument = Lens.field @"policyDocument"
{-# INLINEABLE epPolicyDocument #-}
{-# DEPRECATED policyDocument "Use generic-lens or generic-optics with 'policyDocument' instead"  #-}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epPolicyName :: Lens.Lens' EffectivePolicy (Core.Maybe Types.PolicyName)
epPolicyName = Lens.field @"policyName"
{-# INLINEABLE epPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

instance Core.FromJSON EffectivePolicy where
        parseJSON
          = Core.withObject "EffectivePolicy" Core.$
              \ x ->
                EffectivePolicy' Core.<$>
                  (x Core..:? "policyArn") Core.<*> x Core..:? "policyDocument"
                    Core.<*> x Core..:? "policyName"
