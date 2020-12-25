{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyDetail
  ( PolicyDetail (..),

    -- * Smart constructor
    mkPolicyDetail,

    -- * Lenses
    pdPolicyDocument,
    pdPolicyName,
  )
where

import qualified Network.AWS.IAM.Types.PolicyDocument as Types
import qualified Network.AWS.IAM.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM policy, including the policy document.
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- /See:/ 'mkPolicyDetail' smart constructor.
data PolicyDetail = PolicyDetail'
  { -- | The policy document.
    policyDocument :: Core.Maybe Types.PolicyDocument,
    -- | The name of the policy.
    policyName :: Core.Maybe Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyDetail' value with any optional fields omitted.
mkPolicyDetail ::
  PolicyDetail
mkPolicyDetail =
  PolicyDetail'
    { policyDocument = Core.Nothing,
      policyName = Core.Nothing
    }

-- | The policy document.
--
-- /Note:/ Consider using 'policyDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyDocument :: Lens.Lens' PolicyDetail (Core.Maybe Types.PolicyDocument)
pdPolicyDocument = Lens.field @"policyDocument"
{-# DEPRECATED pdPolicyDocument "Use generic-lens or generic-optics with 'policyDocument' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyName :: Lens.Lens' PolicyDetail (Core.Maybe Types.PolicyName)
pdPolicyName = Lens.field @"policyName"
{-# DEPRECATED pdPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.FromXML PolicyDetail where
  parseXML x =
    PolicyDetail'
      Core.<$> (x Core..@? "PolicyDocument") Core.<*> (x Core..@? "PolicyName")
