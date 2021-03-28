{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.PolicyDescription
  ( PolicyDescription (..)
  -- * Smart constructor
  , mkPolicyDescription
  -- * Lenses
  , pdPolicyAttributeDescriptions
  , pdPolicyName
  , pdPolicyTypeName
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.PolicyAttributeDescription as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.ELB.Types.PolicyTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy.
--
-- /See:/ 'mkPolicyDescription' smart constructor.
data PolicyDescription = PolicyDescription'
  { policyAttributeDescriptions :: Core.Maybe [Types.PolicyAttributeDescription]
    -- ^ The policy attributes.
  , policyName :: Core.Maybe Types.PolicyName
    -- ^ The name of the policy.
  , policyTypeName :: Core.Maybe Types.PolicyTypeName
    -- ^ The name of the policy type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyDescription' value with any optional fields omitted.
mkPolicyDescription
    :: PolicyDescription
mkPolicyDescription
  = PolicyDescription'{policyAttributeDescriptions = Core.Nothing,
                       policyName = Core.Nothing, policyTypeName = Core.Nothing}

-- | The policy attributes.
--
-- /Note:/ Consider using 'policyAttributeDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyAttributeDescriptions :: Lens.Lens' PolicyDescription (Core.Maybe [Types.PolicyAttributeDescription])
pdPolicyAttributeDescriptions = Lens.field @"policyAttributeDescriptions"
{-# INLINEABLE pdPolicyAttributeDescriptions #-}
{-# DEPRECATED policyAttributeDescriptions "Use generic-lens or generic-optics with 'policyAttributeDescriptions' instead"  #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyName :: Lens.Lens' PolicyDescription (Core.Maybe Types.PolicyName)
pdPolicyName = Lens.field @"policyName"
{-# INLINEABLE pdPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The name of the policy type.
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPolicyTypeName :: Lens.Lens' PolicyDescription (Core.Maybe Types.PolicyTypeName)
pdPolicyTypeName = Lens.field @"policyTypeName"
{-# INLINEABLE pdPolicyTypeName #-}
{-# DEPRECATED policyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead"  #-}

instance Core.FromXML PolicyDescription where
        parseXML x
          = PolicyDescription' Core.<$>
              (x Core..@? "PolicyAttributeDescriptions" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "PolicyName"
                Core.<*> x Core..@? "PolicyTypeName"
