{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.PolicyTypeDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.PolicyTypeDescription
  ( PolicyTypeDescription (..)
  -- * Smart constructor
  , mkPolicyTypeDescription
  -- * Lenses
  , ptdDescription
  , ptdPolicyAttributeTypeDescriptions
  , ptdPolicyTypeName
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.Description as Types
import qualified Network.AWS.ELB.Types.PolicyAttributeTypeDescription as Types
import qualified Network.AWS.ELB.Types.PolicyTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a policy type.
--
-- /See:/ 'mkPolicyTypeDescription' smart constructor.
data PolicyTypeDescription = PolicyTypeDescription'
  { description :: Core.Maybe Types.Description
    -- ^ A description of the policy type.
  , policyAttributeTypeDescriptions :: Core.Maybe [Types.PolicyAttributeTypeDescription]
    -- ^ The description of the policy attributes associated with the policies defined by Elastic Load Balancing.
  , policyTypeName :: Core.Maybe Types.PolicyTypeName
    -- ^ The name of the policy type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyTypeDescription' value with any optional fields omitted.
mkPolicyTypeDescription
    :: PolicyTypeDescription
mkPolicyTypeDescription
  = PolicyTypeDescription'{description = Core.Nothing,
                           policyAttributeTypeDescriptions = Core.Nothing,
                           policyTypeName = Core.Nothing}

-- | A description of the policy type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdDescription :: Lens.Lens' PolicyTypeDescription (Core.Maybe Types.Description)
ptdDescription = Lens.field @"description"
{-# INLINEABLE ptdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The description of the policy attributes associated with the policies defined by Elastic Load Balancing.
--
-- /Note:/ Consider using 'policyAttributeTypeDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdPolicyAttributeTypeDescriptions :: Lens.Lens' PolicyTypeDescription (Core.Maybe [Types.PolicyAttributeTypeDescription])
ptdPolicyAttributeTypeDescriptions = Lens.field @"policyAttributeTypeDescriptions"
{-# INLINEABLE ptdPolicyAttributeTypeDescriptions #-}
{-# DEPRECATED policyAttributeTypeDescriptions "Use generic-lens or generic-optics with 'policyAttributeTypeDescriptions' instead"  #-}

-- | The name of the policy type.
--
-- /Note:/ Consider using 'policyTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptdPolicyTypeName :: Lens.Lens' PolicyTypeDescription (Core.Maybe Types.PolicyTypeName)
ptdPolicyTypeName = Lens.field @"policyTypeName"
{-# INLINEABLE ptdPolicyTypeName #-}
{-# DEPRECATED policyTypeName "Use generic-lens or generic-optics with 'policyTypeName' instead"  #-}

instance Core.FromXML PolicyTypeDescription where
        parseXML x
          = PolicyTypeDescription' Core.<$>
              (x Core..@? "Description") Core.<*>
                x Core..@? "PolicyAttributeTypeDescriptions" Core..<@>
                  Core.parseXMLList "member"
                Core.<*> x Core..@? "PolicyTypeName"
