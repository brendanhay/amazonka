{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTypeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.PolicyTypeSummary
  ( PolicyTypeSummary (..)
  -- * Smart constructor
  , mkPolicyTypeSummary
  -- * Lenses
  , ptsStatus
  , ptsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.PolicyType as Types
import qualified Network.AWS.Organizations.Types.PolicyTypeStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Contains information about a policy type and its status in the associated root.
--
-- /See:/ 'mkPolicyTypeSummary' smart constructor.
data PolicyTypeSummary = PolicyTypeSummary'
  { status :: Core.Maybe Types.PolicyTypeStatus
    -- ^ The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
  , type' :: Core.Maybe Types.PolicyType
    -- ^ The name of the policy type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyTypeSummary' value with any optional fields omitted.
mkPolicyTypeSummary
    :: PolicyTypeSummary
mkPolicyTypeSummary
  = PolicyTypeSummary'{status = Core.Nothing, type' = Core.Nothing}

-- | The status of the policy type as it relates to the associated root. To attach a policy of the specified type to a root or to an OU or account in that root, it must be available in the organization and enabled for that root.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsStatus :: Lens.Lens' PolicyTypeSummary (Core.Maybe Types.PolicyTypeStatus)
ptsStatus = Lens.field @"status"
{-# INLINEABLE ptsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The name of the policy type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsType :: Lens.Lens' PolicyTypeSummary (Core.Maybe Types.PolicyType)
ptsType = Lens.field @"type'"
{-# INLINEABLE ptsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON PolicyTypeSummary where
        parseJSON
          = Core.withObject "PolicyTypeSummary" Core.$
              \ x ->
                PolicyTypeSummary' Core.<$>
                  (x Core..:? "Status") Core.<*> x Core..:? "Type"
