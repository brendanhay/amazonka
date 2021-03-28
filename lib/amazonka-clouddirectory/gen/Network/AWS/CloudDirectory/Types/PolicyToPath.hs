{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PolicyToPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.PolicyToPath
  ( PolicyToPath (..)
  -- * Smart constructor
  , mkPolicyToPath
  -- * Lenses
  , ptpPath
  , ptpPolicies
  ) where

import qualified Network.AWS.CloudDirectory.Types.PathString as Types
import qualified Network.AWS.CloudDirectory.Types.PolicyAttachment as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used when a regular object exists in a 'Directory' and you want to find all of the policies that are associated with that object and the parent to that object.
--
-- /See:/ 'mkPolicyToPath' smart constructor.
data PolicyToPath = PolicyToPath'
  { path :: Core.Maybe Types.PathString
    -- ^ The path that is referenced from the root.
  , policies :: Core.Maybe [Types.PolicyAttachment]
    -- ^ List of policy objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PolicyToPath' value with any optional fields omitted.
mkPolicyToPath
    :: PolicyToPath
mkPolicyToPath
  = PolicyToPath'{path = Core.Nothing, policies = Core.Nothing}

-- | The path that is referenced from the root.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpPath :: Lens.Lens' PolicyToPath (Core.Maybe Types.PathString)
ptpPath = Lens.field @"path"
{-# INLINEABLE ptpPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | List of policy objects.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptpPolicies :: Lens.Lens' PolicyToPath (Core.Maybe [Types.PolicyAttachment])
ptpPolicies = Lens.field @"policies"
{-# INLINEABLE ptpPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

instance Core.FromJSON PolicyToPath where
        parseJSON
          = Core.withObject "PolicyToPath" Core.$
              \ x ->
                PolicyToPath' Core.<$>
                  (x Core..:? "Path") Core.<*> x Core..:? "Policies"
