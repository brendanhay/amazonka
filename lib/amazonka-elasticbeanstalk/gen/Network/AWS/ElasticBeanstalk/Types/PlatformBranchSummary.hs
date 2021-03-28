{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
  ( PlatformBranchSummary (..)
  -- * Smart constructor
  , mkPlatformBranchSummary
  -- * Lenses
  , pbsBranchName
  , pbsBranchOrder
  , pbsLifecycleState
  , pbsPlatformName
  , pbsSupportedTierList
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.BranchName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.LifecycleState as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SupportedTier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about a platform branch.
--
-- /See:/ 'mkPlatformBranchSummary' smart constructor.
data PlatformBranchSummary = PlatformBranchSummary'
  { branchName :: Core.Maybe Types.BranchName
    -- ^ The name of the platform branch.
  , branchOrder :: Core.Maybe Core.Int
    -- ^ An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within the platform.
  , lifecycleState :: Core.Maybe Types.LifecycleState
    -- ^ The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@ 
  , platformName :: Core.Maybe Types.PlatformName
    -- ^ The name of the platform to which this platform branch belongs.
  , supportedTierList :: Core.Maybe [Types.SupportedTier]
    -- ^ The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformBranchSummary' value with any optional fields omitted.
mkPlatformBranchSummary
    :: PlatformBranchSummary
mkPlatformBranchSummary
  = PlatformBranchSummary'{branchName = Core.Nothing,
                           branchOrder = Core.Nothing, lifecycleState = Core.Nothing,
                           platformName = Core.Nothing, supportedTierList = Core.Nothing}

-- | The name of the platform branch.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsBranchName :: Lens.Lens' PlatformBranchSummary (Core.Maybe Types.BranchName)
pbsBranchName = Lens.field @"branchName"
{-# INLINEABLE pbsBranchName #-}
{-# DEPRECATED branchName "Use generic-lens or generic-optics with 'branchName' instead"  #-}

-- | An ordinal number that designates the order in which platform branches have been added to a platform. This can be helpful, for example, if your code calls the @ListPlatformBranches@ action and then displays a list of platform branches.
--
-- A larger @BranchOrder@ value designates a newer platform branch within the platform.
--
-- /Note:/ Consider using 'branchOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsBranchOrder :: Lens.Lens' PlatformBranchSummary (Core.Maybe Core.Int)
pbsBranchOrder = Lens.field @"branchOrder"
{-# INLINEABLE pbsBranchOrder #-}
{-# DEPRECATED branchOrder "Use generic-lens or generic-optics with 'branchOrder' instead"  #-}

-- | The support life cycle state of the platform branch.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@ 
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsLifecycleState :: Lens.Lens' PlatformBranchSummary (Core.Maybe Types.LifecycleState)
pbsLifecycleState = Lens.field @"lifecycleState"
{-# INLINEABLE pbsLifecycleState #-}
{-# DEPRECATED lifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead"  #-}

-- | The name of the platform to which this platform branch belongs.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsPlatformName :: Lens.Lens' PlatformBranchSummary (Core.Maybe Types.PlatformName)
pbsPlatformName = Lens.field @"platformName"
{-# INLINEABLE pbsPlatformName #-}
{-# DEPRECATED platformName "Use generic-lens or generic-optics with 'platformName' instead"  #-}

-- | The environment tiers that platform versions in this branch support.
--
-- Possible values: @WebServer/Standard@ | @Worker/SQS/HTTP@ 
--
-- /Note:/ Consider using 'supportedTierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbsSupportedTierList :: Lens.Lens' PlatformBranchSummary (Core.Maybe [Types.SupportedTier])
pbsSupportedTierList = Lens.field @"supportedTierList"
{-# INLINEABLE pbsSupportedTierList #-}
{-# DEPRECATED supportedTierList "Use generic-lens or generic-optics with 'supportedTierList' instead"  #-}

instance Core.FromXML PlatformBranchSummary where
        parseXML x
          = PlatformBranchSummary' Core.<$>
              (x Core..@? "BranchName") Core.<*> x Core..@? "BranchOrder"
                Core.<*> x Core..@? "LifecycleState"
                Core.<*> x Core..@? "PlatformName"
                Core.<*>
                x Core..@? "SupportedTierList" Core..<@> Core.parseXMLList "member"
