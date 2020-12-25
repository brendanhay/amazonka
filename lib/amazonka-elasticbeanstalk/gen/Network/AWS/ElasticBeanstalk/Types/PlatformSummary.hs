{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformSummary
  ( PlatformSummary (..),

    -- * Smart constructor
    mkPlatformSummary,

    -- * Lenses
    psOperatingSystemName,
    psOperatingSystemVersion,
    psPlatformArn,
    psPlatformBranchLifecycleState,
    psPlatformBranchName,
    psPlatformCategory,
    psPlatformLifecycleState,
    psPlatformOwner,
    psPlatformStatus,
    psPlatformVersion,
    psSupportedAddonList,
    psSupportedTierList,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.BranchName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OperatingSystemName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.OperatingSystemVersion as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformArn as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformBranchLifecycleState as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformCategory as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformLifecycleState as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformOwner as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.PlatformVersion as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SupportedAddon as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SupportedTier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary information about a platform version.
--
-- /See:/ 'mkPlatformSummary' smart constructor.
data PlatformSummary = PlatformSummary'
  { -- | The operating system used by the platform version.
    operatingSystemName :: Core.Maybe Types.OperatingSystemName,
    -- | The version of the operating system used by the platform version.
    operatingSystemVersion :: Core.Maybe Types.OperatingSystemVersion,
    -- | The ARN of the platform version.
    platformArn :: Core.Maybe Types.PlatformArn,
    -- | The state of the platform version's branch in its lifecycle.
    --
    -- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
    platformBranchLifecycleState :: Core.Maybe Types.PlatformBranchLifecycleState,
    -- | The platform branch to which the platform version belongs.
    platformBranchName :: Core.Maybe Types.BranchName,
    -- | The category of platform version.
    platformCategory :: Core.Maybe Types.PlatformCategory,
    -- | The state of the platform version in its lifecycle.
    --
    -- Possible values: @recommended@ | empty
    -- If an empty value is returned, the platform version is supported but isn't the recommended one for its branch.
    platformLifecycleState :: Core.Maybe Types.PlatformLifecycleState,
    -- | The AWS account ID of the person who created the platform version.
    platformOwner :: Core.Maybe Types.PlatformOwner,
    -- | The status of the platform version. You can create an environment from the platform version once it is ready.
    platformStatus :: Core.Maybe Types.PlatformStatus,
    -- | The version string of the platform version.
    platformVersion :: Core.Maybe Types.PlatformVersion,
    -- | The additions associated with the platform version.
    supportedAddonList :: Core.Maybe [Types.SupportedAddon],
    -- | The tiers in which the platform version runs.
    supportedTierList :: Core.Maybe [Types.SupportedTier]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformSummary' value with any optional fields omitted.
mkPlatformSummary ::
  PlatformSummary
mkPlatformSummary =
  PlatformSummary'
    { operatingSystemName = Core.Nothing,
      operatingSystemVersion = Core.Nothing,
      platformArn = Core.Nothing,
      platformBranchLifecycleState = Core.Nothing,
      platformBranchName = Core.Nothing,
      platformCategory = Core.Nothing,
      platformLifecycleState = Core.Nothing,
      platformOwner = Core.Nothing,
      platformStatus = Core.Nothing,
      platformVersion = Core.Nothing,
      supportedAddonList = Core.Nothing,
      supportedTierList = Core.Nothing
    }

-- | The operating system used by the platform version.
--
-- /Note:/ Consider using 'operatingSystemName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psOperatingSystemName :: Lens.Lens' PlatformSummary (Core.Maybe Types.OperatingSystemName)
psOperatingSystemName = Lens.field @"operatingSystemName"
{-# DEPRECATED psOperatingSystemName "Use generic-lens or generic-optics with 'operatingSystemName' instead." #-}

-- | The version of the operating system used by the platform version.
--
-- /Note:/ Consider using 'operatingSystemVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psOperatingSystemVersion :: Lens.Lens' PlatformSummary (Core.Maybe Types.OperatingSystemVersion)
psOperatingSystemVersion = Lens.field @"operatingSystemVersion"
{-# DEPRECATED psOperatingSystemVersion "Use generic-lens or generic-optics with 'operatingSystemVersion' instead." #-}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformArn :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformArn)
psPlatformArn = Lens.field @"platformArn"
{-# DEPRECATED psPlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

-- | The state of the platform version's branch in its lifecycle.
--
-- Possible values: @beta@ | @supported@ | @deprecated@ | @retired@
--
-- /Note:/ Consider using 'platformBranchLifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformBranchLifecycleState :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformBranchLifecycleState)
psPlatformBranchLifecycleState = Lens.field @"platformBranchLifecycleState"
{-# DEPRECATED psPlatformBranchLifecycleState "Use generic-lens or generic-optics with 'platformBranchLifecycleState' instead." #-}

-- | The platform branch to which the platform version belongs.
--
-- /Note:/ Consider using 'platformBranchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformBranchName :: Lens.Lens' PlatformSummary (Core.Maybe Types.BranchName)
psPlatformBranchName = Lens.field @"platformBranchName"
{-# DEPRECATED psPlatformBranchName "Use generic-lens or generic-optics with 'platformBranchName' instead." #-}

-- | The category of platform version.
--
-- /Note:/ Consider using 'platformCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformCategory :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformCategory)
psPlatformCategory = Lens.field @"platformCategory"
{-# DEPRECATED psPlatformCategory "Use generic-lens or generic-optics with 'platformCategory' instead." #-}

-- | The state of the platform version in its lifecycle.
--
-- Possible values: @recommended@ | empty
-- If an empty value is returned, the platform version is supported but isn't the recommended one for its branch.
--
-- /Note:/ Consider using 'platformLifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformLifecycleState :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformLifecycleState)
psPlatformLifecycleState = Lens.field @"platformLifecycleState"
{-# DEPRECATED psPlatformLifecycleState "Use generic-lens or generic-optics with 'platformLifecycleState' instead." #-}

-- | The AWS account ID of the person who created the platform version.
--
-- /Note:/ Consider using 'platformOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformOwner :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformOwner)
psPlatformOwner = Lens.field @"platformOwner"
{-# DEPRECATED psPlatformOwner "Use generic-lens or generic-optics with 'platformOwner' instead." #-}

-- | The status of the platform version. You can create an environment from the platform version once it is ready.
--
-- /Note:/ Consider using 'platformStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformStatus :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformStatus)
psPlatformStatus = Lens.field @"platformStatus"
{-# DEPRECATED psPlatformStatus "Use generic-lens or generic-optics with 'platformStatus' instead." #-}

-- | The version string of the platform version.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psPlatformVersion :: Lens.Lens' PlatformSummary (Core.Maybe Types.PlatformVersion)
psPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED psPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The additions associated with the platform version.
--
-- /Note:/ Consider using 'supportedAddonList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSupportedAddonList :: Lens.Lens' PlatformSummary (Core.Maybe [Types.SupportedAddon])
psSupportedAddonList = Lens.field @"supportedAddonList"
{-# DEPRECATED psSupportedAddonList "Use generic-lens or generic-optics with 'supportedAddonList' instead." #-}

-- | The tiers in which the platform version runs.
--
-- /Note:/ Consider using 'supportedTierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSupportedTierList :: Lens.Lens' PlatformSummary (Core.Maybe [Types.SupportedTier])
psSupportedTierList = Lens.field @"supportedTierList"
{-# DEPRECATED psSupportedTierList "Use generic-lens or generic-optics with 'supportedTierList' instead." #-}

instance Core.FromXML PlatformSummary where
  parseXML x =
    PlatformSummary'
      Core.<$> (x Core..@? "OperatingSystemName")
      Core.<*> (x Core..@? "OperatingSystemVersion")
      Core.<*> (x Core..@? "PlatformArn")
      Core.<*> (x Core..@? "PlatformBranchLifecycleState")
      Core.<*> (x Core..@? "PlatformBranchName")
      Core.<*> (x Core..@? "PlatformCategory")
      Core.<*> (x Core..@? "PlatformLifecycleState")
      Core.<*> (x Core..@? "PlatformOwner")
      Core.<*> (x Core..@? "PlatformStatus")
      Core.<*> (x Core..@? "PlatformVersion")
      Core.<*> ( x Core..@? "SupportedAddonList"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "SupportedTierList"
                   Core..<@> Core.parseXMLList "member"
               )
