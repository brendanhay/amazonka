{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionConfiguration
  ( ExecutionConfiguration (..),

    -- * Smart constructor
    mkExecutionConfiguration,

    -- * Lenses
    ecAccountsCleanup,
    ecAppPackagesCleanup,
    ecJobTimeoutMinutes,
    ecSkipAppResign,
    ecVideoCapture,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents configuration information about a test run, such as the execution timeout (in minutes).
--
-- /See:/ 'mkExecutionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { -- | True if account cleanup is enabled at the beginning of the test. Otherwise, false.
    accountsCleanup :: Core.Maybe Core.Bool,
    -- | True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
    appPackagesCleanup :: Core.Maybe Core.Bool,
    -- | The number of minutes a test run executes before it times out.
    jobTimeoutMinutes :: Core.Maybe Core.Int,
    -- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
    skipAppResign :: Core.Maybe Core.Bool,
    -- | Set to true to enable video capture. Otherwise, set to false. The default is true.
    videoCapture :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionConfiguration' value with any optional fields omitted.
mkExecutionConfiguration ::
  ExecutionConfiguration
mkExecutionConfiguration =
  ExecutionConfiguration'
    { accountsCleanup = Core.Nothing,
      appPackagesCleanup = Core.Nothing,
      jobTimeoutMinutes = Core.Nothing,
      skipAppResign = Core.Nothing,
      videoCapture = Core.Nothing
    }

-- | True if account cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'accountsCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecAccountsCleanup :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
ecAccountsCleanup = Lens.field @"accountsCleanup"
{-# DEPRECATED ecAccountsCleanup "Use generic-lens or generic-optics with 'accountsCleanup' instead." #-}

-- | True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'appPackagesCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecAppPackagesCleanup :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
ecAppPackagesCleanup = Lens.field @"appPackagesCleanup"
{-# DEPRECATED ecAppPackagesCleanup "Use generic-lens or generic-optics with 'appPackagesCleanup' instead." #-}

-- | The number of minutes a test run executes before it times out.
--
-- /Note:/ Consider using 'jobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecJobTimeoutMinutes :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Int)
ecJobTimeoutMinutes = Lens.field @"jobTimeoutMinutes"
{-# DEPRECATED ecJobTimeoutMinutes "Use generic-lens or generic-optics with 'jobTimeoutMinutes' instead." #-}

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecSkipAppResign :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
ecSkipAppResign = Lens.field @"skipAppResign"
{-# DEPRECATED ecSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | Set to true to enable video capture. Otherwise, set to false. The default is true.
--
-- /Note:/ Consider using 'videoCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecVideoCapture :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
ecVideoCapture = Lens.field @"videoCapture"
{-# DEPRECATED ecVideoCapture "Use generic-lens or generic-optics with 'videoCapture' instead." #-}

instance Core.FromJSON ExecutionConfiguration where
  toJSON ExecutionConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("accountsCleanup" Core..=) Core.<$> accountsCleanup,
            ("appPackagesCleanup" Core..=) Core.<$> appPackagesCleanup,
            ("jobTimeoutMinutes" Core..=) Core.<$> jobTimeoutMinutes,
            ("skipAppResign" Core..=) Core.<$> skipAppResign,
            ("videoCapture" Core..=) Core.<$> videoCapture
          ]
      )
