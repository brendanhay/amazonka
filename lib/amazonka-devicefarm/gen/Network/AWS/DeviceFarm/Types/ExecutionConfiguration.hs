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
    ecSkipAppResign,
    ecAccountsCleanup,
    ecAppPackagesCleanup,
    ecJobTimeoutMinutes,
    ecVideoCapture,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents configuration information about a test run, such as the execution timeout (in minutes).
--
-- /See:/ 'mkExecutionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { skipAppResign ::
      Lude.Maybe Lude.Bool,
    accountsCleanup :: Lude.Maybe Lude.Bool,
    appPackagesCleanup :: Lude.Maybe Lude.Bool,
    jobTimeoutMinutes :: Lude.Maybe Lude.Int,
    videoCapture :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionConfiguration' with the minimum fields required to make a request.
--
-- * 'accountsCleanup' - True if account cleanup is enabled at the beginning of the test. Otherwise, false.
-- * 'appPackagesCleanup' - True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
-- * 'jobTimeoutMinutes' - The number of minutes a test run executes before it times out.
-- * 'skipAppResign' - When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
-- * 'videoCapture' - Set to true to enable video capture. Otherwise, set to false. The default is true.
mkExecutionConfiguration ::
  ExecutionConfiguration
mkExecutionConfiguration =
  ExecutionConfiguration'
    { skipAppResign = Lude.Nothing,
      accountsCleanup = Lude.Nothing,
      appPackagesCleanup = Lude.Nothing,
      jobTimeoutMinutes = Lude.Nothing,
      videoCapture = Lude.Nothing
    }

-- | When set to @true@ , for private devices, Device Farm does not sign your app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the /AWS Device Farm FAQs/ .
--
-- /Note:/ Consider using 'skipAppResign' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecSkipAppResign :: Lens.Lens' ExecutionConfiguration (Lude.Maybe Lude.Bool)
ecSkipAppResign = Lens.lens (skipAppResign :: ExecutionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {skipAppResign = a} :: ExecutionConfiguration)
{-# DEPRECATED ecSkipAppResign "Use generic-lens or generic-optics with 'skipAppResign' instead." #-}

-- | True if account cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'accountsCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecAccountsCleanup :: Lens.Lens' ExecutionConfiguration (Lude.Maybe Lude.Bool)
ecAccountsCleanup = Lens.lens (accountsCleanup :: ExecutionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {accountsCleanup = a} :: ExecutionConfiguration)
{-# DEPRECATED ecAccountsCleanup "Use generic-lens or generic-optics with 'accountsCleanup' instead." #-}

-- | True if app package cleanup is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'appPackagesCleanup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecAppPackagesCleanup :: Lens.Lens' ExecutionConfiguration (Lude.Maybe Lude.Bool)
ecAppPackagesCleanup = Lens.lens (appPackagesCleanup :: ExecutionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {appPackagesCleanup = a} :: ExecutionConfiguration)
{-# DEPRECATED ecAppPackagesCleanup "Use generic-lens or generic-optics with 'appPackagesCleanup' instead." #-}

-- | The number of minutes a test run executes before it times out.
--
-- /Note:/ Consider using 'jobTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecJobTimeoutMinutes :: Lens.Lens' ExecutionConfiguration (Lude.Maybe Lude.Int)
ecJobTimeoutMinutes = Lens.lens (jobTimeoutMinutes :: ExecutionConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {jobTimeoutMinutes = a} :: ExecutionConfiguration)
{-# DEPRECATED ecJobTimeoutMinutes "Use generic-lens or generic-optics with 'jobTimeoutMinutes' instead." #-}

-- | Set to true to enable video capture. Otherwise, set to false. The default is true.
--
-- /Note:/ Consider using 'videoCapture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecVideoCapture :: Lens.Lens' ExecutionConfiguration (Lude.Maybe Lude.Bool)
ecVideoCapture = Lens.lens (videoCapture :: ExecutionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {videoCapture = a} :: ExecutionConfiguration)
{-# DEPRECATED ecVideoCapture "Use generic-lens or generic-optics with 'videoCapture' instead." #-}

instance Lude.ToJSON ExecutionConfiguration where
  toJSON ExecutionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("skipAppResign" Lude..=) Lude.<$> skipAppResign,
            ("accountsCleanup" Lude..=) Lude.<$> accountsCleanup,
            ("appPackagesCleanup" Lude..=) Lude.<$> appPackagesCleanup,
            ("jobTimeoutMinutes" Lude..=) Lude.<$> jobTimeoutMinutes,
            ("videoCapture" Lude..=) Lude.<$> videoCapture
          ]
      )
