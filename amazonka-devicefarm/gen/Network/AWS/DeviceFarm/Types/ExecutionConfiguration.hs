{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents configuration information about a test run, such as the
-- execution timeout (in minutes).
--
-- /See:/ 'newExecutionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { -- | True if app package cleanup is enabled at the beginning of the test.
    -- Otherwise, false.
    appPackagesCleanup :: Core.Maybe Core.Bool,
    -- | Set to true to enable video capture. Otherwise, set to false. The
    -- default is true.
    videoCapture :: Core.Maybe Core.Bool,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Core.Maybe Core.Bool,
    -- | The number of minutes a test run executes before it times out.
    jobTimeoutMinutes :: Core.Maybe Core.Int,
    -- | True if account cleanup is enabled at the beginning of the test.
    -- Otherwise, false.
    accountsCleanup :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecutionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appPackagesCleanup', 'executionConfiguration_appPackagesCleanup' - True if app package cleanup is enabled at the beginning of the test.
-- Otherwise, false.
--
-- 'videoCapture', 'executionConfiguration_videoCapture' - Set to true to enable video capture. Otherwise, set to false. The
-- default is true.
--
-- 'skipAppResign', 'executionConfiguration_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'jobTimeoutMinutes', 'executionConfiguration_jobTimeoutMinutes' - The number of minutes a test run executes before it times out.
--
-- 'accountsCleanup', 'executionConfiguration_accountsCleanup' - True if account cleanup is enabled at the beginning of the test.
-- Otherwise, false.
newExecutionConfiguration ::
  ExecutionConfiguration
newExecutionConfiguration =
  ExecutionConfiguration'
    { appPackagesCleanup =
        Core.Nothing,
      videoCapture = Core.Nothing,
      skipAppResign = Core.Nothing,
      jobTimeoutMinutes = Core.Nothing,
      accountsCleanup = Core.Nothing
    }

-- | True if app package cleanup is enabled at the beginning of the test.
-- Otherwise, false.
executionConfiguration_appPackagesCleanup :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
executionConfiguration_appPackagesCleanup = Lens.lens (\ExecutionConfiguration' {appPackagesCleanup} -> appPackagesCleanup) (\s@ExecutionConfiguration' {} a -> s {appPackagesCleanup = a} :: ExecutionConfiguration)

-- | Set to true to enable video capture. Otherwise, set to false. The
-- default is true.
executionConfiguration_videoCapture :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
executionConfiguration_videoCapture = Lens.lens (\ExecutionConfiguration' {videoCapture} -> videoCapture) (\s@ExecutionConfiguration' {} a -> s {videoCapture = a} :: ExecutionConfiguration)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <https://aws.amazon.com/device-farm/faq/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
executionConfiguration_skipAppResign :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
executionConfiguration_skipAppResign = Lens.lens (\ExecutionConfiguration' {skipAppResign} -> skipAppResign) (\s@ExecutionConfiguration' {} a -> s {skipAppResign = a} :: ExecutionConfiguration)

-- | The number of minutes a test run executes before it times out.
executionConfiguration_jobTimeoutMinutes :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Int)
executionConfiguration_jobTimeoutMinutes = Lens.lens (\ExecutionConfiguration' {jobTimeoutMinutes} -> jobTimeoutMinutes) (\s@ExecutionConfiguration' {} a -> s {jobTimeoutMinutes = a} :: ExecutionConfiguration)

-- | True if account cleanup is enabled at the beginning of the test.
-- Otherwise, false.
executionConfiguration_accountsCleanup :: Lens.Lens' ExecutionConfiguration (Core.Maybe Core.Bool)
executionConfiguration_accountsCleanup = Lens.lens (\ExecutionConfiguration' {accountsCleanup} -> accountsCleanup) (\s@ExecutionConfiguration' {} a -> s {accountsCleanup = a} :: ExecutionConfiguration)

instance Core.Hashable ExecutionConfiguration

instance Core.NFData ExecutionConfiguration

instance Core.ToJSON ExecutionConfiguration where
  toJSON ExecutionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appPackagesCleanup" Core..=)
              Core.<$> appPackagesCleanup,
            ("videoCapture" Core..=) Core.<$> videoCapture,
            ("skipAppResign" Core..=) Core.<$> skipAppResign,
            ("jobTimeoutMinutes" Core..=)
              Core.<$> jobTimeoutMinutes,
            ("accountsCleanup" Core..=)
              Core.<$> accountsCleanup
          ]
      )
