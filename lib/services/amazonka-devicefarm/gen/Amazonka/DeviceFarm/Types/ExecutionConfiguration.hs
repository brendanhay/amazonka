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
-- Module      : Amazonka.DeviceFarm.Types.ExecutionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ExecutionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents configuration information about a test run, such as the
-- execution timeout (in minutes).
--
-- /See:/ 'newExecutionConfiguration' smart constructor.
data ExecutionConfiguration = ExecutionConfiguration'
  { -- | True if account cleanup is enabled at the beginning of the test.
    -- Otherwise, false.
    accountsCleanup :: Prelude.Maybe Prelude.Bool,
    -- | True if app package cleanup is enabled at the beginning of the test.
    -- Otherwise, false.
    appPackagesCleanup :: Prelude.Maybe Prelude.Bool,
    -- | The number of minutes a test run executes before it times out.
    jobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | When set to @true@, for private devices, Device Farm does not sign your
    -- app again. For public devices, Device Farm always signs your apps again.
    --
    -- For more information about how Device Farm re-signs your apps, see
    -- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
    -- /AWS Device Farm FAQs/.
    skipAppResign :: Prelude.Maybe Prelude.Bool,
    -- | Set to true to enable video capture. Otherwise, set to false. The
    -- default is true.
    videoCapture :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountsCleanup', 'executionConfiguration_accountsCleanup' - True if account cleanup is enabled at the beginning of the test.
-- Otherwise, false.
--
-- 'appPackagesCleanup', 'executionConfiguration_appPackagesCleanup' - True if app package cleanup is enabled at the beginning of the test.
-- Otherwise, false.
--
-- 'jobTimeoutMinutes', 'executionConfiguration_jobTimeoutMinutes' - The number of minutes a test run executes before it times out.
--
-- 'skipAppResign', 'executionConfiguration_skipAppResign' - When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
--
-- 'videoCapture', 'executionConfiguration_videoCapture' - Set to true to enable video capture. Otherwise, set to false. The
-- default is true.
newExecutionConfiguration ::
  ExecutionConfiguration
newExecutionConfiguration =
  ExecutionConfiguration'
    { accountsCleanup =
        Prelude.Nothing,
      appPackagesCleanup = Prelude.Nothing,
      jobTimeoutMinutes = Prelude.Nothing,
      skipAppResign = Prelude.Nothing,
      videoCapture = Prelude.Nothing
    }

-- | True if account cleanup is enabled at the beginning of the test.
-- Otherwise, false.
executionConfiguration_accountsCleanup :: Lens.Lens' ExecutionConfiguration (Prelude.Maybe Prelude.Bool)
executionConfiguration_accountsCleanup = Lens.lens (\ExecutionConfiguration' {accountsCleanup} -> accountsCleanup) (\s@ExecutionConfiguration' {} a -> s {accountsCleanup = a} :: ExecutionConfiguration)

-- | True if app package cleanup is enabled at the beginning of the test.
-- Otherwise, false.
executionConfiguration_appPackagesCleanup :: Lens.Lens' ExecutionConfiguration (Prelude.Maybe Prelude.Bool)
executionConfiguration_appPackagesCleanup = Lens.lens (\ExecutionConfiguration' {appPackagesCleanup} -> appPackagesCleanup) (\s@ExecutionConfiguration' {} a -> s {appPackagesCleanup = a} :: ExecutionConfiguration)

-- | The number of minutes a test run executes before it times out.
executionConfiguration_jobTimeoutMinutes :: Lens.Lens' ExecutionConfiguration (Prelude.Maybe Prelude.Int)
executionConfiguration_jobTimeoutMinutes = Lens.lens (\ExecutionConfiguration' {jobTimeoutMinutes} -> jobTimeoutMinutes) (\s@ExecutionConfiguration' {} a -> s {jobTimeoutMinutes = a} :: ExecutionConfiguration)

-- | When set to @true@, for private devices, Device Farm does not sign your
-- app again. For public devices, Device Farm always signs your apps again.
--
-- For more information about how Device Farm re-signs your apps, see
-- <http://aws.amazon.com/device-farm/faqs/ Do you modify my app?> in the
-- /AWS Device Farm FAQs/.
executionConfiguration_skipAppResign :: Lens.Lens' ExecutionConfiguration (Prelude.Maybe Prelude.Bool)
executionConfiguration_skipAppResign = Lens.lens (\ExecutionConfiguration' {skipAppResign} -> skipAppResign) (\s@ExecutionConfiguration' {} a -> s {skipAppResign = a} :: ExecutionConfiguration)

-- | Set to true to enable video capture. Otherwise, set to false. The
-- default is true.
executionConfiguration_videoCapture :: Lens.Lens' ExecutionConfiguration (Prelude.Maybe Prelude.Bool)
executionConfiguration_videoCapture = Lens.lens (\ExecutionConfiguration' {videoCapture} -> videoCapture) (\s@ExecutionConfiguration' {} a -> s {videoCapture = a} :: ExecutionConfiguration)

instance Prelude.Hashable ExecutionConfiguration where
  hashWithSalt _salt ExecutionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` accountsCleanup
      `Prelude.hashWithSalt` appPackagesCleanup
      `Prelude.hashWithSalt` jobTimeoutMinutes
      `Prelude.hashWithSalt` skipAppResign
      `Prelude.hashWithSalt` videoCapture

instance Prelude.NFData ExecutionConfiguration where
  rnf ExecutionConfiguration' {..} =
    Prelude.rnf accountsCleanup `Prelude.seq`
      Prelude.rnf appPackagesCleanup `Prelude.seq`
        Prelude.rnf jobTimeoutMinutes `Prelude.seq`
          Prelude.rnf skipAppResign `Prelude.seq`
            Prelude.rnf videoCapture

instance Data.ToJSON ExecutionConfiguration where
  toJSON ExecutionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountsCleanup" Data..=)
              Prelude.<$> accountsCleanup,
            ("appPackagesCleanup" Data..=)
              Prelude.<$> appPackagesCleanup,
            ("jobTimeoutMinutes" Data..=)
              Prelude.<$> jobTimeoutMinutes,
            ("skipAppResign" Data..=) Prelude.<$> skipAppResign,
            ("videoCapture" Data..=) Prelude.<$> videoCapture
          ]
      )
