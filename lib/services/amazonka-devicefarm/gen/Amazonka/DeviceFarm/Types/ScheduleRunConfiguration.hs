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
-- Module      : Amazonka.DeviceFarm.Types.ScheduleRunConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ScheduleRunConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.BillingMethod
import Amazonka.DeviceFarm.Types.CustomerArtifactPaths
import Amazonka.DeviceFarm.Types.Location
import Amazonka.DeviceFarm.Types.Radios
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings for a run. Includes things like location, radio
-- states, auxiliary apps, and network profiles.
--
-- /See:/ 'newScheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { -- | Specifies the billing method for a test run: @metered@ or @unmetered@.
    -- If the parameter is not specified, the default value is @metered@.
    --
    -- If you have purchased unmetered device slots, you must set this
    -- parameter to @unmetered@ to make use of them. Otherwise, your run counts
    -- against your metered time.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | Input @CustomerArtifactPaths@ object for the scheduled run
    -- configuration.
    customerArtifactPaths :: Prelude.Maybe CustomerArtifactPaths,
    -- | Information about the radio states for the run.
    radios :: Prelude.Maybe Radios,
    -- | Information about the location that is used for the run.
    location :: Prelude.Maybe Location,
    -- | Information about the locale that is used for the run.
    locale :: Prelude.Maybe Prelude.Text,
    -- | Reserved for internal use.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the extra data for the run. The extra data is a .zip file
    -- that AWS Device Farm extracts to external data for Android or the app\'s
    -- sandbox for iOS.
    extraDataPackageArn :: Prelude.Maybe Prelude.Text,
    -- | A list of upload ARNs for app packages to be installed with your app.
    auxiliaryApps :: Prelude.Maybe [Prelude.Text],
    -- | An array of ARNs for your VPC endpoint configurations.
    vpceConfigurationArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduleRunConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMethod', 'scheduleRunConfiguration_billingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have purchased unmetered device slots, you must set this
-- parameter to @unmetered@ to make use of them. Otherwise, your run counts
-- against your metered time.
--
-- 'customerArtifactPaths', 'scheduleRunConfiguration_customerArtifactPaths' - Input @CustomerArtifactPaths@ object for the scheduled run
-- configuration.
--
-- 'radios', 'scheduleRunConfiguration_radios' - Information about the radio states for the run.
--
-- 'location', 'scheduleRunConfiguration_location' - Information about the location that is used for the run.
--
-- 'locale', 'scheduleRunConfiguration_locale' - Information about the locale that is used for the run.
--
-- 'networkProfileArn', 'scheduleRunConfiguration_networkProfileArn' - Reserved for internal use.
--
-- 'extraDataPackageArn', 'scheduleRunConfiguration_extraDataPackageArn' - The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
--
-- 'auxiliaryApps', 'scheduleRunConfiguration_auxiliaryApps' - A list of upload ARNs for app packages to be installed with your app.
--
-- 'vpceConfigurationArns', 'scheduleRunConfiguration_vpceConfigurationArns' - An array of ARNs for your VPC endpoint configurations.
newScheduleRunConfiguration ::
  ScheduleRunConfiguration
newScheduleRunConfiguration =
  ScheduleRunConfiguration'
    { billingMethod =
        Prelude.Nothing,
      customerArtifactPaths = Prelude.Nothing,
      radios = Prelude.Nothing,
      location = Prelude.Nothing,
      locale = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      extraDataPackageArn = Prelude.Nothing,
      auxiliaryApps = Prelude.Nothing,
      vpceConfigurationArns = Prelude.Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have purchased unmetered device slots, you must set this
-- parameter to @unmetered@ to make use of them. Otherwise, your run counts
-- against your metered time.
scheduleRunConfiguration_billingMethod :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe BillingMethod)
scheduleRunConfiguration_billingMethod = Lens.lens (\ScheduleRunConfiguration' {billingMethod} -> billingMethod) (\s@ScheduleRunConfiguration' {} a -> s {billingMethod = a} :: ScheduleRunConfiguration)

-- | Input @CustomerArtifactPaths@ object for the scheduled run
-- configuration.
scheduleRunConfiguration_customerArtifactPaths :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe CustomerArtifactPaths)
scheduleRunConfiguration_customerArtifactPaths = Lens.lens (\ScheduleRunConfiguration' {customerArtifactPaths} -> customerArtifactPaths) (\s@ScheduleRunConfiguration' {} a -> s {customerArtifactPaths = a} :: ScheduleRunConfiguration)

-- | Information about the radio states for the run.
scheduleRunConfiguration_radios :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Radios)
scheduleRunConfiguration_radios = Lens.lens (\ScheduleRunConfiguration' {radios} -> radios) (\s@ScheduleRunConfiguration' {} a -> s {radios = a} :: ScheduleRunConfiguration)

-- | Information about the location that is used for the run.
scheduleRunConfiguration_location :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Location)
scheduleRunConfiguration_location = Lens.lens (\ScheduleRunConfiguration' {location} -> location) (\s@ScheduleRunConfiguration' {} a -> s {location = a} :: ScheduleRunConfiguration)

-- | Information about the locale that is used for the run.
scheduleRunConfiguration_locale :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_locale = Lens.lens (\ScheduleRunConfiguration' {locale} -> locale) (\s@ScheduleRunConfiguration' {} a -> s {locale = a} :: ScheduleRunConfiguration)

-- | Reserved for internal use.
scheduleRunConfiguration_networkProfileArn :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_networkProfileArn = Lens.lens (\ScheduleRunConfiguration' {networkProfileArn} -> networkProfileArn) (\s@ScheduleRunConfiguration' {} a -> s {networkProfileArn = a} :: ScheduleRunConfiguration)

-- | The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
scheduleRunConfiguration_extraDataPackageArn :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_extraDataPackageArn = Lens.lens (\ScheduleRunConfiguration' {extraDataPackageArn} -> extraDataPackageArn) (\s@ScheduleRunConfiguration' {} a -> s {extraDataPackageArn = a} :: ScheduleRunConfiguration)

-- | A list of upload ARNs for app packages to be installed with your app.
scheduleRunConfiguration_auxiliaryApps :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe [Prelude.Text])
scheduleRunConfiguration_auxiliaryApps = Lens.lens (\ScheduleRunConfiguration' {auxiliaryApps} -> auxiliaryApps) (\s@ScheduleRunConfiguration' {} a -> s {auxiliaryApps = a} :: ScheduleRunConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | An array of ARNs for your VPC endpoint configurations.
scheduleRunConfiguration_vpceConfigurationArns :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe [Prelude.Text])
scheduleRunConfiguration_vpceConfigurationArns = Lens.lens (\ScheduleRunConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@ScheduleRunConfiguration' {} a -> s {vpceConfigurationArns = a} :: ScheduleRunConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ScheduleRunConfiguration where
  hashWithSalt salt' ScheduleRunConfiguration' {..} =
    salt' `Prelude.hashWithSalt` vpceConfigurationArns
      `Prelude.hashWithSalt` auxiliaryApps
      `Prelude.hashWithSalt` extraDataPackageArn
      `Prelude.hashWithSalt` networkProfileArn
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` radios
      `Prelude.hashWithSalt` customerArtifactPaths
      `Prelude.hashWithSalt` billingMethod

instance Prelude.NFData ScheduleRunConfiguration where
  rnf ScheduleRunConfiguration' {..} =
    Prelude.rnf billingMethod
      `Prelude.seq` Prelude.rnf vpceConfigurationArns
      `Prelude.seq` Prelude.rnf auxiliaryApps
      `Prelude.seq` Prelude.rnf extraDataPackageArn
      `Prelude.seq` Prelude.rnf networkProfileArn
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf radios
      `Prelude.seq` Prelude.rnf customerArtifactPaths

instance Core.ToJSON ScheduleRunConfiguration where
  toJSON ScheduleRunConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("billingMethod" Core..=) Prelude.<$> billingMethod,
            ("customerArtifactPaths" Core..=)
              Prelude.<$> customerArtifactPaths,
            ("radios" Core..=) Prelude.<$> radios,
            ("location" Core..=) Prelude.<$> location,
            ("locale" Core..=) Prelude.<$> locale,
            ("networkProfileArn" Core..=)
              Prelude.<$> networkProfileArn,
            ("extraDataPackageArn" Core..=)
              Prelude.<$> extraDataPackageArn,
            ("auxiliaryApps" Core..=) Prelude.<$> auxiliaryApps,
            ("vpceConfigurationArns" Core..=)
              Prelude.<$> vpceConfigurationArns
          ]
      )
