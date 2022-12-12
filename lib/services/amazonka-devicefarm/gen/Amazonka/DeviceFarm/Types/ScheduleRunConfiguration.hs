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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ScheduleRunConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types.BillingMethod
import Amazonka.DeviceFarm.Types.CustomerArtifactPaths
import Amazonka.DeviceFarm.Types.Location
import Amazonka.DeviceFarm.Types.Radios
import qualified Amazonka.Prelude as Prelude

-- | Represents the settings for a run. Includes things like location, radio
-- states, auxiliary apps, and network profiles.
--
-- /See:/ 'newScheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { -- | A list of upload ARNs for app packages to be installed with your app.
    auxiliaryApps :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the billing method for a test run: @metered@ or @unmetered@.
    -- If the parameter is not specified, the default value is @metered@.
    --
    -- If you have purchased unmetered device slots, you must set this
    -- parameter to @unmetered@ to make use of them. Otherwise, your run counts
    -- against your metered time.
    billingMethod :: Prelude.Maybe BillingMethod,
    -- | Input @CustomerArtifactPaths@ object for the scheduled run
    -- configuration.
    customerArtifactPaths :: Prelude.Maybe CustomerArtifactPaths,
    -- | The ARN of the extra data for the run. The extra data is a .zip file
    -- that AWS Device Farm extracts to external data for Android or the app\'s
    -- sandbox for iOS.
    extraDataPackageArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the locale that is used for the run.
    locale :: Prelude.Maybe Prelude.Text,
    -- | Information about the location that is used for the run.
    location :: Prelude.Maybe Location,
    -- | Reserved for internal use.
    networkProfileArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the radio states for the run.
    radios :: Prelude.Maybe Radios,
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
-- 'auxiliaryApps', 'scheduleRunConfiguration_auxiliaryApps' - A list of upload ARNs for app packages to be installed with your app.
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
-- 'extraDataPackageArn', 'scheduleRunConfiguration_extraDataPackageArn' - The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
--
-- 'locale', 'scheduleRunConfiguration_locale' - Information about the locale that is used for the run.
--
-- 'location', 'scheduleRunConfiguration_location' - Information about the location that is used for the run.
--
-- 'networkProfileArn', 'scheduleRunConfiguration_networkProfileArn' - Reserved for internal use.
--
-- 'radios', 'scheduleRunConfiguration_radios' - Information about the radio states for the run.
--
-- 'vpceConfigurationArns', 'scheduleRunConfiguration_vpceConfigurationArns' - An array of ARNs for your VPC endpoint configurations.
newScheduleRunConfiguration ::
  ScheduleRunConfiguration
newScheduleRunConfiguration =
  ScheduleRunConfiguration'
    { auxiliaryApps =
        Prelude.Nothing,
      billingMethod = Prelude.Nothing,
      customerArtifactPaths = Prelude.Nothing,
      extraDataPackageArn = Prelude.Nothing,
      locale = Prelude.Nothing,
      location = Prelude.Nothing,
      networkProfileArn = Prelude.Nothing,
      radios = Prelude.Nothing,
      vpceConfigurationArns = Prelude.Nothing
    }

-- | A list of upload ARNs for app packages to be installed with your app.
scheduleRunConfiguration_auxiliaryApps :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe [Prelude.Text])
scheduleRunConfiguration_auxiliaryApps = Lens.lens (\ScheduleRunConfiguration' {auxiliaryApps} -> auxiliaryApps) (\s@ScheduleRunConfiguration' {} a -> s {auxiliaryApps = a} :: ScheduleRunConfiguration) Prelude.. Lens.mapping Lens.coerced

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

-- | The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
scheduleRunConfiguration_extraDataPackageArn :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_extraDataPackageArn = Lens.lens (\ScheduleRunConfiguration' {extraDataPackageArn} -> extraDataPackageArn) (\s@ScheduleRunConfiguration' {} a -> s {extraDataPackageArn = a} :: ScheduleRunConfiguration)

-- | Information about the locale that is used for the run.
scheduleRunConfiguration_locale :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_locale = Lens.lens (\ScheduleRunConfiguration' {locale} -> locale) (\s@ScheduleRunConfiguration' {} a -> s {locale = a} :: ScheduleRunConfiguration)

-- | Information about the location that is used for the run.
scheduleRunConfiguration_location :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Location)
scheduleRunConfiguration_location = Lens.lens (\ScheduleRunConfiguration' {location} -> location) (\s@ScheduleRunConfiguration' {} a -> s {location = a} :: ScheduleRunConfiguration)

-- | Reserved for internal use.
scheduleRunConfiguration_networkProfileArn :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Prelude.Text)
scheduleRunConfiguration_networkProfileArn = Lens.lens (\ScheduleRunConfiguration' {networkProfileArn} -> networkProfileArn) (\s@ScheduleRunConfiguration' {} a -> s {networkProfileArn = a} :: ScheduleRunConfiguration)

-- | Information about the radio states for the run.
scheduleRunConfiguration_radios :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe Radios)
scheduleRunConfiguration_radios = Lens.lens (\ScheduleRunConfiguration' {radios} -> radios) (\s@ScheduleRunConfiguration' {} a -> s {radios = a} :: ScheduleRunConfiguration)

-- | An array of ARNs for your VPC endpoint configurations.
scheduleRunConfiguration_vpceConfigurationArns :: Lens.Lens' ScheduleRunConfiguration (Prelude.Maybe [Prelude.Text])
scheduleRunConfiguration_vpceConfigurationArns = Lens.lens (\ScheduleRunConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@ScheduleRunConfiguration' {} a -> s {vpceConfigurationArns = a} :: ScheduleRunConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ScheduleRunConfiguration where
  hashWithSalt _salt ScheduleRunConfiguration' {..} =
    _salt `Prelude.hashWithSalt` auxiliaryApps
      `Prelude.hashWithSalt` billingMethod
      `Prelude.hashWithSalt` customerArtifactPaths
      `Prelude.hashWithSalt` extraDataPackageArn
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` networkProfileArn
      `Prelude.hashWithSalt` radios
      `Prelude.hashWithSalt` vpceConfigurationArns

instance Prelude.NFData ScheduleRunConfiguration where
  rnf ScheduleRunConfiguration' {..} =
    Prelude.rnf auxiliaryApps
      `Prelude.seq` Prelude.rnf billingMethod
      `Prelude.seq` Prelude.rnf customerArtifactPaths
      `Prelude.seq` Prelude.rnf extraDataPackageArn
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf networkProfileArn
      `Prelude.seq` Prelude.rnf radios
      `Prelude.seq` Prelude.rnf vpceConfigurationArns

instance Data.ToJSON ScheduleRunConfiguration where
  toJSON ScheduleRunConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("auxiliaryApps" Data..=) Prelude.<$> auxiliaryApps,
            ("billingMethod" Data..=) Prelude.<$> billingMethod,
            ("customerArtifactPaths" Data..=)
              Prelude.<$> customerArtifactPaths,
            ("extraDataPackageArn" Data..=)
              Prelude.<$> extraDataPackageArn,
            ("locale" Data..=) Prelude.<$> locale,
            ("location" Data..=) Prelude.<$> location,
            ("networkProfileArn" Data..=)
              Prelude.<$> networkProfileArn,
            ("radios" Data..=) Prelude.<$> radios,
            ("vpceConfigurationArns" Data..=)
              Prelude.<$> vpceConfigurationArns
          ]
      )
