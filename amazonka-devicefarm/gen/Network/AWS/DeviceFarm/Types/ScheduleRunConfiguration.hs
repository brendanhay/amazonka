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
-- Module      : Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.Radios
import qualified Network.AWS.Lens as Lens

-- | Represents the settings for a run. Includes things like location, radio
-- states, auxiliary apps, and network profiles.
--
-- /See:/ 'newScheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { -- | Information about the locale that is used for the run.
    locale :: Core.Maybe Core.Text,
    -- | A list of upload ARNs for app packages to be installed with your app.
    auxiliaryApps :: Core.Maybe [Core.Text],
    -- | Information about the radio states for the run.
    radios :: Core.Maybe Radios,
    -- | Specifies the billing method for a test run: @metered@ or @unmetered@.
    -- If the parameter is not specified, the default value is @metered@.
    --
    -- If you have purchased unmetered device slots, you must set this
    -- parameter to @unmetered@ to make use of them. Otherwise, your run counts
    -- against your metered time.
    billingMethod :: Core.Maybe BillingMethod,
    -- | Input @CustomerArtifactPaths@ object for the scheduled run
    -- configuration.
    customerArtifactPaths :: Core.Maybe CustomerArtifactPaths,
    -- | An array of ARNs for your VPC endpoint configurations.
    vpceConfigurationArns :: Core.Maybe [Core.Text],
    -- | Reserved for internal use.
    networkProfileArn :: Core.Maybe Core.Text,
    -- | Information about the location that is used for the run.
    location :: Core.Maybe Location,
    -- | The ARN of the extra data for the run. The extra data is a .zip file
    -- that AWS Device Farm extracts to external data for Android or the app\'s
    -- sandbox for iOS.
    extraDataPackageArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduleRunConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'scheduleRunConfiguration_locale' - Information about the locale that is used for the run.
--
-- 'auxiliaryApps', 'scheduleRunConfiguration_auxiliaryApps' - A list of upload ARNs for app packages to be installed with your app.
--
-- 'radios', 'scheduleRunConfiguration_radios' - Information about the radio states for the run.
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
-- 'vpceConfigurationArns', 'scheduleRunConfiguration_vpceConfigurationArns' - An array of ARNs for your VPC endpoint configurations.
--
-- 'networkProfileArn', 'scheduleRunConfiguration_networkProfileArn' - Reserved for internal use.
--
-- 'location', 'scheduleRunConfiguration_location' - Information about the location that is used for the run.
--
-- 'extraDataPackageArn', 'scheduleRunConfiguration_extraDataPackageArn' - The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
newScheduleRunConfiguration ::
  ScheduleRunConfiguration
newScheduleRunConfiguration =
  ScheduleRunConfiguration'
    { locale = Core.Nothing,
      auxiliaryApps = Core.Nothing,
      radios = Core.Nothing,
      billingMethod = Core.Nothing,
      customerArtifactPaths = Core.Nothing,
      vpceConfigurationArns = Core.Nothing,
      networkProfileArn = Core.Nothing,
      location = Core.Nothing,
      extraDataPackageArn = Core.Nothing
    }

-- | Information about the locale that is used for the run.
scheduleRunConfiguration_locale :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Core.Text)
scheduleRunConfiguration_locale = Lens.lens (\ScheduleRunConfiguration' {locale} -> locale) (\s@ScheduleRunConfiguration' {} a -> s {locale = a} :: ScheduleRunConfiguration)

-- | A list of upload ARNs for app packages to be installed with your app.
scheduleRunConfiguration_auxiliaryApps :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe [Core.Text])
scheduleRunConfiguration_auxiliaryApps = Lens.lens (\ScheduleRunConfiguration' {auxiliaryApps} -> auxiliaryApps) (\s@ScheduleRunConfiguration' {} a -> s {auxiliaryApps = a} :: ScheduleRunConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Information about the radio states for the run.
scheduleRunConfiguration_radios :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Radios)
scheduleRunConfiguration_radios = Lens.lens (\ScheduleRunConfiguration' {radios} -> radios) (\s@ScheduleRunConfiguration' {} a -> s {radios = a} :: ScheduleRunConfiguration)

-- | Specifies the billing method for a test run: @metered@ or @unmetered@.
-- If the parameter is not specified, the default value is @metered@.
--
-- If you have purchased unmetered device slots, you must set this
-- parameter to @unmetered@ to make use of them. Otherwise, your run counts
-- against your metered time.
scheduleRunConfiguration_billingMethod :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe BillingMethod)
scheduleRunConfiguration_billingMethod = Lens.lens (\ScheduleRunConfiguration' {billingMethod} -> billingMethod) (\s@ScheduleRunConfiguration' {} a -> s {billingMethod = a} :: ScheduleRunConfiguration)

-- | Input @CustomerArtifactPaths@ object for the scheduled run
-- configuration.
scheduleRunConfiguration_customerArtifactPaths :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe CustomerArtifactPaths)
scheduleRunConfiguration_customerArtifactPaths = Lens.lens (\ScheduleRunConfiguration' {customerArtifactPaths} -> customerArtifactPaths) (\s@ScheduleRunConfiguration' {} a -> s {customerArtifactPaths = a} :: ScheduleRunConfiguration)

-- | An array of ARNs for your VPC endpoint configurations.
scheduleRunConfiguration_vpceConfigurationArns :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe [Core.Text])
scheduleRunConfiguration_vpceConfigurationArns = Lens.lens (\ScheduleRunConfiguration' {vpceConfigurationArns} -> vpceConfigurationArns) (\s@ScheduleRunConfiguration' {} a -> s {vpceConfigurationArns = a} :: ScheduleRunConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Reserved for internal use.
scheduleRunConfiguration_networkProfileArn :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Core.Text)
scheduleRunConfiguration_networkProfileArn = Lens.lens (\ScheduleRunConfiguration' {networkProfileArn} -> networkProfileArn) (\s@ScheduleRunConfiguration' {} a -> s {networkProfileArn = a} :: ScheduleRunConfiguration)

-- | Information about the location that is used for the run.
scheduleRunConfiguration_location :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Location)
scheduleRunConfiguration_location = Lens.lens (\ScheduleRunConfiguration' {location} -> location) (\s@ScheduleRunConfiguration' {} a -> s {location = a} :: ScheduleRunConfiguration)

-- | The ARN of the extra data for the run. The extra data is a .zip file
-- that AWS Device Farm extracts to external data for Android or the app\'s
-- sandbox for iOS.
scheduleRunConfiguration_extraDataPackageArn :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Core.Text)
scheduleRunConfiguration_extraDataPackageArn = Lens.lens (\ScheduleRunConfiguration' {extraDataPackageArn} -> extraDataPackageArn) (\s@ScheduleRunConfiguration' {} a -> s {extraDataPackageArn = a} :: ScheduleRunConfiguration)

instance Core.Hashable ScheduleRunConfiguration

instance Core.NFData ScheduleRunConfiguration

instance Core.ToJSON ScheduleRunConfiguration where
  toJSON ScheduleRunConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("locale" Core..=) Core.<$> locale,
            ("auxiliaryApps" Core..=) Core.<$> auxiliaryApps,
            ("radios" Core..=) Core.<$> radios,
            ("billingMethod" Core..=) Core.<$> billingMethod,
            ("customerArtifactPaths" Core..=)
              Core.<$> customerArtifactPaths,
            ("vpceConfigurationArns" Core..=)
              Core.<$> vpceConfigurationArns,
            ("networkProfileArn" Core..=)
              Core.<$> networkProfileArn,
            ("location" Core..=) Core.<$> location,
            ("extraDataPackageArn" Core..=)
              Core.<$> extraDataPackageArn
          ]
      )
