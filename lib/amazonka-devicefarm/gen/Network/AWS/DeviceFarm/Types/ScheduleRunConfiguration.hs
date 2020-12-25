{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
  ( ScheduleRunConfiguration (..),

    -- * Smart constructor
    mkScheduleRunConfiguration,

    -- * Lenses
    srcAuxiliaryApps,
    srcBillingMethod,
    srcCustomerArtifactPaths,
    srcExtraDataPackageArn,
    srcLocale,
    srcLocation,
    srcNetworkProfileArn,
    srcRadios,
    srcVpceConfigurationArns,
  )
where

import qualified Network.AWS.DeviceFarm.Types.AmazonResourceName as Types
import qualified Network.AWS.DeviceFarm.Types.BillingMethod as Types
import qualified Network.AWS.DeviceFarm.Types.CustomerArtifactPaths as Types
import qualified Network.AWS.DeviceFarm.Types.ExtraDataPackageArn as Types
import qualified Network.AWS.DeviceFarm.Types.Locale as Types
import qualified Network.AWS.DeviceFarm.Types.Location as Types
import qualified Network.AWS.DeviceFarm.Types.NetworkProfileArn as Types
import qualified Network.AWS.DeviceFarm.Types.Radios as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.
--
-- /See:/ 'mkScheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { -- | A list of upload ARNs for app packages to be installed with your app.
    auxiliaryApps :: Core.Maybe [Types.AmazonResourceName],
    -- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
    billingMethod :: Core.Maybe Types.BillingMethod,
    -- | Input @CustomerArtifactPaths@ object for the scheduled run configuration.
    customerArtifactPaths :: Core.Maybe Types.CustomerArtifactPaths,
    -- | The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm extracts to external data for Android or the app's sandbox for iOS.
    extraDataPackageArn :: Core.Maybe Types.ExtraDataPackageArn,
    -- | Information about the locale that is used for the run.
    locale :: Core.Maybe Types.Locale,
    -- | Information about the location that is used for the run.
    location :: Core.Maybe Types.Location,
    -- | Reserved for internal use.
    networkProfileArn :: Core.Maybe Types.NetworkProfileArn,
    -- | Information about the radio states for the run.
    radios :: Core.Maybe Types.Radios,
    -- | An array of ARNs for your VPC endpoint configurations.
    vpceConfigurationArns :: Core.Maybe [Types.AmazonResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduleRunConfiguration' value with any optional fields omitted.
mkScheduleRunConfiguration ::
  ScheduleRunConfiguration
mkScheduleRunConfiguration =
  ScheduleRunConfiguration'
    { auxiliaryApps = Core.Nothing,
      billingMethod = Core.Nothing,
      customerArtifactPaths = Core.Nothing,
      extraDataPackageArn = Core.Nothing,
      locale = Core.Nothing,
      location = Core.Nothing,
      networkProfileArn = Core.Nothing,
      radios = Core.Nothing,
      vpceConfigurationArns = Core.Nothing
    }

-- | A list of upload ARNs for app packages to be installed with your app.
--
-- /Note:/ Consider using 'auxiliaryApps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcAuxiliaryApps :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe [Types.AmazonResourceName])
srcAuxiliaryApps = Lens.field @"auxiliaryApps"
{-# DEPRECATED srcAuxiliaryApps "Use generic-lens or generic-optics with 'auxiliaryApps' instead." #-}

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcBillingMethod :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.BillingMethod)
srcBillingMethod = Lens.field @"billingMethod"
{-# DEPRECATED srcBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | Input @CustomerArtifactPaths@ object for the scheduled run configuration.
--
-- /Note:/ Consider using 'customerArtifactPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcCustomerArtifactPaths :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.CustomerArtifactPaths)
srcCustomerArtifactPaths = Lens.field @"customerArtifactPaths"
{-# DEPRECATED srcCustomerArtifactPaths "Use generic-lens or generic-optics with 'customerArtifactPaths' instead." #-}

-- | The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm extracts to external data for Android or the app's sandbox for iOS.
--
-- /Note:/ Consider using 'extraDataPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcExtraDataPackageArn :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.ExtraDataPackageArn)
srcExtraDataPackageArn = Lens.field @"extraDataPackageArn"
{-# DEPRECATED srcExtraDataPackageArn "Use generic-lens or generic-optics with 'extraDataPackageArn' instead." #-}

-- | Information about the locale that is used for the run.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLocale :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.Locale)
srcLocale = Lens.field @"locale"
{-# DEPRECATED srcLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | Information about the location that is used for the run.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLocation :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.Location)
srcLocation = Lens.field @"location"
{-# DEPRECATED srcLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Reserved for internal use.
--
-- /Note:/ Consider using 'networkProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcNetworkProfileArn :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.NetworkProfileArn)
srcNetworkProfileArn = Lens.field @"networkProfileArn"
{-# DEPRECATED srcNetworkProfileArn "Use generic-lens or generic-optics with 'networkProfileArn' instead." #-}

-- | Information about the radio states for the run.
--
-- /Note:/ Consider using 'radios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcRadios :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe Types.Radios)
srcRadios = Lens.field @"radios"
{-# DEPRECATED srcRadios "Use generic-lens or generic-optics with 'radios' instead." #-}

-- | An array of ARNs for your VPC endpoint configurations.
--
-- /Note:/ Consider using 'vpceConfigurationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcVpceConfigurationArns :: Lens.Lens' ScheduleRunConfiguration (Core.Maybe [Types.AmazonResourceName])
srcVpceConfigurationArns = Lens.field @"vpceConfigurationArns"
{-# DEPRECATED srcVpceConfigurationArns "Use generic-lens or generic-optics with 'vpceConfigurationArns' instead." #-}

instance Core.FromJSON ScheduleRunConfiguration where
  toJSON ScheduleRunConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("auxiliaryApps" Core..=) Core.<$> auxiliaryApps,
            ("billingMethod" Core..=) Core.<$> billingMethod,
            ("customerArtifactPaths" Core..=) Core.<$> customerArtifactPaths,
            ("extraDataPackageArn" Core..=) Core.<$> extraDataPackageArn,
            ("locale" Core..=) Core.<$> locale,
            ("location" Core..=) Core.<$> location,
            ("networkProfileArn" Core..=) Core.<$> networkProfileArn,
            ("radios" Core..=) Core.<$> radios,
            ("vpceConfigurationArns" Core..=) Core.<$> vpceConfigurationArns
          ]
      )
