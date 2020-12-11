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
    srcBillingMethod,
    srcCustomerArtifactPaths,
    srcRadios,
    srcLocation,
    srcLocale,
    srcNetworkProfileARN,
    srcExtraDataPackageARN,
    srcAuxiliaryApps,
    srcVpceConfigurationARNs,
  )
where

import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.Radios
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings for a run. Includes things like location, radio states, auxiliary apps, and network profiles.
--
-- /See:/ 'mkScheduleRunConfiguration' smart constructor.
data ScheduleRunConfiguration = ScheduleRunConfiguration'
  { billingMethod ::
      Lude.Maybe BillingMethod,
    customerArtifactPaths ::
      Lude.Maybe CustomerArtifactPaths,
    radios :: Lude.Maybe Radios,
    location :: Lude.Maybe Location,
    locale :: Lude.Maybe Lude.Text,
    networkProfileARN :: Lude.Maybe Lude.Text,
    extraDataPackageARN ::
      Lude.Maybe Lude.Text,
    auxiliaryApps :: Lude.Maybe [Lude.Text],
    vpceConfigurationARNs ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleRunConfiguration' with the minimum fields required to make a request.
--
-- * 'auxiliaryApps' - A list of upload ARNs for app packages to be installed with your app.
-- * 'billingMethod' - Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
-- * 'customerArtifactPaths' - Input @CustomerArtifactPaths@ object for the scheduled run configuration.
-- * 'extraDataPackageARN' - The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm extracts to external data for Android or the app's sandbox for iOS.
-- * 'locale' - Information about the locale that is used for the run.
-- * 'location' - Information about the location that is used for the run.
-- * 'networkProfileARN' - Reserved for internal use.
-- * 'radios' - Information about the radio states for the run.
-- * 'vpceConfigurationARNs' - An array of ARNs for your VPC endpoint configurations.
mkScheduleRunConfiguration ::
  ScheduleRunConfiguration
mkScheduleRunConfiguration =
  ScheduleRunConfiguration'
    { billingMethod = Lude.Nothing,
      customerArtifactPaths = Lude.Nothing,
      radios = Lude.Nothing,
      location = Lude.Nothing,
      locale = Lude.Nothing,
      networkProfileARN = Lude.Nothing,
      extraDataPackageARN = Lude.Nothing,
      auxiliaryApps = Lude.Nothing,
      vpceConfigurationARNs = Lude.Nothing
    }

-- | Specifies the billing method for a test run: @metered@ or @unmetered@ . If the parameter is not specified, the default value is @metered@ .
--
-- /Note:/ Consider using 'billingMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcBillingMethod :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe BillingMethod)
srcBillingMethod = Lens.lens (billingMethod :: ScheduleRunConfiguration -> Lude.Maybe BillingMethod) (\s a -> s {billingMethod = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcBillingMethod "Use generic-lens or generic-optics with 'billingMethod' instead." #-}

-- | Input @CustomerArtifactPaths@ object for the scheduled run configuration.
--
-- /Note:/ Consider using 'customerArtifactPaths' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcCustomerArtifactPaths :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe CustomerArtifactPaths)
srcCustomerArtifactPaths = Lens.lens (customerArtifactPaths :: ScheduleRunConfiguration -> Lude.Maybe CustomerArtifactPaths) (\s a -> s {customerArtifactPaths = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcCustomerArtifactPaths "Use generic-lens or generic-optics with 'customerArtifactPaths' instead." #-}

-- | Information about the radio states for the run.
--
-- /Note:/ Consider using 'radios' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcRadios :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe Radios)
srcRadios = Lens.lens (radios :: ScheduleRunConfiguration -> Lude.Maybe Radios) (\s a -> s {radios = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcRadios "Use generic-lens or generic-optics with 'radios' instead." #-}

-- | Information about the location that is used for the run.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLocation :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe Location)
srcLocation = Lens.lens (location :: ScheduleRunConfiguration -> Lude.Maybe Location) (\s a -> s {location = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Information about the locale that is used for the run.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcLocale :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe Lude.Text)
srcLocale = Lens.lens (locale :: ScheduleRunConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | Reserved for internal use.
--
-- /Note:/ Consider using 'networkProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcNetworkProfileARN :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe Lude.Text)
srcNetworkProfileARN = Lens.lens (networkProfileARN :: ScheduleRunConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {networkProfileARN = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcNetworkProfileARN "Use generic-lens or generic-optics with 'networkProfileARN' instead." #-}

-- | The ARN of the extra data for the run. The extra data is a .zip file that AWS Device Farm extracts to external data for Android or the app's sandbox for iOS.
--
-- /Note:/ Consider using 'extraDataPackageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcExtraDataPackageARN :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe Lude.Text)
srcExtraDataPackageARN = Lens.lens (extraDataPackageARN :: ScheduleRunConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {extraDataPackageARN = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcExtraDataPackageARN "Use generic-lens or generic-optics with 'extraDataPackageARN' instead." #-}

-- | A list of upload ARNs for app packages to be installed with your app.
--
-- /Note:/ Consider using 'auxiliaryApps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcAuxiliaryApps :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe [Lude.Text])
srcAuxiliaryApps = Lens.lens (auxiliaryApps :: ScheduleRunConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {auxiliaryApps = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcAuxiliaryApps "Use generic-lens or generic-optics with 'auxiliaryApps' instead." #-}

-- | An array of ARNs for your VPC endpoint configurations.
--
-- /Note:/ Consider using 'vpceConfigurationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcVpceConfigurationARNs :: Lens.Lens' ScheduleRunConfiguration (Lude.Maybe [Lude.Text])
srcVpceConfigurationARNs = Lens.lens (vpceConfigurationARNs :: ScheduleRunConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {vpceConfigurationARNs = a} :: ScheduleRunConfiguration)
{-# DEPRECATED srcVpceConfigurationARNs "Use generic-lens or generic-optics with 'vpceConfigurationARNs' instead." #-}

instance Lude.ToJSON ScheduleRunConfiguration where
  toJSON ScheduleRunConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("billingMethod" Lude..=) Lude.<$> billingMethod,
            ("customerArtifactPaths" Lude..=) Lude.<$> customerArtifactPaths,
            ("radios" Lude..=) Lude.<$> radios,
            ("location" Lude..=) Lude.<$> location,
            ("locale" Lude..=) Lude.<$> locale,
            ("networkProfileArn" Lude..=) Lude.<$> networkProfileARN,
            ("extraDataPackageArn" Lude..=) Lude.<$> extraDataPackageARN,
            ("auxiliaryApps" Lude..=) Lude.<$> auxiliaryApps,
            ("vpceConfigurationArns" Lude..=) Lude.<$> vpceConfigurationARNs
          ]
      )
