-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointDemographic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointDemographic
  ( EndpointDemographic (..),

    -- * Smart constructor
    mkEndpointDemographic,

    -- * Lenses
    edPlatform,
    edPlatformVersion,
    edLocale,
    edAppVersion,
    edModel,
    edMake,
    edModelVersion,
    edTimezone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies demographic information about an endpoint, such as the applicable time zone and platform.
--
-- /See:/ 'mkEndpointDemographic' smart constructor.
data EndpointDemographic = EndpointDemographic'
  { platform ::
      Lude.Maybe Lude.Text,
    platformVersion :: Lude.Maybe Lude.Text,
    locale :: Lude.Maybe Lude.Text,
    appVersion :: Lude.Maybe Lude.Text,
    model :: Lude.Maybe Lude.Text,
    make :: Lude.Maybe Lude.Text,
    modelVersion :: Lude.Maybe Lude.Text,
    timezone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EndpointDemographic' with the minimum fields required to make a request.
--
-- * 'appVersion' - The version of the app that's associated with the endpoint.
-- * 'locale' - The locale of the endpoint, in the following format: the ISO 639-1 alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1 alpha-2 value.
-- * 'make' - The manufacturer of the endpoint device, such as apple or samsung.
-- * 'model' - The model name or number of the endpoint device, such as iPhone or SM-G900F.
-- * 'modelVersion' - The model version of the endpoint device.
-- * 'platform' - The platform of the endpoint device, such as ios.
-- * 'platformVersion' - The platform version of the endpoint device.
-- * 'timezone' - The time zone of the endpoint, specified as a tz database name value, such as America/Los_Angeles.
mkEndpointDemographic ::
  EndpointDemographic
mkEndpointDemographic =
  EndpointDemographic'
    { platform = Lude.Nothing,
      platformVersion = Lude.Nothing,
      locale = Lude.Nothing,
      appVersion = Lude.Nothing,
      model = Lude.Nothing,
      make = Lude.Nothing,
      modelVersion = Lude.Nothing,
      timezone = Lude.Nothing
    }

-- | The platform of the endpoint device, such as ios.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatform :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edPlatform = Lens.lens (platform :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {platform = a} :: EndpointDemographic)
{-# DEPRECATED edPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The platform version of the endpoint device.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatformVersion :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edPlatformVersion = Lens.lens (platformVersion :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {platformVersion = a} :: EndpointDemographic)
{-# DEPRECATED edPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The locale of the endpoint, in the following format: the ISO 639-1 alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1 alpha-2 value.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLocale :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edLocale = Lens.lens (locale :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {locale = a} :: EndpointDemographic)
{-# DEPRECATED edLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The version of the app that's associated with the endpoint.
--
-- /Note:/ Consider using 'appVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edAppVersion :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edAppVersion = Lens.lens (appVersion :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {appVersion = a} :: EndpointDemographic)
{-# DEPRECATED edAppVersion "Use generic-lens or generic-optics with 'appVersion' instead." #-}

-- | The model name or number of the endpoint device, such as iPhone or SM-G900F.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edModel :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edModel = Lens.lens (model :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {model = a} :: EndpointDemographic)
{-# DEPRECATED edModel "Use generic-lens or generic-optics with 'model' instead." #-}

-- | The manufacturer of the endpoint device, such as apple or samsung.
--
-- /Note:/ Consider using 'make' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMake :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edMake = Lens.lens (make :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {make = a} :: EndpointDemographic)
{-# DEPRECATED edMake "Use generic-lens or generic-optics with 'make' instead." #-}

-- | The model version of the endpoint device.
--
-- /Note:/ Consider using 'modelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edModelVersion :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edModelVersion = Lens.lens (modelVersion :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {modelVersion = a} :: EndpointDemographic)
{-# DEPRECATED edModelVersion "Use generic-lens or generic-optics with 'modelVersion' instead." #-}

-- | The time zone of the endpoint, specified as a tz database name value, such as America/Los_Angeles.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTimezone :: Lens.Lens' EndpointDemographic (Lude.Maybe Lude.Text)
edTimezone = Lens.lens (timezone :: EndpointDemographic -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: EndpointDemographic)
{-# DEPRECATED edTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

instance Lude.FromJSON EndpointDemographic where
  parseJSON =
    Lude.withObject
      "EndpointDemographic"
      ( \x ->
          EndpointDemographic'
            Lude.<$> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "PlatformVersion")
            Lude.<*> (x Lude..:? "Locale")
            Lude.<*> (x Lude..:? "AppVersion")
            Lude.<*> (x Lude..:? "Model")
            Lude.<*> (x Lude..:? "Make")
            Lude.<*> (x Lude..:? "ModelVersion")
            Lude.<*> (x Lude..:? "Timezone")
      )

instance Lude.ToJSON EndpointDemographic where
  toJSON EndpointDemographic' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Platform" Lude..=) Lude.<$> platform,
            ("PlatformVersion" Lude..=) Lude.<$> platformVersion,
            ("Locale" Lude..=) Lude.<$> locale,
            ("AppVersion" Lude..=) Lude.<$> appVersion,
            ("Model" Lude..=) Lude.<$> model,
            ("Make" Lude..=) Lude.<$> make,
            ("ModelVersion" Lude..=) Lude.<$> modelVersion,
            ("Timezone" Lude..=) Lude.<$> timezone
          ]
      )
