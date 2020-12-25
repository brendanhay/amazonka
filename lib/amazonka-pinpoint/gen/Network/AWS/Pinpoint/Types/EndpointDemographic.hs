{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    edAppVersion,
    edLocale,
    edMake,
    edModel,
    edModelVersion,
    edPlatform,
    edPlatformVersion,
    edTimezone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies demographic information about an endpoint, such as the applicable time zone and platform.
--
-- /See:/ 'mkEndpointDemographic' smart constructor.
data EndpointDemographic = EndpointDemographic'
  { -- | The version of the app that's associated with the endpoint.
    appVersion :: Core.Maybe Core.Text,
    -- | The locale of the endpoint, in the following format: the ISO 639-1 alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1 alpha-2 value.
    locale :: Core.Maybe Core.Text,
    -- | The manufacturer of the endpoint device, such as apple or samsung.
    make :: Core.Maybe Core.Text,
    -- | The model name or number of the endpoint device, such as iPhone or SM-G900F.
    model :: Core.Maybe Core.Text,
    -- | The model version of the endpoint device.
    modelVersion :: Core.Maybe Core.Text,
    -- | The platform of the endpoint device, such as ios.
    platform :: Core.Maybe Core.Text,
    -- | The platform version of the endpoint device.
    platformVersion :: Core.Maybe Core.Text,
    -- | The time zone of the endpoint, specified as a tz database name value, such as America/Los_Angeles.
    timezone :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointDemographic' value with any optional fields omitted.
mkEndpointDemographic ::
  EndpointDemographic
mkEndpointDemographic =
  EndpointDemographic'
    { appVersion = Core.Nothing,
      locale = Core.Nothing,
      make = Core.Nothing,
      model = Core.Nothing,
      modelVersion = Core.Nothing,
      platform = Core.Nothing,
      platformVersion = Core.Nothing,
      timezone = Core.Nothing
    }

-- | The version of the app that's associated with the endpoint.
--
-- /Note:/ Consider using 'appVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edAppVersion :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edAppVersion = Lens.field @"appVersion"
{-# DEPRECATED edAppVersion "Use generic-lens or generic-optics with 'appVersion' instead." #-}

-- | The locale of the endpoint, in the following format: the ISO 639-1 alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1 alpha-2 value.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edLocale :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edLocale = Lens.field @"locale"
{-# DEPRECATED edLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

-- | The manufacturer of the endpoint device, such as apple or samsung.
--
-- /Note:/ Consider using 'make' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMake :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edMake = Lens.field @"make"
{-# DEPRECATED edMake "Use generic-lens or generic-optics with 'make' instead." #-}

-- | The model name or number of the endpoint device, such as iPhone or SM-G900F.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edModel :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edModel = Lens.field @"model"
{-# DEPRECATED edModel "Use generic-lens or generic-optics with 'model' instead." #-}

-- | The model version of the endpoint device.
--
-- /Note:/ Consider using 'modelVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edModelVersion :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edModelVersion = Lens.field @"modelVersion"
{-# DEPRECATED edModelVersion "Use generic-lens or generic-optics with 'modelVersion' instead." #-}

-- | The platform of the endpoint device, such as ios.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatform :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edPlatform = Lens.field @"platform"
{-# DEPRECATED edPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The platform version of the endpoint device.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPlatformVersion :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edPlatformVersion = Lens.field @"platformVersion"
{-# DEPRECATED edPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The time zone of the endpoint, specified as a tz database name value, such as America/Los_Angeles.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edTimezone :: Lens.Lens' EndpointDemographic (Core.Maybe Core.Text)
edTimezone = Lens.field @"timezone"
{-# DEPRECATED edTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

instance Core.FromJSON EndpointDemographic where
  toJSON EndpointDemographic {..} =
    Core.object
      ( Core.catMaybes
          [ ("AppVersion" Core..=) Core.<$> appVersion,
            ("Locale" Core..=) Core.<$> locale,
            ("Make" Core..=) Core.<$> make,
            ("Model" Core..=) Core.<$> model,
            ("ModelVersion" Core..=) Core.<$> modelVersion,
            ("Platform" Core..=) Core.<$> platform,
            ("PlatformVersion" Core..=) Core.<$> platformVersion,
            ("Timezone" Core..=) Core.<$> timezone
          ]
      )

instance Core.FromJSON EndpointDemographic where
  parseJSON =
    Core.withObject "EndpointDemographic" Core.$
      \x ->
        EndpointDemographic'
          Core.<$> (x Core..:? "AppVersion")
          Core.<*> (x Core..:? "Locale")
          Core.<*> (x Core..:? "Make")
          Core.<*> (x Core..:? "Model")
          Core.<*> (x Core..:? "ModelVersion")
          Core.<*> (x Core..:? "Platform")
          Core.<*> (x Core..:? "PlatformVersion")
          Core.<*> (x Core..:? "Timezone")
