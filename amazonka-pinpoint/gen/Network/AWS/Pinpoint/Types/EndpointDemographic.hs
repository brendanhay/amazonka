{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.EndpointDemographic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointDemographic where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies demographic information about an endpoint, such as the
-- applicable time zone and platform.
--
-- /See:/ 'newEndpointDemographic' smart constructor.
data EndpointDemographic = EndpointDemographic'
  { -- | The model name or number of the endpoint device, such as iPhone or
    -- SM-G900F.
    model :: Prelude.Maybe Prelude.Text,
    -- | The platform of the endpoint device, such as ios.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The version of the app that\'s associated with the endpoint.
    appVersion :: Prelude.Maybe Prelude.Text,
    -- | The locale of the endpoint, in the following format: the ISO 639-1
    -- alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1
    -- alpha-2 value.
    locale :: Prelude.Maybe Prelude.Text,
    -- | The platform version of the endpoint device.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | The model version of the endpoint device.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the endpoint, specified as a tz database name value,
    -- such as America\/Los_Angeles.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The manufacturer of the endpoint device, such as apple or samsung.
    make :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointDemographic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'model', 'endpointDemographic_model' - The model name or number of the endpoint device, such as iPhone or
-- SM-G900F.
--
-- 'platform', 'endpointDemographic_platform' - The platform of the endpoint device, such as ios.
--
-- 'appVersion', 'endpointDemographic_appVersion' - The version of the app that\'s associated with the endpoint.
--
-- 'locale', 'endpointDemographic_locale' - The locale of the endpoint, in the following format: the ISO 639-1
-- alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1
-- alpha-2 value.
--
-- 'platformVersion', 'endpointDemographic_platformVersion' - The platform version of the endpoint device.
--
-- 'modelVersion', 'endpointDemographic_modelVersion' - The model version of the endpoint device.
--
-- 'timezone', 'endpointDemographic_timezone' - The time zone of the endpoint, specified as a tz database name value,
-- such as America\/Los_Angeles.
--
-- 'make', 'endpointDemographic_make' - The manufacturer of the endpoint device, such as apple or samsung.
newEndpointDemographic ::
  EndpointDemographic
newEndpointDemographic =
  EndpointDemographic'
    { model = Prelude.Nothing,
      platform = Prelude.Nothing,
      appVersion = Prelude.Nothing,
      locale = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      timezone = Prelude.Nothing,
      make = Prelude.Nothing
    }

-- | The model name or number of the endpoint device, such as iPhone or
-- SM-G900F.
endpointDemographic_model :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_model = Lens.lens (\EndpointDemographic' {model} -> model) (\s@EndpointDemographic' {} a -> s {model = a} :: EndpointDemographic)

-- | The platform of the endpoint device, such as ios.
endpointDemographic_platform :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_platform = Lens.lens (\EndpointDemographic' {platform} -> platform) (\s@EndpointDemographic' {} a -> s {platform = a} :: EndpointDemographic)

-- | The version of the app that\'s associated with the endpoint.
endpointDemographic_appVersion :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_appVersion = Lens.lens (\EndpointDemographic' {appVersion} -> appVersion) (\s@EndpointDemographic' {} a -> s {appVersion = a} :: EndpointDemographic)

-- | The locale of the endpoint, in the following format: the ISO 639-1
-- alpha-2 code, followed by an underscore (_), followed by an ISO 3166-1
-- alpha-2 value.
endpointDemographic_locale :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_locale = Lens.lens (\EndpointDemographic' {locale} -> locale) (\s@EndpointDemographic' {} a -> s {locale = a} :: EndpointDemographic)

-- | The platform version of the endpoint device.
endpointDemographic_platformVersion :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_platformVersion = Lens.lens (\EndpointDemographic' {platformVersion} -> platformVersion) (\s@EndpointDemographic' {} a -> s {platformVersion = a} :: EndpointDemographic)

-- | The model version of the endpoint device.
endpointDemographic_modelVersion :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_modelVersion = Lens.lens (\EndpointDemographic' {modelVersion} -> modelVersion) (\s@EndpointDemographic' {} a -> s {modelVersion = a} :: EndpointDemographic)

-- | The time zone of the endpoint, specified as a tz database name value,
-- such as America\/Los_Angeles.
endpointDemographic_timezone :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_timezone = Lens.lens (\EndpointDemographic' {timezone} -> timezone) (\s@EndpointDemographic' {} a -> s {timezone = a} :: EndpointDemographic)

-- | The manufacturer of the endpoint device, such as apple or samsung.
endpointDemographic_make :: Lens.Lens' EndpointDemographic (Prelude.Maybe Prelude.Text)
endpointDemographic_make = Lens.lens (\EndpointDemographic' {make} -> make) (\s@EndpointDemographic' {} a -> s {make = a} :: EndpointDemographic)

instance Prelude.FromJSON EndpointDemographic where
  parseJSON =
    Prelude.withObject
      "EndpointDemographic"
      ( \x ->
          EndpointDemographic'
            Prelude.<$> (x Prelude..:? "Model")
            Prelude.<*> (x Prelude..:? "Platform")
            Prelude.<*> (x Prelude..:? "AppVersion")
            Prelude.<*> (x Prelude..:? "Locale")
            Prelude.<*> (x Prelude..:? "PlatformVersion")
            Prelude.<*> (x Prelude..:? "ModelVersion")
            Prelude.<*> (x Prelude..:? "Timezone")
            Prelude.<*> (x Prelude..:? "Make")
      )

instance Prelude.Hashable EndpointDemographic

instance Prelude.NFData EndpointDemographic

instance Prelude.ToJSON EndpointDemographic where
  toJSON EndpointDemographic' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Model" Prelude..=) Prelude.<$> model,
            ("Platform" Prelude..=) Prelude.<$> platform,
            ("AppVersion" Prelude..=) Prelude.<$> appVersion,
            ("Locale" Prelude..=) Prelude.<$> locale,
            ("PlatformVersion" Prelude..=)
              Prelude.<$> platformVersion,
            ("ModelVersion" Prelude..=) Prelude.<$> modelVersion,
            ("Timezone" Prelude..=) Prelude.<$> timezone,
            ("Make" Prelude..=) Prelude.<$> make
          ]
      )
