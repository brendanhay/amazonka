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
-- Module      : Amazonka.MediaConvert.Types.MxfSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.MxfAfdSignaling
import Amazonka.MediaConvert.Types.MxfProfile
import Amazonka.MediaConvert.Types.MxfXavcProfileSettings
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to your MXF output container.
--
-- /See:/ 'newMxfSettings' smart constructor.
data MxfSettings = MxfSettings'
  { -- | Optional. When you have AFD signaling set up in your output video
    -- stream, use this setting to choose whether to also include it in the MXF
    -- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
    -- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
    -- AFD values from the video stream for this output to the MXF wrapper.
    -- Regardless of which option you choose, the AFD values remain in the
    -- video stream. Related settings: To set up your output to include or
    -- exclude AFD values, see AfdSignaling, under VideoDescription. On the
    -- console, find AFD signaling under the output\'s video encoding settings.
    afdSignaling :: Prelude.Maybe MxfAfdSignaling,
    -- | Specify the MXF profile, also called shim, for this output. When you
    -- choose Auto, MediaConvert chooses a profile based on the video codec and
    -- resolution. For a list of codecs supported with each MXF profile, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
    -- For more information about the automatic selection behavior, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
    profile :: Prelude.Maybe MxfProfile,
    -- | Specify the XAVC profile settings for MXF outputs when you set your MXF
    -- profile to XAVC.
    xavcProfileSettings :: Prelude.Maybe MxfXavcProfileSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MxfSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'afdSignaling', 'mxfSettings_afdSignaling' - Optional. When you have AFD signaling set up in your output video
-- stream, use this setting to choose whether to also include it in the MXF
-- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
-- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
-- AFD values from the video stream for this output to the MXF wrapper.
-- Regardless of which option you choose, the AFD values remain in the
-- video stream. Related settings: To set up your output to include or
-- exclude AFD values, see AfdSignaling, under VideoDescription. On the
-- console, find AFD signaling under the output\'s video encoding settings.
--
-- 'profile', 'mxfSettings_profile' - Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
--
-- 'xavcProfileSettings', 'mxfSettings_xavcProfileSettings' - Specify the XAVC profile settings for MXF outputs when you set your MXF
-- profile to XAVC.
newMxfSettings ::
  MxfSettings
newMxfSettings =
  MxfSettings'
    { afdSignaling = Prelude.Nothing,
      profile = Prelude.Nothing,
      xavcProfileSettings = Prelude.Nothing
    }

-- | Optional. When you have AFD signaling set up in your output video
-- stream, use this setting to choose whether to also include it in the MXF
-- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
-- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
-- AFD values from the video stream for this output to the MXF wrapper.
-- Regardless of which option you choose, the AFD values remain in the
-- video stream. Related settings: To set up your output to include or
-- exclude AFD values, see AfdSignaling, under VideoDescription. On the
-- console, find AFD signaling under the output\'s video encoding settings.
mxfSettings_afdSignaling :: Lens.Lens' MxfSettings (Prelude.Maybe MxfAfdSignaling)
mxfSettings_afdSignaling = Lens.lens (\MxfSettings' {afdSignaling} -> afdSignaling) (\s@MxfSettings' {} a -> s {afdSignaling = a} :: MxfSettings)

-- | Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
mxfSettings_profile :: Lens.Lens' MxfSettings (Prelude.Maybe MxfProfile)
mxfSettings_profile = Lens.lens (\MxfSettings' {profile} -> profile) (\s@MxfSettings' {} a -> s {profile = a} :: MxfSettings)

-- | Specify the XAVC profile settings for MXF outputs when you set your MXF
-- profile to XAVC.
mxfSettings_xavcProfileSettings :: Lens.Lens' MxfSettings (Prelude.Maybe MxfXavcProfileSettings)
mxfSettings_xavcProfileSettings = Lens.lens (\MxfSettings' {xavcProfileSettings} -> xavcProfileSettings) (\s@MxfSettings' {} a -> s {xavcProfileSettings = a} :: MxfSettings)

instance Data.FromJSON MxfSettings where
  parseJSON =
    Data.withObject
      "MxfSettings"
      ( \x ->
          MxfSettings'
            Prelude.<$> (x Data..:? "afdSignaling")
            Prelude.<*> (x Data..:? "profile")
            Prelude.<*> (x Data..:? "xavcProfileSettings")
      )

instance Prelude.Hashable MxfSettings where
  hashWithSalt _salt MxfSettings' {..} =
    _salt
      `Prelude.hashWithSalt` afdSignaling
      `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` xavcProfileSettings

instance Prelude.NFData MxfSettings where
  rnf MxfSettings' {..} =
    Prelude.rnf afdSignaling
      `Prelude.seq` Prelude.rnf profile
      `Prelude.seq` Prelude.rnf xavcProfileSettings

instance Data.ToJSON MxfSettings where
  toJSON MxfSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("afdSignaling" Data..=) Prelude.<$> afdSignaling,
            ("profile" Data..=) Prelude.<$> profile,
            ("xavcProfileSettings" Data..=)
              Prelude.<$> xavcProfileSettings
          ]
      )
