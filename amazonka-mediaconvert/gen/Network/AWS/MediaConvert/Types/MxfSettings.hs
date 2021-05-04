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
-- Module      : Network.AWS.MediaConvert.Types.MxfSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MxfAfdSignaling
import Network.AWS.MediaConvert.Types.MxfProfile
import qualified Network.AWS.Prelude as Prelude

-- | MXF settings
--
-- /See:/ 'newMxfSettings' smart constructor.
data MxfSettings = MxfSettings'
  { -- | Specify the MXF profile, also called shim, for this output. When you
    -- choose Auto, MediaConvert chooses a profile based on the video codec and
    -- resolution. For a list of codecs supported with each MXF profile, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
    -- For more information about the automatic selection behavior, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
    profile :: Prelude.Maybe MxfProfile,
    -- | Optional. When you have AFD signaling set up in your output video
    -- stream, use this setting to choose whether to also include it in the MXF
    -- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
    -- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
    -- AFD values from the video stream for this output to the MXF wrapper.
    -- Regardless of which option you choose, the AFD values remain in the
    -- video stream. Related settings: To set up your output to include or
    -- exclude AFD values, see AfdSignaling, under VideoDescription. On the
    -- console, find AFD signaling under the output\'s video encoding settings.
    afdSignaling :: Prelude.Maybe MxfAfdSignaling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MxfSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profile', 'mxfSettings_profile' - Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
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
newMxfSettings ::
  MxfSettings
newMxfSettings =
  MxfSettings'
    { profile = Prelude.Nothing,
      afdSignaling = Prelude.Nothing
    }

-- | Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
mxfSettings_profile :: Lens.Lens' MxfSettings (Prelude.Maybe MxfProfile)
mxfSettings_profile = Lens.lens (\MxfSettings' {profile} -> profile) (\s@MxfSettings' {} a -> s {profile = a} :: MxfSettings)

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

instance Prelude.FromJSON MxfSettings where
  parseJSON =
    Prelude.withObject
      "MxfSettings"
      ( \x ->
          MxfSettings'
            Prelude.<$> (x Prelude..:? "profile")
            Prelude.<*> (x Prelude..:? "afdSignaling")
      )

instance Prelude.Hashable MxfSettings

instance Prelude.NFData MxfSettings

instance Prelude.ToJSON MxfSettings where
  toJSON MxfSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("profile" Prelude..=) Prelude.<$> profile,
            ("afdSignaling" Prelude..=)
              Prelude.<$> afdSignaling
          ]
      )
