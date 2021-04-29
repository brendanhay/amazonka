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
-- Module      : Network.AWS.MediaLive.Types.StandardHlsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StandardHlsSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M3u8Settings
import qualified Network.AWS.Prelude as Prelude

-- | Standard Hls Settings
--
-- /See:/ 'newStandardHlsSettings' smart constructor.
data StandardHlsSettings = StandardHlsSettings'
  { -- | List all the audio groups that are used with the video output stream.
    -- Input all the audio GROUP-IDs that are associated to the video, separate
    -- by \',\'.
    audioRenditionSets :: Prelude.Maybe Prelude.Text,
    m3u8Settings :: M3u8Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StandardHlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioRenditionSets', 'standardHlsSettings_audioRenditionSets' - List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
--
-- 'm3u8Settings', 'standardHlsSettings_m3u8Settings' - Undocumented member.
newStandardHlsSettings ::
  -- | 'm3u8Settings'
  M3u8Settings ->
  StandardHlsSettings
newStandardHlsSettings pM3u8Settings_ =
  StandardHlsSettings'
    { audioRenditionSets =
        Prelude.Nothing,
      m3u8Settings = pM3u8Settings_
    }

-- | List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
standardHlsSettings_audioRenditionSets :: Lens.Lens' StandardHlsSettings (Prelude.Maybe Prelude.Text)
standardHlsSettings_audioRenditionSets = Lens.lens (\StandardHlsSettings' {audioRenditionSets} -> audioRenditionSets) (\s@StandardHlsSettings' {} a -> s {audioRenditionSets = a} :: StandardHlsSettings)

-- | Undocumented member.
standardHlsSettings_m3u8Settings :: Lens.Lens' StandardHlsSettings M3u8Settings
standardHlsSettings_m3u8Settings = Lens.lens (\StandardHlsSettings' {m3u8Settings} -> m3u8Settings) (\s@StandardHlsSettings' {} a -> s {m3u8Settings = a} :: StandardHlsSettings)

instance Prelude.FromJSON StandardHlsSettings where
  parseJSON =
    Prelude.withObject
      "StandardHlsSettings"
      ( \x ->
          StandardHlsSettings'
            Prelude.<$> (x Prelude..:? "audioRenditionSets")
            Prelude.<*> (x Prelude..: "m3u8Settings")
      )

instance Prelude.Hashable StandardHlsSettings

instance Prelude.NFData StandardHlsSettings

instance Prelude.ToJSON StandardHlsSettings where
  toJSON StandardHlsSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("audioRenditionSets" Prelude..=)
              Prelude.<$> audioRenditionSets,
            Prelude.Just
              ("m3u8Settings" Prelude..= m3u8Settings)
          ]
      )
