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
-- Module      : Network.AWS.MediaConvert.Types.WebvttHlsSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WebvttHlsSourceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.LanguageCode
import qualified Network.AWS.Prelude as Prelude

-- | Settings specific to WebVTT sources in HLS alternative rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique subtitle track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the specified properties, the
-- job fails. If there is only one subtitle track in the rendition group,
-- the settings can be left empty and the default subtitle track will be
-- chosen. If your caption source is a sidecar file, use FileSourceSettings
-- instead of WebvttHlsSourceSettings.
--
-- /See:/ 'newWebvttHlsSourceSettings' smart constructor.
data WebvttHlsSourceSettings = WebvttHlsSourceSettings'
  { -- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
    renditionLanguageCode :: Prelude.Maybe LanguageCode,
    -- | Optional. Specify alternative group ID
    renditionGroupId :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify media name
    renditionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebvttHlsSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renditionLanguageCode', 'webvttHlsSourceSettings_renditionLanguageCode' - Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
--
-- 'renditionGroupId', 'webvttHlsSourceSettings_renditionGroupId' - Optional. Specify alternative group ID
--
-- 'renditionName', 'webvttHlsSourceSettings_renditionName' - Optional. Specify media name
newWebvttHlsSourceSettings ::
  WebvttHlsSourceSettings
newWebvttHlsSourceSettings =
  WebvttHlsSourceSettings'
    { renditionLanguageCode =
        Prelude.Nothing,
      renditionGroupId = Prelude.Nothing,
      renditionName = Prelude.Nothing
    }

-- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
webvttHlsSourceSettings_renditionLanguageCode :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe LanguageCode)
webvttHlsSourceSettings_renditionLanguageCode = Lens.lens (\WebvttHlsSourceSettings' {renditionLanguageCode} -> renditionLanguageCode) (\s@WebvttHlsSourceSettings' {} a -> s {renditionLanguageCode = a} :: WebvttHlsSourceSettings)

-- | Optional. Specify alternative group ID
webvttHlsSourceSettings_renditionGroupId :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe Prelude.Text)
webvttHlsSourceSettings_renditionGroupId = Lens.lens (\WebvttHlsSourceSettings' {renditionGroupId} -> renditionGroupId) (\s@WebvttHlsSourceSettings' {} a -> s {renditionGroupId = a} :: WebvttHlsSourceSettings)

-- | Optional. Specify media name
webvttHlsSourceSettings_renditionName :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe Prelude.Text)
webvttHlsSourceSettings_renditionName = Lens.lens (\WebvttHlsSourceSettings' {renditionName} -> renditionName) (\s@WebvttHlsSourceSettings' {} a -> s {renditionName = a} :: WebvttHlsSourceSettings)

instance Core.FromJSON WebvttHlsSourceSettings where
  parseJSON =
    Core.withObject
      "WebvttHlsSourceSettings"
      ( \x ->
          WebvttHlsSourceSettings'
            Prelude.<$> (x Core..:? "renditionLanguageCode")
            Prelude.<*> (x Core..:? "renditionGroupId")
            Prelude.<*> (x Core..:? "renditionName")
      )

instance Prelude.Hashable WebvttHlsSourceSettings

instance Prelude.NFData WebvttHlsSourceSettings

instance Core.ToJSON WebvttHlsSourceSettings where
  toJSON WebvttHlsSourceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("renditionLanguageCode" Core..=)
              Prelude.<$> renditionLanguageCode,
            ("renditionGroupId" Core..=)
              Prelude.<$> renditionGroupId,
            ("renditionName" Core..=) Prelude.<$> renditionName
          ]
      )
