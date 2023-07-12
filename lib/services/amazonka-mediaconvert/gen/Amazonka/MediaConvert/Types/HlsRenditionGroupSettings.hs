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
-- Module      : Amazonka.MediaConvert.Types.HlsRenditionGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsRenditionGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | Settings specific to audio sources in an HLS alternate rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique audio track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the properties provided, the
-- job fails. If no properties in hlsRenditionGroupSettings are specified,
-- the default audio track within the video segment is chosen. If there is
-- no audio within video segment, the alternative audio with DEFAULT=YES is
-- chosen instead.
--
-- /See:/ 'newHlsRenditionGroupSettings' smart constructor.
data HlsRenditionGroupSettings = HlsRenditionGroupSettings'
  { -- | Optional. Specify alternative group ID
    renditionGroupId :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
    renditionLanguageCode :: Prelude.Maybe LanguageCode,
    -- | Optional. Specify media name
    renditionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsRenditionGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renditionGroupId', 'hlsRenditionGroupSettings_renditionGroupId' - Optional. Specify alternative group ID
--
-- 'renditionLanguageCode', 'hlsRenditionGroupSettings_renditionLanguageCode' - Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
--
-- 'renditionName', 'hlsRenditionGroupSettings_renditionName' - Optional. Specify media name
newHlsRenditionGroupSettings ::
  HlsRenditionGroupSettings
newHlsRenditionGroupSettings =
  HlsRenditionGroupSettings'
    { renditionGroupId =
        Prelude.Nothing,
      renditionLanguageCode = Prelude.Nothing,
      renditionName = Prelude.Nothing
    }

-- | Optional. Specify alternative group ID
hlsRenditionGroupSettings_renditionGroupId :: Lens.Lens' HlsRenditionGroupSettings (Prelude.Maybe Prelude.Text)
hlsRenditionGroupSettings_renditionGroupId = Lens.lens (\HlsRenditionGroupSettings' {renditionGroupId} -> renditionGroupId) (\s@HlsRenditionGroupSettings' {} a -> s {renditionGroupId = a} :: HlsRenditionGroupSettings)

-- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
hlsRenditionGroupSettings_renditionLanguageCode :: Lens.Lens' HlsRenditionGroupSettings (Prelude.Maybe LanguageCode)
hlsRenditionGroupSettings_renditionLanguageCode = Lens.lens (\HlsRenditionGroupSettings' {renditionLanguageCode} -> renditionLanguageCode) (\s@HlsRenditionGroupSettings' {} a -> s {renditionLanguageCode = a} :: HlsRenditionGroupSettings)

-- | Optional. Specify media name
hlsRenditionGroupSettings_renditionName :: Lens.Lens' HlsRenditionGroupSettings (Prelude.Maybe Prelude.Text)
hlsRenditionGroupSettings_renditionName = Lens.lens (\HlsRenditionGroupSettings' {renditionName} -> renditionName) (\s@HlsRenditionGroupSettings' {} a -> s {renditionName = a} :: HlsRenditionGroupSettings)

instance Data.FromJSON HlsRenditionGroupSettings where
  parseJSON =
    Data.withObject
      "HlsRenditionGroupSettings"
      ( \x ->
          HlsRenditionGroupSettings'
            Prelude.<$> (x Data..:? "renditionGroupId")
            Prelude.<*> (x Data..:? "renditionLanguageCode")
            Prelude.<*> (x Data..:? "renditionName")
      )

instance Prelude.Hashable HlsRenditionGroupSettings where
  hashWithSalt _salt HlsRenditionGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` renditionGroupId
      `Prelude.hashWithSalt` renditionLanguageCode
      `Prelude.hashWithSalt` renditionName

instance Prelude.NFData HlsRenditionGroupSettings where
  rnf HlsRenditionGroupSettings' {..} =
    Prelude.rnf renditionGroupId
      `Prelude.seq` Prelude.rnf renditionLanguageCode
      `Prelude.seq` Prelude.rnf renditionName

instance Data.ToJSON HlsRenditionGroupSettings where
  toJSON HlsRenditionGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("renditionGroupId" Data..=)
              Prelude.<$> renditionGroupId,
            ("renditionLanguageCode" Data..=)
              Prelude.<$> renditionLanguageCode,
            ("renditionName" Data..=) Prelude.<$> renditionName
          ]
      )
