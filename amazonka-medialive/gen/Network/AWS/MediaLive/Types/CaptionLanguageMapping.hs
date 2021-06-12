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
-- Module      : Network.AWS.MediaLive.Types.CaptionLanguageMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionLanguageMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Maps a caption channel to an ISO 693-2 language code
-- (http:\/\/www.loc.gov\/standards\/iso639-2), with an optional
-- description.
--
-- /See:/ 'newCaptionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'
  { -- | Three character ISO 639-2 language code (see
    -- http:\/\/www.loc.gov\/standards\/iso639-2)
    languageCode :: Core.Text,
    -- | Textual description of language
    languageDescription :: Core.Text,
    -- | The closed caption channel being described by this
    -- CaptionLanguageMapping. Each channel mapping must have a unique channel
    -- number (maximum of 4)
    captionChannel :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptionLanguageMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionLanguageMapping_languageCode' - Three character ISO 639-2 language code (see
-- http:\/\/www.loc.gov\/standards\/iso639-2)
--
-- 'languageDescription', 'captionLanguageMapping_languageDescription' - Textual description of language
--
-- 'captionChannel', 'captionLanguageMapping_captionChannel' - The closed caption channel being described by this
-- CaptionLanguageMapping. Each channel mapping must have a unique channel
-- number (maximum of 4)
newCaptionLanguageMapping ::
  -- | 'languageCode'
  Core.Text ->
  -- | 'languageDescription'
  Core.Text ->
  -- | 'captionChannel'
  Core.Natural ->
  CaptionLanguageMapping
newCaptionLanguageMapping
  pLanguageCode_
  pLanguageDescription_
  pCaptionChannel_ =
    CaptionLanguageMapping'
      { languageCode =
          pLanguageCode_,
        languageDescription = pLanguageDescription_,
        captionChannel = pCaptionChannel_
      }

-- | Three character ISO 639-2 language code (see
-- http:\/\/www.loc.gov\/standards\/iso639-2)
captionLanguageMapping_languageCode :: Lens.Lens' CaptionLanguageMapping Core.Text
captionLanguageMapping_languageCode = Lens.lens (\CaptionLanguageMapping' {languageCode} -> languageCode) (\s@CaptionLanguageMapping' {} a -> s {languageCode = a} :: CaptionLanguageMapping)

-- | Textual description of language
captionLanguageMapping_languageDescription :: Lens.Lens' CaptionLanguageMapping Core.Text
captionLanguageMapping_languageDescription = Lens.lens (\CaptionLanguageMapping' {languageDescription} -> languageDescription) (\s@CaptionLanguageMapping' {} a -> s {languageDescription = a} :: CaptionLanguageMapping)

-- | The closed caption channel being described by this
-- CaptionLanguageMapping. Each channel mapping must have a unique channel
-- number (maximum of 4)
captionLanguageMapping_captionChannel :: Lens.Lens' CaptionLanguageMapping Core.Natural
captionLanguageMapping_captionChannel = Lens.lens (\CaptionLanguageMapping' {captionChannel} -> captionChannel) (\s@CaptionLanguageMapping' {} a -> s {captionChannel = a} :: CaptionLanguageMapping)

instance Core.FromJSON CaptionLanguageMapping where
  parseJSON =
    Core.withObject
      "CaptionLanguageMapping"
      ( \x ->
          CaptionLanguageMapping'
            Core.<$> (x Core..: "languageCode")
            Core.<*> (x Core..: "languageDescription")
            Core.<*> (x Core..: "captionChannel")
      )

instance Core.Hashable CaptionLanguageMapping

instance Core.NFData CaptionLanguageMapping

instance Core.ToJSON CaptionLanguageMapping where
  toJSON CaptionLanguageMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("languageCode" Core..= languageCode),
            Core.Just
              ("languageDescription" Core..= languageDescription),
            Core.Just ("captionChannel" Core..= captionChannel)
          ]
      )
