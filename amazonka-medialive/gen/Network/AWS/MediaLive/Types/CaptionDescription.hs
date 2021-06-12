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
-- Module      : Network.AWS.MediaLive.Types.CaptionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionDestinationSettings

-- | Caption Description
--
-- /See:/ 'newCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { -- | ISO 639-2 three-digit code: http:\/\/www.loc.gov\/standards\/iso639-2\/
    languageCode :: Core.Maybe Core.Text,
    -- | Human readable information to indicate captions available for players
    -- (eg. English, or Spanish).
    languageDescription :: Core.Maybe Core.Text,
    -- | Additional settings for captions destination that depend on the
    -- destination type.
    destinationSettings :: Core.Maybe CaptionDestinationSettings,
    -- | Specifies which input caption selector to use as a caption source when
    -- generating output captions. This field should match a captionSelector
    -- name.
    captionSelectorName :: Core.Text,
    -- | Name of the caption description. Used to associate a caption description
    -- with an output. Names must be unique within an event.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CaptionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionDescription_languageCode' - ISO 639-2 three-digit code: http:\/\/www.loc.gov\/standards\/iso639-2\/
--
-- 'languageDescription', 'captionDescription_languageDescription' - Human readable information to indicate captions available for players
-- (eg. English, or Spanish).
--
-- 'destinationSettings', 'captionDescription_destinationSettings' - Additional settings for captions destination that depend on the
-- destination type.
--
-- 'captionSelectorName', 'captionDescription_captionSelectorName' - Specifies which input caption selector to use as a caption source when
-- generating output captions. This field should match a captionSelector
-- name.
--
-- 'name', 'captionDescription_name' - Name of the caption description. Used to associate a caption description
-- with an output. Names must be unique within an event.
newCaptionDescription ::
  -- | 'captionSelectorName'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CaptionDescription
newCaptionDescription pCaptionSelectorName_ pName_ =
  CaptionDescription'
    { languageCode = Core.Nothing,
      languageDescription = Core.Nothing,
      destinationSettings = Core.Nothing,
      captionSelectorName = pCaptionSelectorName_,
      name = pName_
    }

-- | ISO 639-2 three-digit code: http:\/\/www.loc.gov\/standards\/iso639-2\/
captionDescription_languageCode :: Lens.Lens' CaptionDescription (Core.Maybe Core.Text)
captionDescription_languageCode = Lens.lens (\CaptionDescription' {languageCode} -> languageCode) (\s@CaptionDescription' {} a -> s {languageCode = a} :: CaptionDescription)

-- | Human readable information to indicate captions available for players
-- (eg. English, or Spanish).
captionDescription_languageDescription :: Lens.Lens' CaptionDescription (Core.Maybe Core.Text)
captionDescription_languageDescription = Lens.lens (\CaptionDescription' {languageDescription} -> languageDescription) (\s@CaptionDescription' {} a -> s {languageDescription = a} :: CaptionDescription)

-- | Additional settings for captions destination that depend on the
-- destination type.
captionDescription_destinationSettings :: Lens.Lens' CaptionDescription (Core.Maybe CaptionDestinationSettings)
captionDescription_destinationSettings = Lens.lens (\CaptionDescription' {destinationSettings} -> destinationSettings) (\s@CaptionDescription' {} a -> s {destinationSettings = a} :: CaptionDescription)

-- | Specifies which input caption selector to use as a caption source when
-- generating output captions. This field should match a captionSelector
-- name.
captionDescription_captionSelectorName :: Lens.Lens' CaptionDescription Core.Text
captionDescription_captionSelectorName = Lens.lens (\CaptionDescription' {captionSelectorName} -> captionSelectorName) (\s@CaptionDescription' {} a -> s {captionSelectorName = a} :: CaptionDescription)

-- | Name of the caption description. Used to associate a caption description
-- with an output. Names must be unique within an event.
captionDescription_name :: Lens.Lens' CaptionDescription Core.Text
captionDescription_name = Lens.lens (\CaptionDescription' {name} -> name) (\s@CaptionDescription' {} a -> s {name = a} :: CaptionDescription)

instance Core.FromJSON CaptionDescription where
  parseJSON =
    Core.withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            Core.<$> (x Core..:? "languageCode")
            Core.<*> (x Core..:? "languageDescription")
            Core.<*> (x Core..:? "destinationSettings")
            Core.<*> (x Core..: "captionSelectorName")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable CaptionDescription

instance Core.NFData CaptionDescription

instance Core.ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    Core.object
      ( Core.catMaybes
          [ ("languageCode" Core..=) Core.<$> languageCode,
            ("languageDescription" Core..=)
              Core.<$> languageDescription,
            ("destinationSettings" Core..=)
              Core.<$> destinationSettings,
            Core.Just
              ("captionSelectorName" Core..= captionSelectorName),
            Core.Just ("name" Core..= name)
          ]
      )
