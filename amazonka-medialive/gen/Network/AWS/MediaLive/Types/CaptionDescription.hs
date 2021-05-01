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
-- Module      : Network.AWS.MediaLive.Types.CaptionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionDescription where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionDestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Caption Description
--
-- /See:/ 'newCaptionDescription' smart constructor.
data CaptionDescription = CaptionDescription'
  { -- | ISO 639-2 three-digit code: http:\/\/www.loc.gov\/standards\/iso639-2\/
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | Human readable information to indicate captions available for players
    -- (eg. English, or Spanish).
    languageDescription :: Prelude.Maybe Prelude.Text,
    -- | Additional settings for captions destination that depend on the
    -- destination type.
    destinationSettings :: Prelude.Maybe CaptionDestinationSettings,
    -- | Specifies which input caption selector to use as a caption source when
    -- generating output captions. This field should match a captionSelector
    -- name.
    captionSelectorName :: Prelude.Text,
    -- | Name of the caption description. Used to associate a caption description
    -- with an output. Names must be unique within an event.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CaptionDescription
newCaptionDescription pCaptionSelectorName_ pName_ =
  CaptionDescription'
    { languageCode = Prelude.Nothing,
      languageDescription = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      captionSelectorName = pCaptionSelectorName_,
      name = pName_
    }

-- | ISO 639-2 three-digit code: http:\/\/www.loc.gov\/standards\/iso639-2\/
captionDescription_languageCode :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_languageCode = Lens.lens (\CaptionDescription' {languageCode} -> languageCode) (\s@CaptionDescription' {} a -> s {languageCode = a} :: CaptionDescription)

-- | Human readable information to indicate captions available for players
-- (eg. English, or Spanish).
captionDescription_languageDescription :: Lens.Lens' CaptionDescription (Prelude.Maybe Prelude.Text)
captionDescription_languageDescription = Lens.lens (\CaptionDescription' {languageDescription} -> languageDescription) (\s@CaptionDescription' {} a -> s {languageDescription = a} :: CaptionDescription)

-- | Additional settings for captions destination that depend on the
-- destination type.
captionDescription_destinationSettings :: Lens.Lens' CaptionDescription (Prelude.Maybe CaptionDestinationSettings)
captionDescription_destinationSettings = Lens.lens (\CaptionDescription' {destinationSettings} -> destinationSettings) (\s@CaptionDescription' {} a -> s {destinationSettings = a} :: CaptionDescription)

-- | Specifies which input caption selector to use as a caption source when
-- generating output captions. This field should match a captionSelector
-- name.
captionDescription_captionSelectorName :: Lens.Lens' CaptionDescription Prelude.Text
captionDescription_captionSelectorName = Lens.lens (\CaptionDescription' {captionSelectorName} -> captionSelectorName) (\s@CaptionDescription' {} a -> s {captionSelectorName = a} :: CaptionDescription)

-- | Name of the caption description. Used to associate a caption description
-- with an output. Names must be unique within an event.
captionDescription_name :: Lens.Lens' CaptionDescription Prelude.Text
captionDescription_name = Lens.lens (\CaptionDescription' {name} -> name) (\s@CaptionDescription' {} a -> s {name = a} :: CaptionDescription)

instance Prelude.FromJSON CaptionDescription where
  parseJSON =
    Prelude.withObject
      "CaptionDescription"
      ( \x ->
          CaptionDescription'
            Prelude.<$> (x Prelude..:? "languageCode")
            Prelude.<*> (x Prelude..:? "languageDescription")
            Prelude.<*> (x Prelude..:? "destinationSettings")
            Prelude.<*> (x Prelude..: "captionSelectorName")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable CaptionDescription

instance Prelude.NFData CaptionDescription

instance Prelude.ToJSON CaptionDescription where
  toJSON CaptionDescription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("languageCode" Prelude..=)
              Prelude.<$> languageCode,
            ("languageDescription" Prelude..=)
              Prelude.<$> languageDescription,
            ("destinationSettings" Prelude..=)
              Prelude.<$> destinationSettings,
            Prelude.Just
              ( "captionSelectorName"
                  Prelude..= captionSelectorName
              ),
            Prelude.Just ("name" Prelude..= name)
          ]
      )
