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
-- Module      : Network.AWS.MediaLive.Types.CaptionSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CaptionSelector where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CaptionSelectorSettings
import qualified Network.AWS.Prelude as Prelude

-- | Output groups for this Live Event. Output groups contain information
-- about where streams should be distributed.
--
-- /See:/ 'newCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { -- | When specified this field indicates the three letter language code of
    -- the caption track to extract from the source.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | Caption selector settings.
    selectorSettings :: Prelude.Maybe CaptionSelectorSettings,
    -- | Name identifier for a caption selector. This name is used to associate
    -- this caption selector with one or more caption descriptions. Names must
    -- be unique within an event.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CaptionSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionSelector_languageCode' - When specified this field indicates the three letter language code of
-- the caption track to extract from the source.
--
-- 'selectorSettings', 'captionSelector_selectorSettings' - Caption selector settings.
--
-- 'name', 'captionSelector_name' - Name identifier for a caption selector. This name is used to associate
-- this caption selector with one or more caption descriptions. Names must
-- be unique within an event.
newCaptionSelector ::
  -- | 'name'
  Prelude.Text ->
  CaptionSelector
newCaptionSelector pName_ =
  CaptionSelector'
    { languageCode = Prelude.Nothing,
      selectorSettings = Prelude.Nothing,
      name = pName_
    }

-- | When specified this field indicates the three letter language code of
-- the caption track to extract from the source.
captionSelector_languageCode :: Lens.Lens' CaptionSelector (Prelude.Maybe Prelude.Text)
captionSelector_languageCode = Lens.lens (\CaptionSelector' {languageCode} -> languageCode) (\s@CaptionSelector' {} a -> s {languageCode = a} :: CaptionSelector)

-- | Caption selector settings.
captionSelector_selectorSettings :: Lens.Lens' CaptionSelector (Prelude.Maybe CaptionSelectorSettings)
captionSelector_selectorSettings = Lens.lens (\CaptionSelector' {selectorSettings} -> selectorSettings) (\s@CaptionSelector' {} a -> s {selectorSettings = a} :: CaptionSelector)

-- | Name identifier for a caption selector. This name is used to associate
-- this caption selector with one or more caption descriptions. Names must
-- be unique within an event.
captionSelector_name :: Lens.Lens' CaptionSelector Prelude.Text
captionSelector_name = Lens.lens (\CaptionSelector' {name} -> name) (\s@CaptionSelector' {} a -> s {name = a} :: CaptionSelector)

instance Prelude.FromJSON CaptionSelector where
  parseJSON =
    Prelude.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Prelude.<$> (x Prelude..:? "languageCode")
            Prelude.<*> (x Prelude..:? "selectorSettings")
            Prelude.<*> (x Prelude..: "name")
      )

instance Prelude.Hashable CaptionSelector

instance Prelude.NFData CaptionSelector

instance Prelude.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("languageCode" Prelude..=)
              Prelude.<$> languageCode,
            ("selectorSettings" Prelude..=)
              Prelude.<$> selectorSettings,
            Prelude.Just ("name" Prelude..= name)
          ]
      )
