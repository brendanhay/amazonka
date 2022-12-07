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
-- Module      : Amazonka.MediaLive.Types.CaptionSelector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.CaptionSelectorSettings
import qualified Amazonka.Prelude as Prelude

-- | Output groups for this Live Event. Output groups contain information
-- about where streams should be distributed.
--
-- /See:/ 'newCaptionSelector' smart constructor.
data CaptionSelector = CaptionSelector'
  { -- | Caption selector settings.
    selectorSettings :: Prelude.Maybe CaptionSelectorSettings,
    -- | When specified this field indicates the three letter language code of
    -- the caption track to extract from the source.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | Name identifier for a caption selector. This name is used to associate
    -- this caption selector with one or more caption descriptions. Names must
    -- be unique within an event.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'selectorSettings', 'captionSelector_selectorSettings' - Caption selector settings.
--
-- 'languageCode', 'captionSelector_languageCode' - When specified this field indicates the three letter language code of
-- the caption track to extract from the source.
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
    { selectorSettings =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      name = pName_
    }

-- | Caption selector settings.
captionSelector_selectorSettings :: Lens.Lens' CaptionSelector (Prelude.Maybe CaptionSelectorSettings)
captionSelector_selectorSettings = Lens.lens (\CaptionSelector' {selectorSettings} -> selectorSettings) (\s@CaptionSelector' {} a -> s {selectorSettings = a} :: CaptionSelector)

-- | When specified this field indicates the three letter language code of
-- the caption track to extract from the source.
captionSelector_languageCode :: Lens.Lens' CaptionSelector (Prelude.Maybe Prelude.Text)
captionSelector_languageCode = Lens.lens (\CaptionSelector' {languageCode} -> languageCode) (\s@CaptionSelector' {} a -> s {languageCode = a} :: CaptionSelector)

-- | Name identifier for a caption selector. This name is used to associate
-- this caption selector with one or more caption descriptions. Names must
-- be unique within an event.
captionSelector_name :: Lens.Lens' CaptionSelector Prelude.Text
captionSelector_name = Lens.lens (\CaptionSelector' {name} -> name) (\s@CaptionSelector' {} a -> s {name = a} :: CaptionSelector)

instance Data.FromJSON CaptionSelector where
  parseJSON =
    Data.withObject
      "CaptionSelector"
      ( \x ->
          CaptionSelector'
            Prelude.<$> (x Data..:? "selectorSettings")
            Prelude.<*> (x Data..:? "languageCode")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable CaptionSelector where
  hashWithSalt _salt CaptionSelector' {..} =
    _salt `Prelude.hashWithSalt` selectorSettings
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` name

instance Prelude.NFData CaptionSelector where
  rnf CaptionSelector' {..} =
    Prelude.rnf selectorSettings
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CaptionSelector where
  toJSON CaptionSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("selectorSettings" Data..=)
              Prelude.<$> selectorSettings,
            ("languageCode" Data..=) Prelude.<$> languageCode,
            Prelude.Just ("name" Data..= name)
          ]
      )
