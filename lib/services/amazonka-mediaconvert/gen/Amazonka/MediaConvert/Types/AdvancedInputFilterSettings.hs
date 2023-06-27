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
-- Module      : Amazonka.MediaConvert.Types.AdvancedInputFilterSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AdvancedInputFilterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AdvancedInputFilterAddTexture
import Amazonka.MediaConvert.Types.AdvancedInputFilterSharpen
import qualified Amazonka.Prelude as Prelude

-- | Optional settings for Advanced input filter when you set Advanced input
-- filter to Enabled.
--
-- /See:/ 'newAdvancedInputFilterSettings' smart constructor.
data AdvancedInputFilterSettings = AdvancedInputFilterSettings'
  { -- | Add texture and detail to areas of your input video content that were
    -- lost after applying the Advanced input filter. To adaptively add texture
    -- and reduce softness: Choose Enabled. To not add any texture: Keep the
    -- default value, Disabled. We recommend that you choose Disabled for input
    -- video content that doesn\'t have texture, including screen recordings,
    -- computer graphics, or cartoons.
    addTexture :: Prelude.Maybe AdvancedInputFilterAddTexture,
    -- | Optionally specify the amount of sharpening to apply when you use the
    -- Advanced input filter. Sharpening adds contrast to the edges of your
    -- video content and can reduce softness. To apply no sharpening: Keep the
    -- default value, Off. To apply a minimal amount of sharpening choose Low,
    -- or for the maximum choose High.
    sharpening :: Prelude.Maybe AdvancedInputFilterSharpen
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedInputFilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addTexture', 'advancedInputFilterSettings_addTexture' - Add texture and detail to areas of your input video content that were
-- lost after applying the Advanced input filter. To adaptively add texture
-- and reduce softness: Choose Enabled. To not add any texture: Keep the
-- default value, Disabled. We recommend that you choose Disabled for input
-- video content that doesn\'t have texture, including screen recordings,
-- computer graphics, or cartoons.
--
-- 'sharpening', 'advancedInputFilterSettings_sharpening' - Optionally specify the amount of sharpening to apply when you use the
-- Advanced input filter. Sharpening adds contrast to the edges of your
-- video content and can reduce softness. To apply no sharpening: Keep the
-- default value, Off. To apply a minimal amount of sharpening choose Low,
-- or for the maximum choose High.
newAdvancedInputFilterSettings ::
  AdvancedInputFilterSettings
newAdvancedInputFilterSettings =
  AdvancedInputFilterSettings'
    { addTexture =
        Prelude.Nothing,
      sharpening = Prelude.Nothing
    }

-- | Add texture and detail to areas of your input video content that were
-- lost after applying the Advanced input filter. To adaptively add texture
-- and reduce softness: Choose Enabled. To not add any texture: Keep the
-- default value, Disabled. We recommend that you choose Disabled for input
-- video content that doesn\'t have texture, including screen recordings,
-- computer graphics, or cartoons.
advancedInputFilterSettings_addTexture :: Lens.Lens' AdvancedInputFilterSettings (Prelude.Maybe AdvancedInputFilterAddTexture)
advancedInputFilterSettings_addTexture = Lens.lens (\AdvancedInputFilterSettings' {addTexture} -> addTexture) (\s@AdvancedInputFilterSettings' {} a -> s {addTexture = a} :: AdvancedInputFilterSettings)

-- | Optionally specify the amount of sharpening to apply when you use the
-- Advanced input filter. Sharpening adds contrast to the edges of your
-- video content and can reduce softness. To apply no sharpening: Keep the
-- default value, Off. To apply a minimal amount of sharpening choose Low,
-- or for the maximum choose High.
advancedInputFilterSettings_sharpening :: Lens.Lens' AdvancedInputFilterSettings (Prelude.Maybe AdvancedInputFilterSharpen)
advancedInputFilterSettings_sharpening = Lens.lens (\AdvancedInputFilterSettings' {sharpening} -> sharpening) (\s@AdvancedInputFilterSettings' {} a -> s {sharpening = a} :: AdvancedInputFilterSettings)

instance Data.FromJSON AdvancedInputFilterSettings where
  parseJSON =
    Data.withObject
      "AdvancedInputFilterSettings"
      ( \x ->
          AdvancedInputFilterSettings'
            Prelude.<$> (x Data..:? "addTexture")
            Prelude.<*> (x Data..:? "sharpening")
      )

instance Prelude.Hashable AdvancedInputFilterSettings where
  hashWithSalt _salt AdvancedInputFilterSettings' {..} =
    _salt
      `Prelude.hashWithSalt` addTexture
      `Prelude.hashWithSalt` sharpening

instance Prelude.NFData AdvancedInputFilterSettings where
  rnf AdvancedInputFilterSettings' {..} =
    Prelude.rnf addTexture
      `Prelude.seq` Prelude.rnf sharpening

instance Data.ToJSON AdvancedInputFilterSettings where
  toJSON AdvancedInputFilterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("addTexture" Data..=) Prelude.<$> addTexture,
            ("sharpening" Data..=) Prelude.<$> sharpening
          ]
      )
