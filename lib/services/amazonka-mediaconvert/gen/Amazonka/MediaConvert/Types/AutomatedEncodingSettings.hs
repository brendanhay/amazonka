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
-- Module      : Amazonka.MediaConvert.Types.AutomatedEncodingSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AutomatedEncodingSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.AutomatedAbrSettings
import qualified Amazonka.Prelude as Prelude

-- | Use automated encoding to have MediaConvert choose your encoding
-- settings for you, based on characteristics of your input video.
--
-- /See:/ 'newAutomatedEncodingSettings' smart constructor.
data AutomatedEncodingSettings = AutomatedEncodingSettings'
  { -- | Use automated ABR to have MediaConvert set up the renditions in your ABR
    -- package for you automatically, based on characteristics of your input
    -- video. This feature optimizes video quality while minimizing the overall
    -- size of your ABR package.
    abrSettings :: Prelude.Maybe AutomatedAbrSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomatedEncodingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abrSettings', 'automatedEncodingSettings_abrSettings' - Use automated ABR to have MediaConvert set up the renditions in your ABR
-- package for you automatically, based on characteristics of your input
-- video. This feature optimizes video quality while minimizing the overall
-- size of your ABR package.
newAutomatedEncodingSettings ::
  AutomatedEncodingSettings
newAutomatedEncodingSettings =
  AutomatedEncodingSettings'
    { abrSettings =
        Prelude.Nothing
    }

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR
-- package for you automatically, based on characteristics of your input
-- video. This feature optimizes video quality while minimizing the overall
-- size of your ABR package.
automatedEncodingSettings_abrSettings :: Lens.Lens' AutomatedEncodingSettings (Prelude.Maybe AutomatedAbrSettings)
automatedEncodingSettings_abrSettings = Lens.lens (\AutomatedEncodingSettings' {abrSettings} -> abrSettings) (\s@AutomatedEncodingSettings' {} a -> s {abrSettings = a} :: AutomatedEncodingSettings)

instance Core.FromJSON AutomatedEncodingSettings where
  parseJSON =
    Core.withObject
      "AutomatedEncodingSettings"
      ( \x ->
          AutomatedEncodingSettings'
            Prelude.<$> (x Core..:? "abrSettings")
      )

instance Prelude.Hashable AutomatedEncodingSettings where
  hashWithSalt _salt AutomatedEncodingSettings' {..} =
    _salt `Prelude.hashWithSalt` abrSettings

instance Prelude.NFData AutomatedEncodingSettings where
  rnf AutomatedEncodingSettings' {..} =
    Prelude.rnf abrSettings

instance Core.ToJSON AutomatedEncodingSettings where
  toJSON AutomatedEncodingSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("abrSettings" Core..=) Prelude.<$> abrSettings]
      )
