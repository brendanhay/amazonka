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
-- Module      : Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedEncodingSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AutomatedEncodingSettings where
  parseJSON =
    Prelude.withObject
      "AutomatedEncodingSettings"
      ( \x ->
          AutomatedEncodingSettings'
            Prelude.<$> (x Prelude..:? "abrSettings")
      )

instance Prelude.Hashable AutomatedEncodingSettings

instance Prelude.NFData AutomatedEncodingSettings

instance Prelude.ToJSON AutomatedEncodingSettings where
  toJSON AutomatedEncodingSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("abrSettings" Prelude..=) Prelude.<$> abrSettings]
      )
