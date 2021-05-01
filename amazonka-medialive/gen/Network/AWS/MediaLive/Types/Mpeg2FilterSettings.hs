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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2FilterSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2FilterSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.TemporalFilterSettings
import qualified Network.AWS.Prelude as Prelude

-- | Mpeg2 Filter Settings
--
-- /See:/ 'newMpeg2FilterSettings' smart constructor.
data Mpeg2FilterSettings = Mpeg2FilterSettings'
  { temporalFilterSettings :: Prelude.Maybe TemporalFilterSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Mpeg2FilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporalFilterSettings', 'mpeg2FilterSettings_temporalFilterSettings' - Undocumented member.
newMpeg2FilterSettings ::
  Mpeg2FilterSettings
newMpeg2FilterSettings =
  Mpeg2FilterSettings'
    { temporalFilterSettings =
        Prelude.Nothing
    }

-- | Undocumented member.
mpeg2FilterSettings_temporalFilterSettings :: Lens.Lens' Mpeg2FilterSettings (Prelude.Maybe TemporalFilterSettings)
mpeg2FilterSettings_temporalFilterSettings = Lens.lens (\Mpeg2FilterSettings' {temporalFilterSettings} -> temporalFilterSettings) (\s@Mpeg2FilterSettings' {} a -> s {temporalFilterSettings = a} :: Mpeg2FilterSettings)

instance Prelude.FromJSON Mpeg2FilterSettings where
  parseJSON =
    Prelude.withObject
      "Mpeg2FilterSettings"
      ( \x ->
          Mpeg2FilterSettings'
            Prelude.<$> (x Prelude..:? "temporalFilterSettings")
      )

instance Prelude.Hashable Mpeg2FilterSettings

instance Prelude.NFData Mpeg2FilterSettings

instance Prelude.ToJSON Mpeg2FilterSettings where
  toJSON Mpeg2FilterSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("temporalFilterSettings" Prelude..=)
              Prelude.<$> temporalFilterSettings
          ]
      )
