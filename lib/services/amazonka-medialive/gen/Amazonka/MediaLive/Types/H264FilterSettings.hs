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
-- Module      : Amazonka.MediaLive.Types.H264FilterSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264FilterSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.TemporalFilterSettings
import qualified Amazonka.Prelude as Prelude

-- | H264 Filter Settings
--
-- /See:/ 'newH264FilterSettings' smart constructor.
data H264FilterSettings = H264FilterSettings'
  { temporalFilterSettings :: Prelude.Maybe TemporalFilterSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H264FilterSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporalFilterSettings', 'h264FilterSettings_temporalFilterSettings' - Undocumented member.
newH264FilterSettings ::
  H264FilterSettings
newH264FilterSettings =
  H264FilterSettings'
    { temporalFilterSettings =
        Prelude.Nothing
    }

-- | Undocumented member.
h264FilterSettings_temporalFilterSettings :: Lens.Lens' H264FilterSettings (Prelude.Maybe TemporalFilterSettings)
h264FilterSettings_temporalFilterSettings = Lens.lens (\H264FilterSettings' {temporalFilterSettings} -> temporalFilterSettings) (\s@H264FilterSettings' {} a -> s {temporalFilterSettings = a} :: H264FilterSettings)

instance Data.FromJSON H264FilterSettings where
  parseJSON =
    Data.withObject
      "H264FilterSettings"
      ( \x ->
          H264FilterSettings'
            Prelude.<$> (x Data..:? "temporalFilterSettings")
      )

instance Prelude.Hashable H264FilterSettings where
  hashWithSalt _salt H264FilterSettings' {..} =
    _salt `Prelude.hashWithSalt` temporalFilterSettings

instance Prelude.NFData H264FilterSettings where
  rnf H264FilterSettings' {..} =
    Prelude.rnf temporalFilterSettings

instance Data.ToJSON H264FilterSettings where
  toJSON H264FilterSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("temporalFilterSettings" Data..=)
              Prelude.<$> temporalFilterSettings
          ]
      )
