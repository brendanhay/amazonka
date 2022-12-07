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
-- Module      : Amazonka.MediaLive.Types.DvbSubSourceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.DvbSubOcrLanguage
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Source Settings
--
-- /See:/ 'newDvbSubSourceSettings' smart constructor.
data DvbSubSourceSettings = DvbSubSourceSettings'
  { -- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source
    -- content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed
    -- through, regardless of selectors.
    pid :: Prelude.Maybe Prelude.Natural,
    -- | If you will configure a WebVTT caption description that references this
    -- caption selector, use this field to provide the language to consider
    -- when translating the image-based source to text.
    ocrLanguage :: Prelude.Maybe DvbSubOcrLanguage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbSubSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pid', 'dvbSubSourceSettings_pid' - When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source
-- content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed
-- through, regardless of selectors.
--
-- 'ocrLanguage', 'dvbSubSourceSettings_ocrLanguage' - If you will configure a WebVTT caption description that references this
-- caption selector, use this field to provide the language to consider
-- when translating the image-based source to text.
newDvbSubSourceSettings ::
  DvbSubSourceSettings
newDvbSubSourceSettings =
  DvbSubSourceSettings'
    { pid = Prelude.Nothing,
      ocrLanguage = Prelude.Nothing
    }

-- | When using DVB-Sub with Burn-In or SMPTE-TT, use this PID for the source
-- content. Unused for DVB-Sub passthrough. All DVB-Sub content is passed
-- through, regardless of selectors.
dvbSubSourceSettings_pid :: Lens.Lens' DvbSubSourceSettings (Prelude.Maybe Prelude.Natural)
dvbSubSourceSettings_pid = Lens.lens (\DvbSubSourceSettings' {pid} -> pid) (\s@DvbSubSourceSettings' {} a -> s {pid = a} :: DvbSubSourceSettings)

-- | If you will configure a WebVTT caption description that references this
-- caption selector, use this field to provide the language to consider
-- when translating the image-based source to text.
dvbSubSourceSettings_ocrLanguage :: Lens.Lens' DvbSubSourceSettings (Prelude.Maybe DvbSubOcrLanguage)
dvbSubSourceSettings_ocrLanguage = Lens.lens (\DvbSubSourceSettings' {ocrLanguage} -> ocrLanguage) (\s@DvbSubSourceSettings' {} a -> s {ocrLanguage = a} :: DvbSubSourceSettings)

instance Data.FromJSON DvbSubSourceSettings where
  parseJSON =
    Data.withObject
      "DvbSubSourceSettings"
      ( \x ->
          DvbSubSourceSettings'
            Prelude.<$> (x Data..:? "pid")
            Prelude.<*> (x Data..:? "ocrLanguage")
      )

instance Prelude.Hashable DvbSubSourceSettings where
  hashWithSalt _salt DvbSubSourceSettings' {..} =
    _salt `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` ocrLanguage

instance Prelude.NFData DvbSubSourceSettings where
  rnf DvbSubSourceSettings' {..} =
    Prelude.rnf pid
      `Prelude.seq` Prelude.rnf ocrLanguage

instance Data.ToJSON DvbSubSourceSettings where
  toJSON DvbSubSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pid" Data..=) Prelude.<$> pid,
            ("ocrLanguage" Data..=) Prelude.<$> ocrLanguage
          ]
      )
