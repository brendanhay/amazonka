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
-- Module      : Amazonka.IVS.Types.IngestConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.IngestConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types.AudioConfiguration
import Amazonka.IVS.Types.VideoConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Object specifying the ingest configuration set up by the broadcaster,
-- usually in an encoder.
--
-- /See:/ 'newIngestConfiguration' smart constructor.
data IngestConfiguration = IngestConfiguration'
  { -- | Encoder settings for audio.
    audio :: Prelude.Maybe AudioConfiguration,
    -- | Encoder settings for video.
    video :: Prelude.Maybe VideoConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audio', 'ingestConfiguration_audio' - Encoder settings for audio.
--
-- 'video', 'ingestConfiguration_video' - Encoder settings for video.
newIngestConfiguration ::
  IngestConfiguration
newIngestConfiguration =
  IngestConfiguration'
    { audio = Prelude.Nothing,
      video = Prelude.Nothing
    }

-- | Encoder settings for audio.
ingestConfiguration_audio :: Lens.Lens' IngestConfiguration (Prelude.Maybe AudioConfiguration)
ingestConfiguration_audio = Lens.lens (\IngestConfiguration' {audio} -> audio) (\s@IngestConfiguration' {} a -> s {audio = a} :: IngestConfiguration)

-- | Encoder settings for video.
ingestConfiguration_video :: Lens.Lens' IngestConfiguration (Prelude.Maybe VideoConfiguration)
ingestConfiguration_video = Lens.lens (\IngestConfiguration' {video} -> video) (\s@IngestConfiguration' {} a -> s {video = a} :: IngestConfiguration)

instance Data.FromJSON IngestConfiguration where
  parseJSON =
    Data.withObject
      "IngestConfiguration"
      ( \x ->
          IngestConfiguration'
            Prelude.<$> (x Data..:? "audio")
            Prelude.<*> (x Data..:? "video")
      )

instance Prelude.Hashable IngestConfiguration where
  hashWithSalt _salt IngestConfiguration' {..} =
    _salt `Prelude.hashWithSalt` audio
      `Prelude.hashWithSalt` video

instance Prelude.NFData IngestConfiguration where
  rnf IngestConfiguration' {..} =
    Prelude.rnf audio `Prelude.seq` Prelude.rnf video
