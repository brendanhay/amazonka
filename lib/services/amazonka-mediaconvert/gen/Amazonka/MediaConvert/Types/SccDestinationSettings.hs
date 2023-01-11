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
-- Module      : Amazonka.MediaConvert.Types.SccDestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SccDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.SccDestinationFramerate
import qualified Amazonka.Prelude as Prelude

-- | Settings related to SCC captions. SCC is a sidecar format that holds
-- captions in a file that is separate from the video container. Set up
-- sidecar captions in the same output group, but different output from
-- your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/scc-srt-output-captions.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set destinationType to SCC.
--
-- /See:/ 'newSccDestinationSettings' smart constructor.
data SccDestinationSettings = SccDestinationSettings'
  { -- | Set Framerate (SccDestinationFramerate) to make sure that the captions
    -- and the video are synchronized in the output. Specify a frame rate that
    -- matches the frame rate of the associated video. If the video frame rate
    -- is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the
    -- video has video_insertion=true and drop_frame_timecode=true; otherwise,
    -- choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
    framerate :: Prelude.Maybe SccDestinationFramerate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SccDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framerate', 'sccDestinationSettings_framerate' - Set Framerate (SccDestinationFramerate) to make sure that the captions
-- and the video are synchronized in the output. Specify a frame rate that
-- matches the frame rate of the associated video. If the video frame rate
-- is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the
-- video has video_insertion=true and drop_frame_timecode=true; otherwise,
-- choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
newSccDestinationSettings ::
  SccDestinationSettings
newSccDestinationSettings =
  SccDestinationSettings'
    { framerate =
        Prelude.Nothing
    }

-- | Set Framerate (SccDestinationFramerate) to make sure that the captions
-- and the video are synchronized in the output. Specify a frame rate that
-- matches the frame rate of the associated video. If the video frame rate
-- is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the
-- video has video_insertion=true and drop_frame_timecode=true; otherwise,
-- choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
sccDestinationSettings_framerate :: Lens.Lens' SccDestinationSettings (Prelude.Maybe SccDestinationFramerate)
sccDestinationSettings_framerate = Lens.lens (\SccDestinationSettings' {framerate} -> framerate) (\s@SccDestinationSettings' {} a -> s {framerate = a} :: SccDestinationSettings)

instance Data.FromJSON SccDestinationSettings where
  parseJSON =
    Data.withObject
      "SccDestinationSettings"
      ( \x ->
          SccDestinationSettings'
            Prelude.<$> (x Data..:? "framerate")
      )

instance Prelude.Hashable SccDestinationSettings where
  hashWithSalt _salt SccDestinationSettings' {..} =
    _salt `Prelude.hashWithSalt` framerate

instance Prelude.NFData SccDestinationSettings where
  rnf SccDestinationSettings' {..} =
    Prelude.rnf framerate

instance Data.ToJSON SccDestinationSettings where
  toJSON SccDestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("framerate" Data..=) Prelude.<$> framerate]
      )
