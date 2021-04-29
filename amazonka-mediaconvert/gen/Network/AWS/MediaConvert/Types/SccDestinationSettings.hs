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
-- Module      : Network.AWS.MediaConvert.Types.SccDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SccDestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.SccDestinationFramerate
import qualified Network.AWS.Prelude as Prelude

-- | Settings for SCC caption output.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON SccDestinationSettings where
  parseJSON =
    Prelude.withObject
      "SccDestinationSettings"
      ( \x ->
          SccDestinationSettings'
            Prelude.<$> (x Prelude..:? "framerate")
      )

instance Prelude.Hashable SccDestinationSettings

instance Prelude.NFData SccDestinationSettings

instance Prelude.ToJSON SccDestinationSettings where
  toJSON SccDestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("framerate" Prelude..=) Prelude.<$> framerate]
      )
