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
-- Module      : Amazonka.ChimeSdkMeetings.Types.MeetingFeaturesConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.MeetingFeaturesConfiguration where

import Amazonka.ChimeSdkMeetings.Types.AudioFeatures
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings of the features available to a meeting.>
--
-- /See:/ 'newMeetingFeaturesConfiguration' smart constructor.
data MeetingFeaturesConfiguration = MeetingFeaturesConfiguration'
  { -- | The configuration settings for the audio features available to a
    -- meeting.
    audio :: Prelude.Maybe AudioFeatures
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeetingFeaturesConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audio', 'meetingFeaturesConfiguration_audio' - The configuration settings for the audio features available to a
-- meeting.
newMeetingFeaturesConfiguration ::
  MeetingFeaturesConfiguration
newMeetingFeaturesConfiguration =
  MeetingFeaturesConfiguration'
    { audio =
        Prelude.Nothing
    }

-- | The configuration settings for the audio features available to a
-- meeting.
meetingFeaturesConfiguration_audio :: Lens.Lens' MeetingFeaturesConfiguration (Prelude.Maybe AudioFeatures)
meetingFeaturesConfiguration_audio = Lens.lens (\MeetingFeaturesConfiguration' {audio} -> audio) (\s@MeetingFeaturesConfiguration' {} a -> s {audio = a} :: MeetingFeaturesConfiguration)

instance Data.FromJSON MeetingFeaturesConfiguration where
  parseJSON =
    Data.withObject
      "MeetingFeaturesConfiguration"
      ( \x ->
          MeetingFeaturesConfiguration'
            Prelude.<$> (x Data..:? "Audio")
      )

instance
  Prelude.Hashable
    MeetingFeaturesConfiguration
  where
  hashWithSalt _salt MeetingFeaturesConfiguration' {..} =
    _salt `Prelude.hashWithSalt` audio

instance Prelude.NFData MeetingFeaturesConfiguration where
  rnf MeetingFeaturesConfiguration' {..} =
    Prelude.rnf audio

instance Data.ToJSON MeetingFeaturesConfiguration where
  toJSON MeetingFeaturesConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Audio" Data..=) Prelude.<$> audio]
      )
