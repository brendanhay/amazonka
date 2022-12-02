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
-- Module      : Amazonka.ChimeSdkMeetings.Types.AudioFeatures
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.AudioFeatures where

import Amazonka.ChimeSdkMeetings.Types.MeetingFeatureStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An optional category of meeting features that contains audio-specific
-- configurations, such as operating parameters for Amazon Voice Focus.
--
-- /See:/ 'newAudioFeatures' smart constructor.
data AudioFeatures = AudioFeatures'
  { -- | Makes echo reduction available to clients who connect to the meeting.
    echoReduction :: Prelude.Maybe MeetingFeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioFeatures' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'echoReduction', 'audioFeatures_echoReduction' - Makes echo reduction available to clients who connect to the meeting.
newAudioFeatures ::
  AudioFeatures
newAudioFeatures =
  AudioFeatures' {echoReduction = Prelude.Nothing}

-- | Makes echo reduction available to clients who connect to the meeting.
audioFeatures_echoReduction :: Lens.Lens' AudioFeatures (Prelude.Maybe MeetingFeatureStatus)
audioFeatures_echoReduction = Lens.lens (\AudioFeatures' {echoReduction} -> echoReduction) (\s@AudioFeatures' {} a -> s {echoReduction = a} :: AudioFeatures)

instance Data.FromJSON AudioFeatures where
  parseJSON =
    Data.withObject
      "AudioFeatures"
      ( \x ->
          AudioFeatures'
            Prelude.<$> (x Data..:? "EchoReduction")
      )

instance Prelude.Hashable AudioFeatures where
  hashWithSalt _salt AudioFeatures' {..} =
    _salt `Prelude.hashWithSalt` echoReduction

instance Prelude.NFData AudioFeatures where
  rnf AudioFeatures' {..} = Prelude.rnf echoReduction

instance Data.ToJSON AudioFeatures where
  toJSON AudioFeatures' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EchoReduction" Data..=)
              Prelude.<$> echoReduction
          ]
      )
