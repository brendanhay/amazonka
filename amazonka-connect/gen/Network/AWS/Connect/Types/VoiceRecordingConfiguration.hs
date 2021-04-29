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
-- Module      : Network.AWS.Connect.Types.VoiceRecordingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingConfiguration where

import Network.AWS.Connect.Types.VoiceRecordingTrack
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the recording configuration settings.
--
-- /See:/ 'newVoiceRecordingConfiguration' smart constructor.
data VoiceRecordingConfiguration = VoiceRecordingConfiguration'
  { -- | Identifies which track is being recorded.
    voiceRecordingTrack :: Prelude.Maybe VoiceRecordingTrack
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VoiceRecordingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceRecordingTrack', 'voiceRecordingConfiguration_voiceRecordingTrack' - Identifies which track is being recorded.
newVoiceRecordingConfiguration ::
  VoiceRecordingConfiguration
newVoiceRecordingConfiguration =
  VoiceRecordingConfiguration'
    { voiceRecordingTrack =
        Prelude.Nothing
    }

-- | Identifies which track is being recorded.
voiceRecordingConfiguration_voiceRecordingTrack :: Lens.Lens' VoiceRecordingConfiguration (Prelude.Maybe VoiceRecordingTrack)
voiceRecordingConfiguration_voiceRecordingTrack = Lens.lens (\VoiceRecordingConfiguration' {voiceRecordingTrack} -> voiceRecordingTrack) (\s@VoiceRecordingConfiguration' {} a -> s {voiceRecordingTrack = a} :: VoiceRecordingConfiguration)

instance Prelude.Hashable VoiceRecordingConfiguration

instance Prelude.NFData VoiceRecordingConfiguration

instance Prelude.ToJSON VoiceRecordingConfiguration where
  toJSON VoiceRecordingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VoiceRecordingTrack" Prelude..=)
              Prelude.<$> voiceRecordingTrack
          ]
      )
