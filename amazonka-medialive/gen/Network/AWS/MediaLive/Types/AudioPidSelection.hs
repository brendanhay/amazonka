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
-- Module      : Network.AWS.MediaLive.Types.AudioPidSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioPidSelection where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Audio Pid Selection
--
-- /See:/ 'newAudioPidSelection' smart constructor.
data AudioPidSelection = AudioPidSelection'
  { -- | Selects a specific PID from within a source.
    pid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioPidSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pid', 'audioPidSelection_pid' - Selects a specific PID from within a source.
newAudioPidSelection ::
  -- | 'pid'
  Prelude.Natural ->
  AudioPidSelection
newAudioPidSelection pPid_ =
  AudioPidSelection' {pid = pPid_}

-- | Selects a specific PID from within a source.
audioPidSelection_pid :: Lens.Lens' AudioPidSelection Prelude.Natural
audioPidSelection_pid = Lens.lens (\AudioPidSelection' {pid} -> pid) (\s@AudioPidSelection' {} a -> s {pid = a} :: AudioPidSelection)

instance Core.FromJSON AudioPidSelection where
  parseJSON =
    Core.withObject
      "AudioPidSelection"
      ( \x ->
          AudioPidSelection' Prelude.<$> (x Core..: "pid")
      )

instance Prelude.Hashable AudioPidSelection

instance Prelude.NFData AudioPidSelection

instance Core.ToJSON AudioPidSelection where
  toJSON AudioPidSelection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("pid" Core..= pid)]
      )
