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
-- Module      : Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'newPauseStateScheduleActionSettings' smart constructor.
data PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'
  { pipelines :: Core.Maybe [PipelinePauseStateSettings]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PauseStateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelines', 'pauseStateScheduleActionSettings_pipelines' - Undocumented member.
newPauseStateScheduleActionSettings ::
  PauseStateScheduleActionSettings
newPauseStateScheduleActionSettings =
  PauseStateScheduleActionSettings'
    { pipelines =
        Core.Nothing
    }

-- | Undocumented member.
pauseStateScheduleActionSettings_pipelines :: Lens.Lens' PauseStateScheduleActionSettings (Core.Maybe [PipelinePauseStateSettings])
pauseStateScheduleActionSettings_pipelines = Lens.lens (\PauseStateScheduleActionSettings' {pipelines} -> pipelines) (\s@PauseStateScheduleActionSettings' {} a -> s {pipelines = a} :: PauseStateScheduleActionSettings) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    PauseStateScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "PauseStateScheduleActionSettings"
      ( \x ->
          PauseStateScheduleActionSettings'
            Core.<$> (x Core..:? "pipelines" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    PauseStateScheduleActionSettings

instance Core.NFData PauseStateScheduleActionSettings

instance Core.ToJSON PauseStateScheduleActionSettings where
  toJSON PauseStateScheduleActionSettings' {..} =
    Core.object
      ( Core.catMaybes
          [("pipelines" Core..=) Core.<$> pipelines]
      )
