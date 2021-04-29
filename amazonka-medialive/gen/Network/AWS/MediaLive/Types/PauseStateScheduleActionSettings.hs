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
-- Module      : Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PauseStateScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.PipelinePauseStateSettings
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the action to set pause state of a channel.
--
-- /See:/ 'newPauseStateScheduleActionSettings' smart constructor.
data PauseStateScheduleActionSettings = PauseStateScheduleActionSettings'
  { pipelines :: Prelude.Maybe [PipelinePauseStateSettings]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Undocumented member.
pauseStateScheduleActionSettings_pipelines :: Lens.Lens' PauseStateScheduleActionSettings (Prelude.Maybe [PipelinePauseStateSettings])
pauseStateScheduleActionSettings_pipelines = Lens.lens (\PauseStateScheduleActionSettings' {pipelines} -> pipelines) (\s@PauseStateScheduleActionSettings' {} a -> s {pipelines = a} :: PauseStateScheduleActionSettings) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    PauseStateScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "PauseStateScheduleActionSettings"
      ( \x ->
          PauseStateScheduleActionSettings'
            Prelude.<$> ( x Prelude..:? "pipelines"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    PauseStateScheduleActionSettings

instance
  Prelude.NFData
    PauseStateScheduleActionSettings

instance
  Prelude.ToJSON
    PauseStateScheduleActionSettings
  where
  toJSON PauseStateScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pipelines" Prelude..=) Prelude.<$> pipelines]
      )
