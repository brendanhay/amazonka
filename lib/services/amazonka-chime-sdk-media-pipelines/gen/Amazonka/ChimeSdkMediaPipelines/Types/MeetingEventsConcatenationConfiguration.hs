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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MeetingEventsConcatenationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MeetingEventsConcatenationConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration object for an event concatenation pipeline.
--
-- /See:/ 'newMeetingEventsConcatenationConfiguration' smart constructor.
data MeetingEventsConcatenationConfiguration = MeetingEventsConcatenationConfiguration'
  { -- | Enables or disables the configuration object.
    state :: ArtifactsConcatenationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeetingEventsConcatenationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'meetingEventsConcatenationConfiguration_state' - Enables or disables the configuration object.
newMeetingEventsConcatenationConfiguration ::
  -- | 'state'
  ArtifactsConcatenationState ->
  MeetingEventsConcatenationConfiguration
newMeetingEventsConcatenationConfiguration pState_ =
  MeetingEventsConcatenationConfiguration'
    { state =
        pState_
    }

-- | Enables or disables the configuration object.
meetingEventsConcatenationConfiguration_state :: Lens.Lens' MeetingEventsConcatenationConfiguration ArtifactsConcatenationState
meetingEventsConcatenationConfiguration_state = Lens.lens (\MeetingEventsConcatenationConfiguration' {state} -> state) (\s@MeetingEventsConcatenationConfiguration' {} a -> s {state = a} :: MeetingEventsConcatenationConfiguration)

instance
  Data.FromJSON
    MeetingEventsConcatenationConfiguration
  where
  parseJSON =
    Data.withObject
      "MeetingEventsConcatenationConfiguration"
      ( \x ->
          MeetingEventsConcatenationConfiguration'
            Prelude.<$> (x Data..: "State")
      )

instance
  Prelude.Hashable
    MeetingEventsConcatenationConfiguration
  where
  hashWithSalt
    _salt
    MeetingEventsConcatenationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    MeetingEventsConcatenationConfiguration
  where
  rnf MeetingEventsConcatenationConfiguration' {..} =
    Prelude.rnf state

instance
  Data.ToJSON
    MeetingEventsConcatenationConfiguration
  where
  toJSON MeetingEventsConcatenationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("State" Data..= state)]
      )
