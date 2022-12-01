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
-- Module      : Amazonka.MediaTailor.Types.Transition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.Transition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaTailor.Types.RelativePosition
import qualified Amazonka.Prelude as Prelude

-- | Program transition configuration.
--
-- /See:/ 'newTransition' smart constructor.
data Transition = Transition'
  { -- | The name of the program that this program will be inserted next to, as
    -- defined by @RelativePosition@.
    relativeProgram :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the program is scheduled to start, in epoch
    -- milliseconds.
    scheduledStartTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The duration of the live program in seconds.
    durationMillis :: Prelude.Maybe Prelude.Integer,
    -- | The position where this program will be inserted relative to the
    -- @RelativePosition@.
    relativePosition :: RelativePosition,
    -- | Defines when the program plays in the schedule. You can set the value to
    -- @ABSOLUTE@ or @RELATIVE@.
    --
    -- @ABSOLUTE@ - The program plays at a specific wall clock time. This
    -- setting can only be used for channels using the @LINEAR@ @PlaybackMode@.
    --
    -- Note the following considerations when using @ABSOLUTE@ transitions:
    --
    -- If the preceding program in the schedule has a duration that extends
    -- past the wall clock time, MediaTailor truncates the preceding program on
    -- a common segment boundary.
    --
    -- If there are gaps in playback, MediaTailor plays the @FillerSlate@ you
    -- configured for your linear channel.
    --
    -- @RELATIVE@ - The program is inserted into the schedule either before or
    -- after a program that you specify via @RelativePosition@.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Transition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relativeProgram', 'transition_relativeProgram' - The name of the program that this program will be inserted next to, as
-- defined by @RelativePosition@.
--
-- 'scheduledStartTimeMillis', 'transition_scheduledStartTimeMillis' - The date and time that the program is scheduled to start, in epoch
-- milliseconds.
--
-- 'durationMillis', 'transition_durationMillis' - The duration of the live program in seconds.
--
-- 'relativePosition', 'transition_relativePosition' - The position where this program will be inserted relative to the
-- @RelativePosition@.
--
-- 'type'', 'transition_type' - Defines when the program plays in the schedule. You can set the value to
-- @ABSOLUTE@ or @RELATIVE@.
--
-- @ABSOLUTE@ - The program plays at a specific wall clock time. This
-- setting can only be used for channels using the @LINEAR@ @PlaybackMode@.
--
-- Note the following considerations when using @ABSOLUTE@ transitions:
--
-- If the preceding program in the schedule has a duration that extends
-- past the wall clock time, MediaTailor truncates the preceding program on
-- a common segment boundary.
--
-- If there are gaps in playback, MediaTailor plays the @FillerSlate@ you
-- configured for your linear channel.
--
-- @RELATIVE@ - The program is inserted into the schedule either before or
-- after a program that you specify via @RelativePosition@.
newTransition ::
  -- | 'relativePosition'
  RelativePosition ->
  -- | 'type''
  Prelude.Text ->
  Transition
newTransition pRelativePosition_ pType_ =
  Transition'
    { relativeProgram = Prelude.Nothing,
      scheduledStartTimeMillis = Prelude.Nothing,
      durationMillis = Prelude.Nothing,
      relativePosition = pRelativePosition_,
      type' = pType_
    }

-- | The name of the program that this program will be inserted next to, as
-- defined by @RelativePosition@.
transition_relativeProgram :: Lens.Lens' Transition (Prelude.Maybe Prelude.Text)
transition_relativeProgram = Lens.lens (\Transition' {relativeProgram} -> relativeProgram) (\s@Transition' {} a -> s {relativeProgram = a} :: Transition)

-- | The date and time that the program is scheduled to start, in epoch
-- milliseconds.
transition_scheduledStartTimeMillis :: Lens.Lens' Transition (Prelude.Maybe Prelude.Integer)
transition_scheduledStartTimeMillis = Lens.lens (\Transition' {scheduledStartTimeMillis} -> scheduledStartTimeMillis) (\s@Transition' {} a -> s {scheduledStartTimeMillis = a} :: Transition)

-- | The duration of the live program in seconds.
transition_durationMillis :: Lens.Lens' Transition (Prelude.Maybe Prelude.Integer)
transition_durationMillis = Lens.lens (\Transition' {durationMillis} -> durationMillis) (\s@Transition' {} a -> s {durationMillis = a} :: Transition)

-- | The position where this program will be inserted relative to the
-- @RelativePosition@.
transition_relativePosition :: Lens.Lens' Transition RelativePosition
transition_relativePosition = Lens.lens (\Transition' {relativePosition} -> relativePosition) (\s@Transition' {} a -> s {relativePosition = a} :: Transition)

-- | Defines when the program plays in the schedule. You can set the value to
-- @ABSOLUTE@ or @RELATIVE@.
--
-- @ABSOLUTE@ - The program plays at a specific wall clock time. This
-- setting can only be used for channels using the @LINEAR@ @PlaybackMode@.
--
-- Note the following considerations when using @ABSOLUTE@ transitions:
--
-- If the preceding program in the schedule has a duration that extends
-- past the wall clock time, MediaTailor truncates the preceding program on
-- a common segment boundary.
--
-- If there are gaps in playback, MediaTailor plays the @FillerSlate@ you
-- configured for your linear channel.
--
-- @RELATIVE@ - The program is inserted into the schedule either before or
-- after a program that you specify via @RelativePosition@.
transition_type :: Lens.Lens' Transition Prelude.Text
transition_type = Lens.lens (\Transition' {type'} -> type') (\s@Transition' {} a -> s {type' = a} :: Transition)

instance Prelude.Hashable Transition where
  hashWithSalt _salt Transition' {..} =
    _salt `Prelude.hashWithSalt` relativeProgram
      `Prelude.hashWithSalt` scheduledStartTimeMillis
      `Prelude.hashWithSalt` durationMillis
      `Prelude.hashWithSalt` relativePosition
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Transition where
  rnf Transition' {..} =
    Prelude.rnf relativeProgram
      `Prelude.seq` Prelude.rnf scheduledStartTimeMillis
      `Prelude.seq` Prelude.rnf durationMillis
      `Prelude.seq` Prelude.rnf relativePosition
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON Transition where
  toJSON Transition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RelativeProgram" Core..=)
              Prelude.<$> relativeProgram,
            ("ScheduledStartTimeMillis" Core..=)
              Prelude.<$> scheduledStartTimeMillis,
            ("DurationMillis" Core..=)
              Prelude.<$> durationMillis,
            Prelude.Just
              ("RelativePosition" Core..= relativePosition),
            Prelude.Just ("Type" Core..= type')
          ]
      )
