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
-- Module      : Network.AWS.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepTimeline where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'newStepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { -- | The date and time when the cluster step execution started.
    startDateTime :: Core.Maybe Core.POSIX,
    -- | The date and time when the cluster step execution completed or failed.
    endDateTime :: Core.Maybe Core.POSIX,
    -- | The date and time when the cluster step was created.
    creationDateTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StepTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDateTime', 'stepTimeline_startDateTime' - The date and time when the cluster step execution started.
--
-- 'endDateTime', 'stepTimeline_endDateTime' - The date and time when the cluster step execution completed or failed.
--
-- 'creationDateTime', 'stepTimeline_creationDateTime' - The date and time when the cluster step was created.
newStepTimeline ::
  StepTimeline
newStepTimeline =
  StepTimeline'
    { startDateTime = Core.Nothing,
      endDateTime = Core.Nothing,
      creationDateTime = Core.Nothing
    }

-- | The date and time when the cluster step execution started.
stepTimeline_startDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.UTCTime)
stepTimeline_startDateTime = Lens.lens (\StepTimeline' {startDateTime} -> startDateTime) (\s@StepTimeline' {} a -> s {startDateTime = a} :: StepTimeline) Core.. Lens.mapping Core._Time

-- | The date and time when the cluster step execution completed or failed.
stepTimeline_endDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.UTCTime)
stepTimeline_endDateTime = Lens.lens (\StepTimeline' {endDateTime} -> endDateTime) (\s@StepTimeline' {} a -> s {endDateTime = a} :: StepTimeline) Core.. Lens.mapping Core._Time

-- | The date and time when the cluster step was created.
stepTimeline_creationDateTime :: Lens.Lens' StepTimeline (Core.Maybe Core.UTCTime)
stepTimeline_creationDateTime = Lens.lens (\StepTimeline' {creationDateTime} -> creationDateTime) (\s@StepTimeline' {} a -> s {creationDateTime = a} :: StepTimeline) Core.. Lens.mapping Core._Time

instance Core.FromJSON StepTimeline where
  parseJSON =
    Core.withObject
      "StepTimeline"
      ( \x ->
          StepTimeline'
            Core.<$> (x Core..:? "StartDateTime")
            Core.<*> (x Core..:? "EndDateTime")
            Core.<*> (x Core..:? "CreationDateTime")
      )

instance Core.Hashable StepTimeline

instance Core.NFData StepTimeline
