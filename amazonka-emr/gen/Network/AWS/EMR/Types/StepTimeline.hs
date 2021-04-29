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
-- Module      : Network.AWS.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepTimeline where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'newStepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { -- | The date and time when the cluster step execution started.
    startDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the cluster step execution completed or failed.
    endDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the cluster step was created.
    creationDateTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { startDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | The date and time when the cluster step execution started.
stepTimeline_startDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_startDateTime = Lens.lens (\StepTimeline' {startDateTime} -> startDateTime) (\s@StepTimeline' {} a -> s {startDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the cluster step execution completed or failed.
stepTimeline_endDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_endDateTime = Lens.lens (\StepTimeline' {endDateTime} -> endDateTime) (\s@StepTimeline' {} a -> s {endDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the cluster step was created.
stepTimeline_creationDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_creationDateTime = Lens.lens (\StepTimeline' {creationDateTime} -> creationDateTime) (\s@StepTimeline' {} a -> s {creationDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON StepTimeline where
  parseJSON =
    Prelude.withObject
      "StepTimeline"
      ( \x ->
          StepTimeline'
            Prelude.<$> (x Prelude..:? "StartDateTime")
            Prelude.<*> (x Prelude..:? "EndDateTime")
            Prelude.<*> (x Prelude..:? "CreationDateTime")
      )

instance Prelude.Hashable StepTimeline

instance Prelude.NFData StepTimeline
