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
-- Module      : Amazonka.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The timeline of the cluster step lifecycle.
--
-- /See:/ 'newStepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { -- | The date and time when the cluster step was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the cluster step execution completed or failed.
    endDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the cluster step execution started.
    startDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'stepTimeline_creationDateTime' - The date and time when the cluster step was created.
--
-- 'endDateTime', 'stepTimeline_endDateTime' - The date and time when the cluster step execution completed or failed.
--
-- 'startDateTime', 'stepTimeline_startDateTime' - The date and time when the cluster step execution started.
newStepTimeline ::
  StepTimeline
newStepTimeline =
  StepTimeline'
    { creationDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing,
      startDateTime = Prelude.Nothing
    }

-- | The date and time when the cluster step was created.
stepTimeline_creationDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_creationDateTime = Lens.lens (\StepTimeline' {creationDateTime} -> creationDateTime) (\s@StepTimeline' {} a -> s {creationDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time when the cluster step execution completed or failed.
stepTimeline_endDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_endDateTime = Lens.lens (\StepTimeline' {endDateTime} -> endDateTime) (\s@StepTimeline' {} a -> s {endDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Data._Time

-- | The date and time when the cluster step execution started.
stepTimeline_startDateTime :: Lens.Lens' StepTimeline (Prelude.Maybe Prelude.UTCTime)
stepTimeline_startDateTime = Lens.lens (\StepTimeline' {startDateTime} -> startDateTime) (\s@StepTimeline' {} a -> s {startDateTime = a} :: StepTimeline) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON StepTimeline where
  parseJSON =
    Data.withObject
      "StepTimeline"
      ( \x ->
          StepTimeline'
            Prelude.<$> (x Data..:? "CreationDateTime")
            Prelude.<*> (x Data..:? "EndDateTime")
            Prelude.<*> (x Data..:? "StartDateTime")
      )

instance Prelude.Hashable StepTimeline where
  hashWithSalt _salt StepTimeline' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` startDateTime

instance Prelude.NFData StepTimeline where
  rnf StepTimeline' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf endDateTime
      `Prelude.seq` Prelude.rnf startDateTime
