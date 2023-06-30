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
-- Module      : Amazonka.SageMaker.Types.InferenceExperimentSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceExperimentSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The start and end times of an inference experiment.
--
-- The maximum duration that you can set for an inference experiment is 30
-- days.
--
-- /See:/ 'newInferenceExperimentSchedule' smart constructor.
data InferenceExperimentSchedule = InferenceExperimentSchedule'
  { -- | The timestamp at which the inference experiment ended or will end.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp at which the inference experiment started or will start.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceExperimentSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'inferenceExperimentSchedule_endTime' - The timestamp at which the inference experiment ended or will end.
--
-- 'startTime', 'inferenceExperimentSchedule_startTime' - The timestamp at which the inference experiment started or will start.
newInferenceExperimentSchedule ::
  InferenceExperimentSchedule
newInferenceExperimentSchedule =
  InferenceExperimentSchedule'
    { endTime =
        Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The timestamp at which the inference experiment ended or will end.
inferenceExperimentSchedule_endTime :: Lens.Lens' InferenceExperimentSchedule (Prelude.Maybe Prelude.UTCTime)
inferenceExperimentSchedule_endTime = Lens.lens (\InferenceExperimentSchedule' {endTime} -> endTime) (\s@InferenceExperimentSchedule' {} a -> s {endTime = a} :: InferenceExperimentSchedule) Prelude.. Lens.mapping Data._Time

-- | The timestamp at which the inference experiment started or will start.
inferenceExperimentSchedule_startTime :: Lens.Lens' InferenceExperimentSchedule (Prelude.Maybe Prelude.UTCTime)
inferenceExperimentSchedule_startTime = Lens.lens (\InferenceExperimentSchedule' {startTime} -> startTime) (\s@InferenceExperimentSchedule' {} a -> s {startTime = a} :: InferenceExperimentSchedule) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON InferenceExperimentSchedule where
  parseJSON =
    Data.withObject
      "InferenceExperimentSchedule"
      ( \x ->
          InferenceExperimentSchedule'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable InferenceExperimentSchedule where
  hashWithSalt _salt InferenceExperimentSchedule' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData InferenceExperimentSchedule where
  rnf InferenceExperimentSchedule' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON InferenceExperimentSchedule where
  toJSON InferenceExperimentSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )
