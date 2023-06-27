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
-- Module      : Amazonka.Config.Types.ResourceEvaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceEvaluation where

import Amazonka.Config.Types.EvaluationMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns details of a resource evaluation.
--
-- /See:/ 'newResourceEvaluation' smart constructor.
data ResourceEvaluation = ResourceEvaluation'
  { -- | The mode of an evaluation. The valid values are Detective or Proactive.
    evaluationMode :: Prelude.Maybe EvaluationMode,
    -- | The starting time of an execution.
    evaluationStartTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ResourceEvaluationId of a evaluation.
    resourceEvaluationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceEvaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationMode', 'resourceEvaluation_evaluationMode' - The mode of an evaluation. The valid values are Detective or Proactive.
--
-- 'evaluationStartTimestamp', 'resourceEvaluation_evaluationStartTimestamp' - The starting time of an execution.
--
-- 'resourceEvaluationId', 'resourceEvaluation_resourceEvaluationId' - The ResourceEvaluationId of a evaluation.
newResourceEvaluation ::
  ResourceEvaluation
newResourceEvaluation =
  ResourceEvaluation'
    { evaluationMode =
        Prelude.Nothing,
      evaluationStartTimestamp = Prelude.Nothing,
      resourceEvaluationId = Prelude.Nothing
    }

-- | The mode of an evaluation. The valid values are Detective or Proactive.
resourceEvaluation_evaluationMode :: Lens.Lens' ResourceEvaluation (Prelude.Maybe EvaluationMode)
resourceEvaluation_evaluationMode = Lens.lens (\ResourceEvaluation' {evaluationMode} -> evaluationMode) (\s@ResourceEvaluation' {} a -> s {evaluationMode = a} :: ResourceEvaluation)

-- | The starting time of an execution.
resourceEvaluation_evaluationStartTimestamp :: Lens.Lens' ResourceEvaluation (Prelude.Maybe Prelude.UTCTime)
resourceEvaluation_evaluationStartTimestamp = Lens.lens (\ResourceEvaluation' {evaluationStartTimestamp} -> evaluationStartTimestamp) (\s@ResourceEvaluation' {} a -> s {evaluationStartTimestamp = a} :: ResourceEvaluation) Prelude.. Lens.mapping Data._Time

-- | The ResourceEvaluationId of a evaluation.
resourceEvaluation_resourceEvaluationId :: Lens.Lens' ResourceEvaluation (Prelude.Maybe Prelude.Text)
resourceEvaluation_resourceEvaluationId = Lens.lens (\ResourceEvaluation' {resourceEvaluationId} -> resourceEvaluationId) (\s@ResourceEvaluation' {} a -> s {resourceEvaluationId = a} :: ResourceEvaluation)

instance Data.FromJSON ResourceEvaluation where
  parseJSON =
    Data.withObject
      "ResourceEvaluation"
      ( \x ->
          ResourceEvaluation'
            Prelude.<$> (x Data..:? "EvaluationMode")
            Prelude.<*> (x Data..:? "EvaluationStartTimestamp")
            Prelude.<*> (x Data..:? "ResourceEvaluationId")
      )

instance Prelude.Hashable ResourceEvaluation where
  hashWithSalt _salt ResourceEvaluation' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationMode
      `Prelude.hashWithSalt` evaluationStartTimestamp
      `Prelude.hashWithSalt` resourceEvaluationId

instance Prelude.NFData ResourceEvaluation where
  rnf ResourceEvaluation' {..} =
    Prelude.rnf evaluationMode
      `Prelude.seq` Prelude.rnf evaluationStartTimestamp
      `Prelude.seq` Prelude.rnf resourceEvaluationId
