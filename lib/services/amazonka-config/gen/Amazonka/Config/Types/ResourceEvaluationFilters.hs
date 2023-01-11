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
-- Module      : Amazonka.Config.Types.ResourceEvaluationFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceEvaluationFilters where

import Amazonka.Config.Types.EvaluationMode
import Amazonka.Config.Types.TimeWindow
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns details of a resource evaluation based on the selected filter.
--
-- /See:/ 'newResourceEvaluationFilters' smart constructor.
data ResourceEvaluationFilters = ResourceEvaluationFilters'
  { -- | Filters evaluations for a given infrastructure deployment. For example:
    -- CFN Stack.
    evaluationContextIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Filters all resource evaluations results based on an evaluation mode.
    -- the valid value for this API is @Proactive@.
    evaluationMode :: Prelude.Maybe EvaluationMode,
    -- | Returns a @TimeWindow@ object.
    timeWindow :: Prelude.Maybe TimeWindow
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceEvaluationFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationContextIdentifier', 'resourceEvaluationFilters_evaluationContextIdentifier' - Filters evaluations for a given infrastructure deployment. For example:
-- CFN Stack.
--
-- 'evaluationMode', 'resourceEvaluationFilters_evaluationMode' - Filters all resource evaluations results based on an evaluation mode.
-- the valid value for this API is @Proactive@.
--
-- 'timeWindow', 'resourceEvaluationFilters_timeWindow' - Returns a @TimeWindow@ object.
newResourceEvaluationFilters ::
  ResourceEvaluationFilters
newResourceEvaluationFilters =
  ResourceEvaluationFilters'
    { evaluationContextIdentifier =
        Prelude.Nothing,
      evaluationMode = Prelude.Nothing,
      timeWindow = Prelude.Nothing
    }

-- | Filters evaluations for a given infrastructure deployment. For example:
-- CFN Stack.
resourceEvaluationFilters_evaluationContextIdentifier :: Lens.Lens' ResourceEvaluationFilters (Prelude.Maybe Prelude.Text)
resourceEvaluationFilters_evaluationContextIdentifier = Lens.lens (\ResourceEvaluationFilters' {evaluationContextIdentifier} -> evaluationContextIdentifier) (\s@ResourceEvaluationFilters' {} a -> s {evaluationContextIdentifier = a} :: ResourceEvaluationFilters)

-- | Filters all resource evaluations results based on an evaluation mode.
-- the valid value for this API is @Proactive@.
resourceEvaluationFilters_evaluationMode :: Lens.Lens' ResourceEvaluationFilters (Prelude.Maybe EvaluationMode)
resourceEvaluationFilters_evaluationMode = Lens.lens (\ResourceEvaluationFilters' {evaluationMode} -> evaluationMode) (\s@ResourceEvaluationFilters' {} a -> s {evaluationMode = a} :: ResourceEvaluationFilters)

-- | Returns a @TimeWindow@ object.
resourceEvaluationFilters_timeWindow :: Lens.Lens' ResourceEvaluationFilters (Prelude.Maybe TimeWindow)
resourceEvaluationFilters_timeWindow = Lens.lens (\ResourceEvaluationFilters' {timeWindow} -> timeWindow) (\s@ResourceEvaluationFilters' {} a -> s {timeWindow = a} :: ResourceEvaluationFilters)

instance Prelude.Hashable ResourceEvaluationFilters where
  hashWithSalt _salt ResourceEvaluationFilters' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationContextIdentifier
      `Prelude.hashWithSalt` evaluationMode
      `Prelude.hashWithSalt` timeWindow

instance Prelude.NFData ResourceEvaluationFilters where
  rnf ResourceEvaluationFilters' {..} =
    Prelude.rnf evaluationContextIdentifier
      `Prelude.seq` Prelude.rnf evaluationMode
      `Prelude.seq` Prelude.rnf timeWindow

instance Data.ToJSON ResourceEvaluationFilters where
  toJSON ResourceEvaluationFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EvaluationContextIdentifier" Data..=)
              Prelude.<$> evaluationContextIdentifier,
            ("EvaluationMode" Data..=)
              Prelude.<$> evaluationMode,
            ("TimeWindow" Data..=) Prelude.<$> timeWindow
          ]
      )
