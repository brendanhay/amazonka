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
-- Module      : Amazonka.Glue.Types.DataQualityRuleResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRuleResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataQualityRuleResultStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the result of the evaluation of a data quality rule.
--
-- /See:/ 'newDataQualityRuleResult' smart constructor.
data DataQualityRuleResult = DataQualityRuleResult'
  { -- | A description of the data quality rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | An evaluation message.
    evaluationMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the data quality rule.
    name :: Prelude.Maybe Prelude.Text,
    -- | A pass or fail status for the rule.
    result :: Prelude.Maybe DataQualityRuleResultStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRuleResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'dataQualityRuleResult_description' - A description of the data quality rule.
--
-- 'evaluationMessage', 'dataQualityRuleResult_evaluationMessage' - An evaluation message.
--
-- 'name', 'dataQualityRuleResult_name' - The name of the data quality rule.
--
-- 'result', 'dataQualityRuleResult_result' - A pass or fail status for the rule.
newDataQualityRuleResult ::
  DataQualityRuleResult
newDataQualityRuleResult =
  DataQualityRuleResult'
    { description =
        Prelude.Nothing,
      evaluationMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      result = Prelude.Nothing
    }

-- | A description of the data quality rule.
dataQualityRuleResult_description :: Lens.Lens' DataQualityRuleResult (Prelude.Maybe Prelude.Text)
dataQualityRuleResult_description = Lens.lens (\DataQualityRuleResult' {description} -> description) (\s@DataQualityRuleResult' {} a -> s {description = a} :: DataQualityRuleResult)

-- | An evaluation message.
dataQualityRuleResult_evaluationMessage :: Lens.Lens' DataQualityRuleResult (Prelude.Maybe Prelude.Text)
dataQualityRuleResult_evaluationMessage = Lens.lens (\DataQualityRuleResult' {evaluationMessage} -> evaluationMessage) (\s@DataQualityRuleResult' {} a -> s {evaluationMessage = a} :: DataQualityRuleResult)

-- | The name of the data quality rule.
dataQualityRuleResult_name :: Lens.Lens' DataQualityRuleResult (Prelude.Maybe Prelude.Text)
dataQualityRuleResult_name = Lens.lens (\DataQualityRuleResult' {name} -> name) (\s@DataQualityRuleResult' {} a -> s {name = a} :: DataQualityRuleResult)

-- | A pass or fail status for the rule.
dataQualityRuleResult_result :: Lens.Lens' DataQualityRuleResult (Prelude.Maybe DataQualityRuleResultStatus)
dataQualityRuleResult_result = Lens.lens (\DataQualityRuleResult' {result} -> result) (\s@DataQualityRuleResult' {} a -> s {result = a} :: DataQualityRuleResult)

instance Data.FromJSON DataQualityRuleResult where
  parseJSON =
    Data.withObject
      "DataQualityRuleResult"
      ( \x ->
          DataQualityRuleResult'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EvaluationMessage")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Result")
      )

instance Prelude.Hashable DataQualityRuleResult where
  hashWithSalt _salt DataQualityRuleResult' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` evaluationMessage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` result

instance Prelude.NFData DataQualityRuleResult where
  rnf DataQualityRuleResult' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf evaluationMessage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf result
