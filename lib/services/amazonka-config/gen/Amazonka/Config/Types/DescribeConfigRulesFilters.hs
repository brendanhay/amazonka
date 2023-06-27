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
-- Module      : Amazonka.Config.Types.DescribeConfigRulesFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.DescribeConfigRulesFilters where

import Amazonka.Config.Types.EvaluationMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns a filtered list of Detective or Proactive Config rules. By
-- default, if the filter is not defined, this API returns an unfiltered
-- list. For more information on Detective or Proactive Config rules, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/evaluate-config-rules.html Evaluation Mode>
-- in the /Config Developer Guide/.
--
-- /See:/ 'newDescribeConfigRulesFilters' smart constructor.
data DescribeConfigRulesFilters = DescribeConfigRulesFilters'
  { -- | The mode of an evaluation. The valid values are Detective or Proactive.
    evaluationMode :: Prelude.Maybe EvaluationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigRulesFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationMode', 'describeConfigRulesFilters_evaluationMode' - The mode of an evaluation. The valid values are Detective or Proactive.
newDescribeConfigRulesFilters ::
  DescribeConfigRulesFilters
newDescribeConfigRulesFilters =
  DescribeConfigRulesFilters'
    { evaluationMode =
        Prelude.Nothing
    }

-- | The mode of an evaluation. The valid values are Detective or Proactive.
describeConfigRulesFilters_evaluationMode :: Lens.Lens' DescribeConfigRulesFilters (Prelude.Maybe EvaluationMode)
describeConfigRulesFilters_evaluationMode = Lens.lens (\DescribeConfigRulesFilters' {evaluationMode} -> evaluationMode) (\s@DescribeConfigRulesFilters' {} a -> s {evaluationMode = a} :: DescribeConfigRulesFilters)

instance Prelude.Hashable DescribeConfigRulesFilters where
  hashWithSalt _salt DescribeConfigRulesFilters' {..} =
    _salt `Prelude.hashWithSalt` evaluationMode

instance Prelude.NFData DescribeConfigRulesFilters where
  rnf DescribeConfigRulesFilters' {..} =
    Prelude.rnf evaluationMode

instance Data.ToJSON DescribeConfigRulesFilters where
  toJSON DescribeConfigRulesFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EvaluationMode" Data..=)
              Prelude.<$> evaluationMode
          ]
      )
