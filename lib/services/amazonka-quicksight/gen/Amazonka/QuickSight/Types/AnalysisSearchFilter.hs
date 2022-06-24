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
-- Module      : Amazonka.QuickSight.Types.AnalysisSearchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for one or more analyses.
--
-- /See:/ 'newAnalysisSearchFilter' smart constructor.
data AnalysisSearchFilter = AnalysisSearchFilter'
  { -- | The name of the value that you want to use as a filter, for example
    -- @\"Name\": \"QUICKSIGHT_USER\"@.
    name :: Prelude.Maybe AnalysisFilterAttribute,
    -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StringEquals\"@.
    operator :: Prelude.Maybe FilterOperator,
    -- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
    -- want to use as a filter, for example @\"Value\"@. An example is
    -- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'analysisSearchFilter_name' - The name of the value that you want to use as a filter, for example
-- @\"Name\": \"QUICKSIGHT_USER\"@.
--
-- 'operator', 'analysisSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@.
--
-- 'value', 'analysisSearchFilter_value' - The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example @\"Value\"@. An example is
-- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
newAnalysisSearchFilter ::
  AnalysisSearchFilter
newAnalysisSearchFilter =
  AnalysisSearchFilter'
    { name = Prelude.Nothing,
      operator = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the value that you want to use as a filter, for example
-- @\"Name\": \"QUICKSIGHT_USER\"@.
analysisSearchFilter_name :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe AnalysisFilterAttribute)
analysisSearchFilter_name = Lens.lens (\AnalysisSearchFilter' {name} -> name) (\s@AnalysisSearchFilter' {} a -> s {name = a} :: AnalysisSearchFilter)

-- | The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@.
analysisSearchFilter_operator :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe FilterOperator)
analysisSearchFilter_operator = Lens.lens (\AnalysisSearchFilter' {operator} -> operator) (\s@AnalysisSearchFilter' {} a -> s {operator = a} :: AnalysisSearchFilter)

-- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example @\"Value\"@. An example is
-- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
analysisSearchFilter_value :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe Prelude.Text)
analysisSearchFilter_value = Lens.lens (\AnalysisSearchFilter' {value} -> value) (\s@AnalysisSearchFilter' {} a -> s {value = a} :: AnalysisSearchFilter)

instance Prelude.Hashable AnalysisSearchFilter where
  hashWithSalt _salt AnalysisSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData AnalysisSearchFilter where
  rnf AnalysisSearchFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON AnalysisSearchFilter where
  toJSON AnalysisSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Operator" Core..=) Prelude.<$> operator,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
