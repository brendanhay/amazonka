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
-- Module      : Amazonka.Inspector2.Types.CoverageStringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CoverageStringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CoverageStringComparison
import qualified Amazonka.Prelude as Prelude

-- | Contains details of a coverage string filter.
--
-- /See:/ 'newCoverageStringFilter' smart constructor.
data CoverageStringFilter = CoverageStringFilter'
  { -- | The operator to compare strings on.
    comparison :: CoverageStringComparison,
    -- | The value to compare strings on.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'coverageStringFilter_comparison' - The operator to compare strings on.
--
-- 'value', 'coverageStringFilter_value' - The value to compare strings on.
newCoverageStringFilter ::
  -- | 'comparison'
  CoverageStringComparison ->
  -- | 'value'
  Prelude.Text ->
  CoverageStringFilter
newCoverageStringFilter pComparison_ pValue_ =
  CoverageStringFilter'
    { comparison = pComparison_,
      value = pValue_
    }

-- | The operator to compare strings on.
coverageStringFilter_comparison :: Lens.Lens' CoverageStringFilter CoverageStringComparison
coverageStringFilter_comparison = Lens.lens (\CoverageStringFilter' {comparison} -> comparison) (\s@CoverageStringFilter' {} a -> s {comparison = a} :: CoverageStringFilter)

-- | The value to compare strings on.
coverageStringFilter_value :: Lens.Lens' CoverageStringFilter Prelude.Text
coverageStringFilter_value = Lens.lens (\CoverageStringFilter' {value} -> value) (\s@CoverageStringFilter' {} a -> s {value = a} :: CoverageStringFilter)

instance Prelude.Hashable CoverageStringFilter where
  hashWithSalt _salt CoverageStringFilter' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` value

instance Prelude.NFData CoverageStringFilter where
  rnf CoverageStringFilter' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CoverageStringFilter where
  toJSON CoverageStringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("value" Data..= value)
          ]
      )
