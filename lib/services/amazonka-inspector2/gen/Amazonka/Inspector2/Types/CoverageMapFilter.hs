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
-- Module      : Amazonka.Inspector2.Types.CoverageMapFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CoverageMapFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CoverageMapComparison
import qualified Amazonka.Prelude as Prelude

-- | Contains details of a coverage map filter.
--
-- /See:/ 'newCoverageMapFilter' smart constructor.
data CoverageMapFilter = CoverageMapFilter'
  { -- | The tag value associated with the coverage map filter.
    value :: Prelude.Maybe Prelude.Text,
    -- | The operator to compare coverage on.
    comparison :: CoverageMapComparison,
    -- | The tag key associated with the coverage map filter.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageMapFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'coverageMapFilter_value' - The tag value associated with the coverage map filter.
--
-- 'comparison', 'coverageMapFilter_comparison' - The operator to compare coverage on.
--
-- 'key', 'coverageMapFilter_key' - The tag key associated with the coverage map filter.
newCoverageMapFilter ::
  -- | 'comparison'
  CoverageMapComparison ->
  -- | 'key'
  Prelude.Text ->
  CoverageMapFilter
newCoverageMapFilter pComparison_ pKey_ =
  CoverageMapFilter'
    { value = Prelude.Nothing,
      comparison = pComparison_,
      key = pKey_
    }

-- | The tag value associated with the coverage map filter.
coverageMapFilter_value :: Lens.Lens' CoverageMapFilter (Prelude.Maybe Prelude.Text)
coverageMapFilter_value = Lens.lens (\CoverageMapFilter' {value} -> value) (\s@CoverageMapFilter' {} a -> s {value = a} :: CoverageMapFilter)

-- | The operator to compare coverage on.
coverageMapFilter_comparison :: Lens.Lens' CoverageMapFilter CoverageMapComparison
coverageMapFilter_comparison = Lens.lens (\CoverageMapFilter' {comparison} -> comparison) (\s@CoverageMapFilter' {} a -> s {comparison = a} :: CoverageMapFilter)

-- | The tag key associated with the coverage map filter.
coverageMapFilter_key :: Lens.Lens' CoverageMapFilter Prelude.Text
coverageMapFilter_key = Lens.lens (\CoverageMapFilter' {key} -> key) (\s@CoverageMapFilter' {} a -> s {key = a} :: CoverageMapFilter)

instance Prelude.Hashable CoverageMapFilter where
  hashWithSalt _salt CoverageMapFilter' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` key

instance Prelude.NFData CoverageMapFilter where
  rnf CoverageMapFilter' {..} =
    Prelude.rnf value `Prelude.seq`
      Prelude.rnf comparison `Prelude.seq`
        Prelude.rnf key

instance Data.ToJSON CoverageMapFilter where
  toJSON CoverageMapFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("key" Data..= key)
          ]
      )
