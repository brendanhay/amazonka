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
-- Module      : Amazonka.Inspector2.Types.ResourceStringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceStringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ResourceStringComparison
import qualified Amazonka.Prelude as Prelude

-- | A resource string filter for a software bill of materials report.
--
-- /See:/ 'newResourceStringFilter' smart constructor.
data ResourceStringFilter = ResourceStringFilter'
  { -- | The filter\'s comparison.
    comparison :: ResourceStringComparison,
    -- | The filter\'s value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'resourceStringFilter_comparison' - The filter\'s comparison.
--
-- 'value', 'resourceStringFilter_value' - The filter\'s value.
newResourceStringFilter ::
  -- | 'comparison'
  ResourceStringComparison ->
  -- | 'value'
  Prelude.Text ->
  ResourceStringFilter
newResourceStringFilter pComparison_ pValue_ =
  ResourceStringFilter'
    { comparison = pComparison_,
      value = pValue_
    }

-- | The filter\'s comparison.
resourceStringFilter_comparison :: Lens.Lens' ResourceStringFilter ResourceStringComparison
resourceStringFilter_comparison = Lens.lens (\ResourceStringFilter' {comparison} -> comparison) (\s@ResourceStringFilter' {} a -> s {comparison = a} :: ResourceStringFilter)

-- | The filter\'s value.
resourceStringFilter_value :: Lens.Lens' ResourceStringFilter Prelude.Text
resourceStringFilter_value = Lens.lens (\ResourceStringFilter' {value} -> value) (\s@ResourceStringFilter' {} a -> s {value = a} :: ResourceStringFilter)

instance Data.FromJSON ResourceStringFilter where
  parseJSON =
    Data.withObject
      "ResourceStringFilter"
      ( \x ->
          ResourceStringFilter'
            Prelude.<$> (x Data..: "comparison")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable ResourceStringFilter where
  hashWithSalt _salt ResourceStringFilter' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` value

instance Prelude.NFData ResourceStringFilter where
  rnf ResourceStringFilter' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ResourceStringFilter where
  toJSON ResourceStringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("value" Data..= value)
          ]
      )
