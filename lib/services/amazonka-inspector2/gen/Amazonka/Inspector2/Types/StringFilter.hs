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
-- Module      : Amazonka.Inspector2.Types.StringFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.StringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.StringComparison
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the details of a string filter.
--
-- /See:/ 'newStringFilter' smart constructor.
data StringFilter = StringFilter'
  { -- | The operator to use when comparing values in the filter.
    comparison :: StringComparison,
    -- | The value to filter on.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparison', 'stringFilter_comparison' - The operator to use when comparing values in the filter.
--
-- 'value', 'stringFilter_value' - The value to filter on.
newStringFilter ::
  -- | 'comparison'
  StringComparison ->
  -- | 'value'
  Prelude.Text ->
  StringFilter
newStringFilter pComparison_ pValue_ =
  StringFilter'
    { comparison = pComparison_,
      value = pValue_
    }

-- | The operator to use when comparing values in the filter.
stringFilter_comparison :: Lens.Lens' StringFilter StringComparison
stringFilter_comparison = Lens.lens (\StringFilter' {comparison} -> comparison) (\s@StringFilter' {} a -> s {comparison = a} :: StringFilter)

-- | The value to filter on.
stringFilter_value :: Lens.Lens' StringFilter Prelude.Text
stringFilter_value = Lens.lens (\StringFilter' {value} -> value) (\s@StringFilter' {} a -> s {value = a} :: StringFilter)

instance Data.FromJSON StringFilter where
  parseJSON =
    Data.withObject
      "StringFilter"
      ( \x ->
          StringFilter'
            Prelude.<$> (x Data..: "comparison")
            Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable StringFilter where
  hashWithSalt _salt StringFilter' {..} =
    _salt
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` value

instance Prelude.NFData StringFilter where
  rnf StringFilter' {..} =
    Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON StringFilter where
  toJSON StringFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("value" Data..= value)
          ]
      )
