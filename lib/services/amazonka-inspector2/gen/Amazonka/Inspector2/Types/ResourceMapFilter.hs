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
-- Module      : Amazonka.Inspector2.Types.ResourceMapFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ResourceMapFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.ResourceMapComparison
import qualified Amazonka.Prelude as Prelude

-- | A resource map filter for a software bill of material report.
--
-- /See:/ 'newResourceMapFilter' smart constructor.
data ResourceMapFilter = ResourceMapFilter'
  { -- | The filter\'s value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The filter\'s comparison.
    comparison :: ResourceMapComparison,
    -- | The filter\'s key.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceMapFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceMapFilter_value' - The filter\'s value.
--
-- 'comparison', 'resourceMapFilter_comparison' - The filter\'s comparison.
--
-- 'key', 'resourceMapFilter_key' - The filter\'s key.
newResourceMapFilter ::
  -- | 'comparison'
  ResourceMapComparison ->
  -- | 'key'
  Prelude.Text ->
  ResourceMapFilter
newResourceMapFilter pComparison_ pKey_ =
  ResourceMapFilter'
    { value = Prelude.Nothing,
      comparison = pComparison_,
      key = pKey_
    }

-- | The filter\'s value.
resourceMapFilter_value :: Lens.Lens' ResourceMapFilter (Prelude.Maybe Prelude.Text)
resourceMapFilter_value = Lens.lens (\ResourceMapFilter' {value} -> value) (\s@ResourceMapFilter' {} a -> s {value = a} :: ResourceMapFilter)

-- | The filter\'s comparison.
resourceMapFilter_comparison :: Lens.Lens' ResourceMapFilter ResourceMapComparison
resourceMapFilter_comparison = Lens.lens (\ResourceMapFilter' {comparison} -> comparison) (\s@ResourceMapFilter' {} a -> s {comparison = a} :: ResourceMapFilter)

-- | The filter\'s key.
resourceMapFilter_key :: Lens.Lens' ResourceMapFilter Prelude.Text
resourceMapFilter_key = Lens.lens (\ResourceMapFilter' {key} -> key) (\s@ResourceMapFilter' {} a -> s {key = a} :: ResourceMapFilter)

instance Data.FromJSON ResourceMapFilter where
  parseJSON =
    Data.withObject
      "ResourceMapFilter"
      ( \x ->
          ResourceMapFilter'
            Prelude.<$> (x Data..:? "value")
            Prelude.<*> (x Data..: "comparison")
            Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable ResourceMapFilter where
  hashWithSalt _salt ResourceMapFilter' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` comparison
      `Prelude.hashWithSalt` key

instance Prelude.NFData ResourceMapFilter where
  rnf ResourceMapFilter' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf comparison
      `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ResourceMapFilter where
  toJSON ResourceMapFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("comparison" Data..= comparison),
            Prelude.Just ("key" Data..= key)
          ]
      )
