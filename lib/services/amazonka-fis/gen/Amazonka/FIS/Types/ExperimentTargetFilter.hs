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
-- Module      : Amazonka.FIS.Types.ExperimentTargetFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTargetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter used for the target resources in an experiment.
--
-- /See:/ 'newExperimentTargetFilter' smart constructor.
data ExperimentTargetFilter = ExperimentTargetFilter'
  { -- | The attribute path for the filter.
    path :: Prelude.Maybe Prelude.Text,
    -- | The attribute values for the filter.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTargetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'experimentTargetFilter_path' - The attribute path for the filter.
--
-- 'values', 'experimentTargetFilter_values' - The attribute values for the filter.
newExperimentTargetFilter ::
  ExperimentTargetFilter
newExperimentTargetFilter =
  ExperimentTargetFilter'
    { path = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The attribute path for the filter.
experimentTargetFilter_path :: Lens.Lens' ExperimentTargetFilter (Prelude.Maybe Prelude.Text)
experimentTargetFilter_path = Lens.lens (\ExperimentTargetFilter' {path} -> path) (\s@ExperimentTargetFilter' {} a -> s {path = a} :: ExperimentTargetFilter)

-- | The attribute values for the filter.
experimentTargetFilter_values :: Lens.Lens' ExperimentTargetFilter (Prelude.Maybe [Prelude.Text])
experimentTargetFilter_values = Lens.lens (\ExperimentTargetFilter' {values} -> values) (\s@ExperimentTargetFilter' {} a -> s {values = a} :: ExperimentTargetFilter) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ExperimentTargetFilter where
  parseJSON =
    Core.withObject
      "ExperimentTargetFilter"
      ( \x ->
          ExperimentTargetFilter'
            Prelude.<$> (x Core..:? "path")
            Prelude.<*> (x Core..:? "values" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentTargetFilter where
  hashWithSalt _salt ExperimentTargetFilter' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` values

instance Prelude.NFData ExperimentTargetFilter where
  rnf ExperimentTargetFilter' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf values
