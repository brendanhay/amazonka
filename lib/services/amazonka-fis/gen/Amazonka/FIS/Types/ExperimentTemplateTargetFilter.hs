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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateTargetFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateTargetFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter used for the target resources in an experiment
-- template.
--
-- /See:/ 'newExperimentTemplateTargetFilter' smart constructor.
data ExperimentTemplateTargetFilter = ExperimentTemplateTargetFilter'
  { -- | The attribute path for the filter.
    path :: Prelude.Maybe Prelude.Text,
    -- | The attribute values for the filter.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateTargetFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'experimentTemplateTargetFilter_path' - The attribute path for the filter.
--
-- 'values', 'experimentTemplateTargetFilter_values' - The attribute values for the filter.
newExperimentTemplateTargetFilter ::
  ExperimentTemplateTargetFilter
newExperimentTemplateTargetFilter =
  ExperimentTemplateTargetFilter'
    { path =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The attribute path for the filter.
experimentTemplateTargetFilter_path :: Lens.Lens' ExperimentTemplateTargetFilter (Prelude.Maybe Prelude.Text)
experimentTemplateTargetFilter_path = Lens.lens (\ExperimentTemplateTargetFilter' {path} -> path) (\s@ExperimentTemplateTargetFilter' {} a -> s {path = a} :: ExperimentTemplateTargetFilter)

-- | The attribute values for the filter.
experimentTemplateTargetFilter_values :: Lens.Lens' ExperimentTemplateTargetFilter (Prelude.Maybe [Prelude.Text])
experimentTemplateTargetFilter_values = Lens.lens (\ExperimentTemplateTargetFilter' {values} -> values) (\s@ExperimentTemplateTargetFilter' {} a -> s {values = a} :: ExperimentTemplateTargetFilter) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentTemplateTargetFilter where
  parseJSON =
    Data.withObject
      "ExperimentTemplateTargetFilter"
      ( \x ->
          ExperimentTemplateTargetFilter'
            Prelude.<$> (x Data..:? "path")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    ExperimentTemplateTargetFilter
  where
  hashWithSalt
    _salt
    ExperimentTemplateTargetFilter' {..} =
      _salt `Prelude.hashWithSalt` path
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    ExperimentTemplateTargetFilter
  where
  rnf ExperimentTemplateTargetFilter' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf values
