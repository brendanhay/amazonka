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
-- Module      : Amazonka.SageMaker.Types.Explainability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Explainability where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricsSource

-- | Contains explainability metrics for a model.
--
-- /See:/ 'newExplainability' smart constructor.
data Explainability = Explainability'
  { -- | The explainability report for a model.
    report :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Explainability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'report', 'explainability_report' - The explainability report for a model.
newExplainability ::
  Explainability
newExplainability =
  Explainability' {report = Prelude.Nothing}

-- | The explainability report for a model.
explainability_report :: Lens.Lens' Explainability (Prelude.Maybe MetricsSource)
explainability_report = Lens.lens (\Explainability' {report} -> report) (\s@Explainability' {} a -> s {report = a} :: Explainability)

instance Data.FromJSON Explainability where
  parseJSON =
    Data.withObject
      "Explainability"
      ( \x ->
          Explainability' Prelude.<$> (x Data..:? "Report")
      )

instance Prelude.Hashable Explainability where
  hashWithSalt _salt Explainability' {..} =
    _salt `Prelude.hashWithSalt` report

instance Prelude.NFData Explainability where
  rnf Explainability' {..} = Prelude.rnf report

instance Data.ToJSON Explainability where
  toJSON Explainability' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Report" Data..=) Prelude.<$> report]
      )
