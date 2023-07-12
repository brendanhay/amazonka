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
-- Module      : Amazonka.SageMaker.Types.Bias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Bias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricsSource

-- | Contains bias metrics for a model.
--
-- /See:/ 'newBias' smart constructor.
data Bias = Bias'
  { -- | The post-training bias report for a model.
    postTrainingReport :: Prelude.Maybe MetricsSource,
    -- | The pre-training bias report for a model.
    preTrainingReport :: Prelude.Maybe MetricsSource,
    -- | The bias report for a model
    report :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postTrainingReport', 'bias_postTrainingReport' - The post-training bias report for a model.
--
-- 'preTrainingReport', 'bias_preTrainingReport' - The pre-training bias report for a model.
--
-- 'report', 'bias_report' - The bias report for a model
newBias ::
  Bias
newBias =
  Bias'
    { postTrainingReport = Prelude.Nothing,
      preTrainingReport = Prelude.Nothing,
      report = Prelude.Nothing
    }

-- | The post-training bias report for a model.
bias_postTrainingReport :: Lens.Lens' Bias (Prelude.Maybe MetricsSource)
bias_postTrainingReport = Lens.lens (\Bias' {postTrainingReport} -> postTrainingReport) (\s@Bias' {} a -> s {postTrainingReport = a} :: Bias)

-- | The pre-training bias report for a model.
bias_preTrainingReport :: Lens.Lens' Bias (Prelude.Maybe MetricsSource)
bias_preTrainingReport = Lens.lens (\Bias' {preTrainingReport} -> preTrainingReport) (\s@Bias' {} a -> s {preTrainingReport = a} :: Bias)

-- | The bias report for a model
bias_report :: Lens.Lens' Bias (Prelude.Maybe MetricsSource)
bias_report = Lens.lens (\Bias' {report} -> report) (\s@Bias' {} a -> s {report = a} :: Bias)

instance Data.FromJSON Bias where
  parseJSON =
    Data.withObject
      "Bias"
      ( \x ->
          Bias'
            Prelude.<$> (x Data..:? "PostTrainingReport")
            Prelude.<*> (x Data..:? "PreTrainingReport")
            Prelude.<*> (x Data..:? "Report")
      )

instance Prelude.Hashable Bias where
  hashWithSalt _salt Bias' {..} =
    _salt
      `Prelude.hashWithSalt` postTrainingReport
      `Prelude.hashWithSalt` preTrainingReport
      `Prelude.hashWithSalt` report

instance Prelude.NFData Bias where
  rnf Bias' {..} =
    Prelude.rnf postTrainingReport
      `Prelude.seq` Prelude.rnf preTrainingReport
      `Prelude.seq` Prelude.rnf report

instance Data.ToJSON Bias where
  toJSON Bias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PostTrainingReport" Data..=)
              Prelude.<$> postTrainingReport,
            ("PreTrainingReport" Data..=)
              Prelude.<$> preTrainingReport,
            ("Report" Data..=) Prelude.<$> report
          ]
      )
