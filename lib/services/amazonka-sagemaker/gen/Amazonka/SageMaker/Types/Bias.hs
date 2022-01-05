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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Bias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricsSource

-- | Contains bias metrics for a model.
--
-- /See:/ 'newBias' smart constructor.
data Bias = Bias'
  { -- | The bias report for a model
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
-- 'report', 'bias_report' - The bias report for a model
newBias ::
  Bias
newBias = Bias' {report = Prelude.Nothing}

-- | The bias report for a model
bias_report :: Lens.Lens' Bias (Prelude.Maybe MetricsSource)
bias_report = Lens.lens (\Bias' {report} -> report) (\s@Bias' {} a -> s {report = a} :: Bias)

instance Core.FromJSON Bias where
  parseJSON =
    Core.withObject
      "Bias"
      (\x -> Bias' Prelude.<$> (x Core..:? "Report"))

instance Prelude.Hashable Bias where
  hashWithSalt _salt Bias' {..} =
    _salt `Prelude.hashWithSalt` report

instance Prelude.NFData Bias where
  rnf Bias' {..} = Prelude.rnf report

instance Core.ToJSON Bias where
  toJSON Bias' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Report" Core..=) Prelude.<$> report]
      )
