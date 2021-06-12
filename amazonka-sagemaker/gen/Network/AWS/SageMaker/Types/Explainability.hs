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
-- Module      : Network.AWS.SageMaker.Types.Explainability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Explainability where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.MetricsSource

-- | Contains explainability metrics for a model.
--
-- /See:/ 'newExplainability' smart constructor.
data Explainability = Explainability'
  { -- | The explainability report for a model.
    report :: Core.Maybe MetricsSource
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Explainability' {report = Core.Nothing}

-- | The explainability report for a model.
explainability_report :: Lens.Lens' Explainability (Core.Maybe MetricsSource)
explainability_report = Lens.lens (\Explainability' {report} -> report) (\s@Explainability' {} a -> s {report = a} :: Explainability)

instance Core.FromJSON Explainability where
  parseJSON =
    Core.withObject
      "Explainability"
      ( \x ->
          Explainability' Core.<$> (x Core..:? "Report")
      )

instance Core.Hashable Explainability

instance Core.NFData Explainability

instance Core.ToJSON Explainability where
  toJSON Explainability' {..} =
    Core.object
      (Core.catMaybes [("Report" Core..=) Core.<$> report])
