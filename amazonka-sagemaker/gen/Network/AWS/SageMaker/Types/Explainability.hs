{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetricsSource

-- | Contains explainability metrics for a model.
--
-- /See:/ 'newExplainability' smart constructor.
data Explainability = Explainability'
  { -- | The explainability report for a model.
    report :: Prelude.Maybe MetricsSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Explainability where
  parseJSON =
    Prelude.withObject
      "Explainability"
      ( \x ->
          Explainability' Prelude.<$> (x Prelude..:? "Report")
      )

instance Prelude.Hashable Explainability

instance Prelude.NFData Explainability

instance Prelude.ToJSON Explainability where
  toJSON Explainability' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Report" Prelude..=) Prelude.<$> report]
      )
