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
-- Module      : Amazonka.CodeGuruReviewer.Types.Metrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.Metrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the statistics from the code review.
--
-- /See:/ 'newMetrics' smart constructor.
data Metrics = Metrics'
  { -- | Total number of recommendations found in the code review.
    findingsCount :: Prelude.Maybe Prelude.Integer,
    -- | @MeteredLinesOfCode@ is the number of lines of code in the repository
    -- where the code review happened. This does not include non-code lines
    -- such as comments and blank lines.
    meteredLinesOfCodeCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingsCount', 'metrics_findingsCount' - Total number of recommendations found in the code review.
--
-- 'meteredLinesOfCodeCount', 'metrics_meteredLinesOfCodeCount' - @MeteredLinesOfCode@ is the number of lines of code in the repository
-- where the code review happened. This does not include non-code lines
-- such as comments and blank lines.
newMetrics ::
  Metrics
newMetrics =
  Metrics'
    { findingsCount = Prelude.Nothing,
      meteredLinesOfCodeCount = Prelude.Nothing
    }

-- | Total number of recommendations found in the code review.
metrics_findingsCount :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Integer)
metrics_findingsCount = Lens.lens (\Metrics' {findingsCount} -> findingsCount) (\s@Metrics' {} a -> s {findingsCount = a} :: Metrics)

-- | @MeteredLinesOfCode@ is the number of lines of code in the repository
-- where the code review happened. This does not include non-code lines
-- such as comments and blank lines.
metrics_meteredLinesOfCodeCount :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Integer)
metrics_meteredLinesOfCodeCount = Lens.lens (\Metrics' {meteredLinesOfCodeCount} -> meteredLinesOfCodeCount) (\s@Metrics' {} a -> s {meteredLinesOfCodeCount = a} :: Metrics)

instance Core.FromJSON Metrics where
  parseJSON =
    Core.withObject
      "Metrics"
      ( \x ->
          Metrics'
            Prelude.<$> (x Core..:? "FindingsCount")
            Prelude.<*> (x Core..:? "MeteredLinesOfCodeCount")
      )

instance Prelude.Hashable Metrics where
  hashWithSalt salt' Metrics' {..} =
    salt'
      `Prelude.hashWithSalt` meteredLinesOfCodeCount
      `Prelude.hashWithSalt` findingsCount

instance Prelude.NFData Metrics where
  rnf Metrics' {..} =
    Prelude.rnf findingsCount
      `Prelude.seq` Prelude.rnf meteredLinesOfCodeCount
