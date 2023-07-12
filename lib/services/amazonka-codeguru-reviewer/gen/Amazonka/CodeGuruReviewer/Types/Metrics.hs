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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.Metrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the statistics from the code review.
--
-- /See:/ 'newMetrics' smart constructor.
data Metrics = Metrics'
  { -- | Total number of recommendations found in the code review.
    findingsCount :: Prelude.Maybe Prelude.Integer,
    -- | @MeteredLinesOfCodeCount@ is the number of lines of code in the
    -- repository where the code review happened. This does not include
    -- non-code lines such as comments and blank lines.
    meteredLinesOfCodeCount :: Prelude.Maybe Prelude.Integer,
    -- | @SuppressedLinesOfCodeCount@ is the number of lines of code in the
    -- repository where the code review happened that CodeGuru Reviewer did not
    -- analyze. The lines suppressed in the analysis is based on the
    -- @excludeFiles@ variable in the @aws-codeguru-reviewer.yml@ file. This
    -- number does not include non-code lines such as comments and blank lines.
    suppressedLinesOfCodeCount :: Prelude.Maybe Prelude.Integer
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
-- 'meteredLinesOfCodeCount', 'metrics_meteredLinesOfCodeCount' - @MeteredLinesOfCodeCount@ is the number of lines of code in the
-- repository where the code review happened. This does not include
-- non-code lines such as comments and blank lines.
--
-- 'suppressedLinesOfCodeCount', 'metrics_suppressedLinesOfCodeCount' - @SuppressedLinesOfCodeCount@ is the number of lines of code in the
-- repository where the code review happened that CodeGuru Reviewer did not
-- analyze. The lines suppressed in the analysis is based on the
-- @excludeFiles@ variable in the @aws-codeguru-reviewer.yml@ file. This
-- number does not include non-code lines such as comments and blank lines.
newMetrics ::
  Metrics
newMetrics =
  Metrics'
    { findingsCount = Prelude.Nothing,
      meteredLinesOfCodeCount = Prelude.Nothing,
      suppressedLinesOfCodeCount = Prelude.Nothing
    }

-- | Total number of recommendations found in the code review.
metrics_findingsCount :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Integer)
metrics_findingsCount = Lens.lens (\Metrics' {findingsCount} -> findingsCount) (\s@Metrics' {} a -> s {findingsCount = a} :: Metrics)

-- | @MeteredLinesOfCodeCount@ is the number of lines of code in the
-- repository where the code review happened. This does not include
-- non-code lines such as comments and blank lines.
metrics_meteredLinesOfCodeCount :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Integer)
metrics_meteredLinesOfCodeCount = Lens.lens (\Metrics' {meteredLinesOfCodeCount} -> meteredLinesOfCodeCount) (\s@Metrics' {} a -> s {meteredLinesOfCodeCount = a} :: Metrics)

-- | @SuppressedLinesOfCodeCount@ is the number of lines of code in the
-- repository where the code review happened that CodeGuru Reviewer did not
-- analyze. The lines suppressed in the analysis is based on the
-- @excludeFiles@ variable in the @aws-codeguru-reviewer.yml@ file. This
-- number does not include non-code lines such as comments and blank lines.
metrics_suppressedLinesOfCodeCount :: Lens.Lens' Metrics (Prelude.Maybe Prelude.Integer)
metrics_suppressedLinesOfCodeCount = Lens.lens (\Metrics' {suppressedLinesOfCodeCount} -> suppressedLinesOfCodeCount) (\s@Metrics' {} a -> s {suppressedLinesOfCodeCount = a} :: Metrics)

instance Data.FromJSON Metrics where
  parseJSON =
    Data.withObject
      "Metrics"
      ( \x ->
          Metrics'
            Prelude.<$> (x Data..:? "FindingsCount")
            Prelude.<*> (x Data..:? "MeteredLinesOfCodeCount")
            Prelude.<*> (x Data..:? "SuppressedLinesOfCodeCount")
      )

instance Prelude.Hashable Metrics where
  hashWithSalt _salt Metrics' {..} =
    _salt
      `Prelude.hashWithSalt` findingsCount
      `Prelude.hashWithSalt` meteredLinesOfCodeCount
      `Prelude.hashWithSalt` suppressedLinesOfCodeCount

instance Prelude.NFData Metrics where
  rnf Metrics' {..} =
    Prelude.rnf findingsCount
      `Prelude.seq` Prelude.rnf meteredLinesOfCodeCount
      `Prelude.seq` Prelude.rnf suppressedLinesOfCodeCount
