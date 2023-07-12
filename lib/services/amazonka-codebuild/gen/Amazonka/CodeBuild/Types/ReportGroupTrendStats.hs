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
-- Module      : Amazonka.CodeBuild.Types.ReportGroupTrendStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportGroupTrendStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains trend statistics for a set of reports. The actual values depend
-- on the type of trend being collected. For more information, see .
--
-- /See:/ 'newReportGroupTrendStats' smart constructor.
data ReportGroupTrendStats = ReportGroupTrendStats'
  { -- | Contains the average of all values analyzed.
    average :: Prelude.Maybe Prelude.Text,
    -- | Contains the maximum value analyzed.
    max :: Prelude.Maybe Prelude.Text,
    -- | Contains the minimum value analyzed.
    min :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportGroupTrendStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'average', 'reportGroupTrendStats_average' - Contains the average of all values analyzed.
--
-- 'max', 'reportGroupTrendStats_max' - Contains the maximum value analyzed.
--
-- 'min', 'reportGroupTrendStats_min' - Contains the minimum value analyzed.
newReportGroupTrendStats ::
  ReportGroupTrendStats
newReportGroupTrendStats =
  ReportGroupTrendStats'
    { average = Prelude.Nothing,
      max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | Contains the average of all values analyzed.
reportGroupTrendStats_average :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_average = Lens.lens (\ReportGroupTrendStats' {average} -> average) (\s@ReportGroupTrendStats' {} a -> s {average = a} :: ReportGroupTrendStats)

-- | Contains the maximum value analyzed.
reportGroupTrendStats_max :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_max = Lens.lens (\ReportGroupTrendStats' {max} -> max) (\s@ReportGroupTrendStats' {} a -> s {max = a} :: ReportGroupTrendStats)

-- | Contains the minimum value analyzed.
reportGroupTrendStats_min :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_min = Lens.lens (\ReportGroupTrendStats' {min} -> min) (\s@ReportGroupTrendStats' {} a -> s {min = a} :: ReportGroupTrendStats)

instance Data.FromJSON ReportGroupTrendStats where
  parseJSON =
    Data.withObject
      "ReportGroupTrendStats"
      ( \x ->
          ReportGroupTrendStats'
            Prelude.<$> (x Data..:? "average")
            Prelude.<*> (x Data..:? "max")
            Prelude.<*> (x Data..:? "min")
      )

instance Prelude.Hashable ReportGroupTrendStats where
  hashWithSalt _salt ReportGroupTrendStats' {..} =
    _salt
      `Prelude.hashWithSalt` average
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData ReportGroupTrendStats where
  rnf ReportGroupTrendStats' {..} =
    Prelude.rnf average
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf min
