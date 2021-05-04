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
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendStats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendStats where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains trend statistics for a set of reports. The actual values depend
-- on the type of trend being collected. For more information, see .
--
-- /See:/ 'newReportGroupTrendStats' smart constructor.
data ReportGroupTrendStats = ReportGroupTrendStats'
  { -- | Contains the minimum value analyzed.
    min :: Prelude.Maybe Prelude.Text,
    -- | Contains the maximum value analyzed.
    max :: Prelude.Maybe Prelude.Text,
    -- | Contains the average of all values analyzed.
    average :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReportGroupTrendStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'min', 'reportGroupTrendStats_min' - Contains the minimum value analyzed.
--
-- 'max', 'reportGroupTrendStats_max' - Contains the maximum value analyzed.
--
-- 'average', 'reportGroupTrendStats_average' - Contains the average of all values analyzed.
newReportGroupTrendStats ::
  ReportGroupTrendStats
newReportGroupTrendStats =
  ReportGroupTrendStats'
    { min = Prelude.Nothing,
      max = Prelude.Nothing,
      average = Prelude.Nothing
    }

-- | Contains the minimum value analyzed.
reportGroupTrendStats_min :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_min = Lens.lens (\ReportGroupTrendStats' {min} -> min) (\s@ReportGroupTrendStats' {} a -> s {min = a} :: ReportGroupTrendStats)

-- | Contains the maximum value analyzed.
reportGroupTrendStats_max :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_max = Lens.lens (\ReportGroupTrendStats' {max} -> max) (\s@ReportGroupTrendStats' {} a -> s {max = a} :: ReportGroupTrendStats)

-- | Contains the average of all values analyzed.
reportGroupTrendStats_average :: Lens.Lens' ReportGroupTrendStats (Prelude.Maybe Prelude.Text)
reportGroupTrendStats_average = Lens.lens (\ReportGroupTrendStats' {average} -> average) (\s@ReportGroupTrendStats' {} a -> s {average = a} :: ReportGroupTrendStats)

instance Prelude.FromJSON ReportGroupTrendStats where
  parseJSON =
    Prelude.withObject
      "ReportGroupTrendStats"
      ( \x ->
          ReportGroupTrendStats'
            Prelude.<$> (x Prelude..:? "min")
            Prelude.<*> (x Prelude..:? "max")
            Prelude.<*> (x Prelude..:? "average")
      )

instance Prelude.Hashable ReportGroupTrendStats

instance Prelude.NFData ReportGroupTrendStats
