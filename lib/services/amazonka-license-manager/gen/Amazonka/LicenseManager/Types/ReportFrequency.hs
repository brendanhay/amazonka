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
-- Module      : Amazonka.LicenseManager.Types.ReportFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ReportFrequency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types.ReportFrequencyType
import qualified Amazonka.Prelude as Prelude

-- | Details about how frequently reports are generated.
--
-- /See:/ 'newReportFrequency' smart constructor.
data ReportFrequency = ReportFrequency'
  { -- | Time period between each report. The period can be daily, weekly, or
    -- monthly.
    period :: Prelude.Maybe ReportFrequencyType,
    -- | Number of times within the frequency period that a report is generated.
    -- The only supported value is @1@.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportFrequency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'period', 'reportFrequency_period' - Time period between each report. The period can be daily, weekly, or
-- monthly.
--
-- 'value', 'reportFrequency_value' - Number of times within the frequency period that a report is generated.
-- The only supported value is @1@.
newReportFrequency ::
  ReportFrequency
newReportFrequency =
  ReportFrequency'
    { period = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Time period between each report. The period can be daily, weekly, or
-- monthly.
reportFrequency_period :: Lens.Lens' ReportFrequency (Prelude.Maybe ReportFrequencyType)
reportFrequency_period = Lens.lens (\ReportFrequency' {period} -> period) (\s@ReportFrequency' {} a -> s {period = a} :: ReportFrequency)

-- | Number of times within the frequency period that a report is generated.
-- The only supported value is @1@.
reportFrequency_value :: Lens.Lens' ReportFrequency (Prelude.Maybe Prelude.Int)
reportFrequency_value = Lens.lens (\ReportFrequency' {value} -> value) (\s@ReportFrequency' {} a -> s {value = a} :: ReportFrequency)

instance Data.FromJSON ReportFrequency where
  parseJSON =
    Data.withObject
      "ReportFrequency"
      ( \x ->
          ReportFrequency'
            Prelude.<$> (x Data..:? "period")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ReportFrequency where
  hashWithSalt _salt ReportFrequency' {..} =
    _salt `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReportFrequency where
  rnf ReportFrequency' {..} =
    Prelude.rnf period `Prelude.seq` Prelude.rnf value

instance Data.ToJSON ReportFrequency where
  toJSON ReportFrequency' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("period" Data..=) Prelude.<$> period,
            ("value" Data..=) Prelude.<$> value
          ]
      )
