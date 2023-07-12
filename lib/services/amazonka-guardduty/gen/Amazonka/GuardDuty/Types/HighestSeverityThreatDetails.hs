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
-- Module      : Amazonka.GuardDuty.Types.HighestSeverityThreatDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.HighestSeverityThreatDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details of the highest severity threat detected during scan and
-- number of infected files.
--
-- /See:/ 'newHighestSeverityThreatDetails' smart constructor.
data HighestSeverityThreatDetails = HighestSeverityThreatDetails'
  { -- | Total number of infected files with the highest severity threat
    -- detected.
    count :: Prelude.Maybe Prelude.Int,
    -- | Severity level of the highest severity threat detected.
    severity :: Prelude.Maybe Prelude.Text,
    -- | Threat name of the highest severity threat detected as part of the
    -- malware scan.
    threatName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HighestSeverityThreatDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'highestSeverityThreatDetails_count' - Total number of infected files with the highest severity threat
-- detected.
--
-- 'severity', 'highestSeverityThreatDetails_severity' - Severity level of the highest severity threat detected.
--
-- 'threatName', 'highestSeverityThreatDetails_threatName' - Threat name of the highest severity threat detected as part of the
-- malware scan.
newHighestSeverityThreatDetails ::
  HighestSeverityThreatDetails
newHighestSeverityThreatDetails =
  HighestSeverityThreatDetails'
    { count =
        Prelude.Nothing,
      severity = Prelude.Nothing,
      threatName = Prelude.Nothing
    }

-- | Total number of infected files with the highest severity threat
-- detected.
highestSeverityThreatDetails_count :: Lens.Lens' HighestSeverityThreatDetails (Prelude.Maybe Prelude.Int)
highestSeverityThreatDetails_count = Lens.lens (\HighestSeverityThreatDetails' {count} -> count) (\s@HighestSeverityThreatDetails' {} a -> s {count = a} :: HighestSeverityThreatDetails)

-- | Severity level of the highest severity threat detected.
highestSeverityThreatDetails_severity :: Lens.Lens' HighestSeverityThreatDetails (Prelude.Maybe Prelude.Text)
highestSeverityThreatDetails_severity = Lens.lens (\HighestSeverityThreatDetails' {severity} -> severity) (\s@HighestSeverityThreatDetails' {} a -> s {severity = a} :: HighestSeverityThreatDetails)

-- | Threat name of the highest severity threat detected as part of the
-- malware scan.
highestSeverityThreatDetails_threatName :: Lens.Lens' HighestSeverityThreatDetails (Prelude.Maybe Prelude.Text)
highestSeverityThreatDetails_threatName = Lens.lens (\HighestSeverityThreatDetails' {threatName} -> threatName) (\s@HighestSeverityThreatDetails' {} a -> s {threatName = a} :: HighestSeverityThreatDetails)

instance Data.FromJSON HighestSeverityThreatDetails where
  parseJSON =
    Data.withObject
      "HighestSeverityThreatDetails"
      ( \x ->
          HighestSeverityThreatDetails'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "severity")
            Prelude.<*> (x Data..:? "threatName")
      )

instance
  Prelude.Hashable
    HighestSeverityThreatDetails
  where
  hashWithSalt _salt HighestSeverityThreatDetails' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` threatName

instance Prelude.NFData HighestSeverityThreatDetails where
  rnf HighestSeverityThreatDetails' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf threatName
