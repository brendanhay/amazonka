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
-- Module      : Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AntipatternSeveritySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.Severity
import qualified Amazonka.Prelude as Prelude

-- | Contains the summary of anti-patterns and their severity.
--
-- /See:/ 'newAntipatternSeveritySummary' smart constructor.
data AntipatternSeveritySummary = AntipatternSeveritySummary'
  { -- | Contains the count of anti-patterns.
    count :: Prelude.Maybe Prelude.Int,
    -- | Contains the severity of anti-patterns.
    severity :: Prelude.Maybe Severity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntipatternSeveritySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'antipatternSeveritySummary_count' - Contains the count of anti-patterns.
--
-- 'severity', 'antipatternSeveritySummary_severity' - Contains the severity of anti-patterns.
newAntipatternSeveritySummary ::
  AntipatternSeveritySummary
newAntipatternSeveritySummary =
  AntipatternSeveritySummary'
    { count =
        Prelude.Nothing,
      severity = Prelude.Nothing
    }

-- | Contains the count of anti-patterns.
antipatternSeveritySummary_count :: Lens.Lens' AntipatternSeveritySummary (Prelude.Maybe Prelude.Int)
antipatternSeveritySummary_count = Lens.lens (\AntipatternSeveritySummary' {count} -> count) (\s@AntipatternSeveritySummary' {} a -> s {count = a} :: AntipatternSeveritySummary)

-- | Contains the severity of anti-patterns.
antipatternSeveritySummary_severity :: Lens.Lens' AntipatternSeveritySummary (Prelude.Maybe Severity)
antipatternSeveritySummary_severity = Lens.lens (\AntipatternSeveritySummary' {severity} -> severity) (\s@AntipatternSeveritySummary' {} a -> s {severity = a} :: AntipatternSeveritySummary)

instance Data.FromJSON AntipatternSeveritySummary where
  parseJSON =
    Data.withObject
      "AntipatternSeveritySummary"
      ( \x ->
          AntipatternSeveritySummary'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "severity")
      )

instance Prelude.Hashable AntipatternSeveritySummary where
  hashWithSalt _salt AntipatternSeveritySummary' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` severity

instance Prelude.NFData AntipatternSeveritySummary where
  rnf AntipatternSeveritySummary' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf severity
