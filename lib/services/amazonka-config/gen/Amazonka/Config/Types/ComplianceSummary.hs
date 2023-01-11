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
-- Module      : Amazonka.Config.Types.ComplianceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ComplianceSummary where

import Amazonka.Config.Types.ComplianceContributorCount
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of Config rules or Amazon Web Services resources that are
-- compliant and noncompliant.
--
-- /See:/ 'newComplianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
  { -- | The time that Config created the compliance summary.
    complianceSummaryTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The number of Config rules or Amazon Web Services resources that are
    -- compliant, up to a maximum of 25 for rules and 100 for resources.
    compliantResourceCount :: Prelude.Maybe ComplianceContributorCount,
    -- | The number of Config rules or Amazon Web Services resources that are
    -- noncompliant, up to a maximum of 25 for rules and 100 for resources.
    nonCompliantResourceCount :: Prelude.Maybe ComplianceContributorCount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummaryTimestamp', 'complianceSummary_complianceSummaryTimestamp' - The time that Config created the compliance summary.
--
-- 'compliantResourceCount', 'complianceSummary_compliantResourceCount' - The number of Config rules or Amazon Web Services resources that are
-- compliant, up to a maximum of 25 for rules and 100 for resources.
--
-- 'nonCompliantResourceCount', 'complianceSummary_nonCompliantResourceCount' - The number of Config rules or Amazon Web Services resources that are
-- noncompliant, up to a maximum of 25 for rules and 100 for resources.
newComplianceSummary ::
  ComplianceSummary
newComplianceSummary =
  ComplianceSummary'
    { complianceSummaryTimestamp =
        Prelude.Nothing,
      compliantResourceCount = Prelude.Nothing,
      nonCompliantResourceCount = Prelude.Nothing
    }

-- | The time that Config created the compliance summary.
complianceSummary_complianceSummaryTimestamp :: Lens.Lens' ComplianceSummary (Prelude.Maybe Prelude.UTCTime)
complianceSummary_complianceSummaryTimestamp = Lens.lens (\ComplianceSummary' {complianceSummaryTimestamp} -> complianceSummaryTimestamp) (\s@ComplianceSummary' {} a -> s {complianceSummaryTimestamp = a} :: ComplianceSummary) Prelude.. Lens.mapping Data._Time

-- | The number of Config rules or Amazon Web Services resources that are
-- compliant, up to a maximum of 25 for rules and 100 for resources.
complianceSummary_compliantResourceCount :: Lens.Lens' ComplianceSummary (Prelude.Maybe ComplianceContributorCount)
complianceSummary_compliantResourceCount = Lens.lens (\ComplianceSummary' {compliantResourceCount} -> compliantResourceCount) (\s@ComplianceSummary' {} a -> s {compliantResourceCount = a} :: ComplianceSummary)

-- | The number of Config rules or Amazon Web Services resources that are
-- noncompliant, up to a maximum of 25 for rules and 100 for resources.
complianceSummary_nonCompliantResourceCount :: Lens.Lens' ComplianceSummary (Prelude.Maybe ComplianceContributorCount)
complianceSummary_nonCompliantResourceCount = Lens.lens (\ComplianceSummary' {nonCompliantResourceCount} -> nonCompliantResourceCount) (\s@ComplianceSummary' {} a -> s {nonCompliantResourceCount = a} :: ComplianceSummary)

instance Data.FromJSON ComplianceSummary where
  parseJSON =
    Data.withObject
      "ComplianceSummary"
      ( \x ->
          ComplianceSummary'
            Prelude.<$> (x Data..:? "ComplianceSummaryTimestamp")
            Prelude.<*> (x Data..:? "CompliantResourceCount")
            Prelude.<*> (x Data..:? "NonCompliantResourceCount")
      )

instance Prelude.Hashable ComplianceSummary where
  hashWithSalt _salt ComplianceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` complianceSummaryTimestamp
      `Prelude.hashWithSalt` compliantResourceCount
      `Prelude.hashWithSalt` nonCompliantResourceCount

instance Prelude.NFData ComplianceSummary where
  rnf ComplianceSummary' {..} =
    Prelude.rnf complianceSummaryTimestamp
      `Prelude.seq` Prelude.rnf compliantResourceCount
      `Prelude.seq` Prelude.rnf nonCompliantResourceCount
