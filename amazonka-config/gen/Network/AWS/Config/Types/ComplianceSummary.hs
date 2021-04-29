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
-- Module      : Network.AWS.Config.Types.ComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummary where

import Network.AWS.Config.Types.ComplianceContributorCount
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of AWS Config rules or AWS resources that are compliant and
-- noncompliant.
--
-- /See:/ 'newComplianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
  { -- | The time that AWS Config created the compliance summary.
    complianceSummaryTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The number of AWS Config rules or AWS resources that are noncompliant,
    -- up to a maximum of 25 for rules and 100 for resources.
    nonCompliantResourceCount :: Prelude.Maybe ComplianceContributorCount,
    -- | The number of AWS Config rules or AWS resources that are compliant, up
    -- to a maximum of 25 for rules and 100 for resources.
    compliantResourceCount :: Prelude.Maybe ComplianceContributorCount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummaryTimestamp', 'complianceSummary_complianceSummaryTimestamp' - The time that AWS Config created the compliance summary.
--
-- 'nonCompliantResourceCount', 'complianceSummary_nonCompliantResourceCount' - The number of AWS Config rules or AWS resources that are noncompliant,
-- up to a maximum of 25 for rules and 100 for resources.
--
-- 'compliantResourceCount', 'complianceSummary_compliantResourceCount' - The number of AWS Config rules or AWS resources that are compliant, up
-- to a maximum of 25 for rules and 100 for resources.
newComplianceSummary ::
  ComplianceSummary
newComplianceSummary =
  ComplianceSummary'
    { complianceSummaryTimestamp =
        Prelude.Nothing,
      nonCompliantResourceCount = Prelude.Nothing,
      compliantResourceCount = Prelude.Nothing
    }

-- | The time that AWS Config created the compliance summary.
complianceSummary_complianceSummaryTimestamp :: Lens.Lens' ComplianceSummary (Prelude.Maybe Prelude.UTCTime)
complianceSummary_complianceSummaryTimestamp = Lens.lens (\ComplianceSummary' {complianceSummaryTimestamp} -> complianceSummaryTimestamp) (\s@ComplianceSummary' {} a -> s {complianceSummaryTimestamp = a} :: ComplianceSummary) Prelude.. Lens.mapping Prelude._Time

-- | The number of AWS Config rules or AWS resources that are noncompliant,
-- up to a maximum of 25 for rules and 100 for resources.
complianceSummary_nonCompliantResourceCount :: Lens.Lens' ComplianceSummary (Prelude.Maybe ComplianceContributorCount)
complianceSummary_nonCompliantResourceCount = Lens.lens (\ComplianceSummary' {nonCompliantResourceCount} -> nonCompliantResourceCount) (\s@ComplianceSummary' {} a -> s {nonCompliantResourceCount = a} :: ComplianceSummary)

-- | The number of AWS Config rules or AWS resources that are compliant, up
-- to a maximum of 25 for rules and 100 for resources.
complianceSummary_compliantResourceCount :: Lens.Lens' ComplianceSummary (Prelude.Maybe ComplianceContributorCount)
complianceSummary_compliantResourceCount = Lens.lens (\ComplianceSummary' {compliantResourceCount} -> compliantResourceCount) (\s@ComplianceSummary' {} a -> s {compliantResourceCount = a} :: ComplianceSummary)

instance Prelude.FromJSON ComplianceSummary where
  parseJSON =
    Prelude.withObject
      "ComplianceSummary"
      ( \x ->
          ComplianceSummary'
            Prelude.<$> (x Prelude..:? "ComplianceSummaryTimestamp")
            Prelude.<*> (x Prelude..:? "NonCompliantResourceCount")
            Prelude.<*> (x Prelude..:? "CompliantResourceCount")
      )

instance Prelude.Hashable ComplianceSummary

instance Prelude.NFData ComplianceSummary
