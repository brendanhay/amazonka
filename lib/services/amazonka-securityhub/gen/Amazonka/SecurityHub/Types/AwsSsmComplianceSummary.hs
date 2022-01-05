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
-- Module      : Amazonka.SecurityHub.Types.AwsSsmComplianceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSsmComplianceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the details about the compliance status for a patch.
--
-- /See:/ 'newAwsSsmComplianceSummary' smart constructor.
data AwsSsmComplianceSummary = AwsSsmComplianceSummary'
  { -- | The current patch compliance status.
    --
    -- The possible status values are:
    --
    -- -   @COMPLIANT@
    --
    -- -   @NON_COMPLIANT@
    --
    -- -   @UNSPECIFIED_DATA@
    status :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @LOW@.
    nonCompliantLowCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @HIGH@.
    compliantHighCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @UNSPECIFIED@.
    nonCompliantUnspecifiedCount :: Prelude.Maybe Prelude.Int,
    -- | The type of execution that was used determine compliance.
    executionType :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @INFORMATIONAL@.
    compliantInformationalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @HIGH@.
    nonCompliantHighCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @MEDIUM@.
    nonCompliantMediumCount :: Prelude.Maybe Prelude.Int,
    -- | The highest severity for the patches.
    overallSeverity :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @CRITICAL@.
    compliantCriticalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @INFORMATIONAL@.
    nonCompliantInformationalCount :: Prelude.Maybe Prelude.Int,
    -- | The type of resource for which the compliance was determined. For
    -- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @UNSPECIFIED@.
    compliantUnspecifiedCount :: Prelude.Maybe Prelude.Int,
    -- | For the patch items that are noncompliant, the number of items that have
    -- a severity of @CRITICAL@.
    nonCompliantCriticalCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the patch baseline. The patch baseline lists the
    -- patches that are approved for installation.
    patchBaselineId :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @LOW@.
    compliantLowCount :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the patch group for which compliance was determined. A
    -- patch group uses tags to group EC2 instances that should have the same
    -- patch compliance.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @MEDIUM@.
    compliantMediumCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSsmComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsSsmComplianceSummary_status' - The current patch compliance status.
--
-- The possible status values are:
--
-- -   @COMPLIANT@
--
-- -   @NON_COMPLIANT@
--
-- -   @UNSPECIFIED_DATA@
--
-- 'nonCompliantLowCount', 'awsSsmComplianceSummary_nonCompliantLowCount' - For the patches that are noncompliant, the number that have a severity
-- of @LOW@.
--
-- 'compliantHighCount', 'awsSsmComplianceSummary_compliantHighCount' - For the patches that are compliant, the number that have a severity of
-- @HIGH@.
--
-- 'nonCompliantUnspecifiedCount', 'awsSsmComplianceSummary_nonCompliantUnspecifiedCount' - For the patches that are noncompliant, the number that have a severity
-- of @UNSPECIFIED@.
--
-- 'executionType', 'awsSsmComplianceSummary_executionType' - The type of execution that was used determine compliance.
--
-- 'compliantInformationalCount', 'awsSsmComplianceSummary_compliantInformationalCount' - For the patches that are compliant, the number that have a severity of
-- @INFORMATIONAL@.
--
-- 'nonCompliantHighCount', 'awsSsmComplianceSummary_nonCompliantHighCount' - For the patches that are noncompliant, the number that have a severity
-- of @HIGH@.
--
-- 'nonCompliantMediumCount', 'awsSsmComplianceSummary_nonCompliantMediumCount' - For the patches that are noncompliant, the number that have a severity
-- of @MEDIUM@.
--
-- 'overallSeverity', 'awsSsmComplianceSummary_overallSeverity' - The highest severity for the patches.
--
-- 'compliantCriticalCount', 'awsSsmComplianceSummary_compliantCriticalCount' - For the patches that are compliant, the number that have a severity of
-- @CRITICAL@.
--
-- 'nonCompliantInformationalCount', 'awsSsmComplianceSummary_nonCompliantInformationalCount' - For the patches that are noncompliant, the number that have a severity
-- of @INFORMATIONAL@.
--
-- 'complianceType', 'awsSsmComplianceSummary_complianceType' - The type of resource for which the compliance was determined. For
-- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
--
-- 'compliantUnspecifiedCount', 'awsSsmComplianceSummary_compliantUnspecifiedCount' - For the patches that are compliant, the number that have a severity of
-- @UNSPECIFIED@.
--
-- 'nonCompliantCriticalCount', 'awsSsmComplianceSummary_nonCompliantCriticalCount' - For the patch items that are noncompliant, the number of items that have
-- a severity of @CRITICAL@.
--
-- 'patchBaselineId', 'awsSsmComplianceSummary_patchBaselineId' - The identifier of the patch baseline. The patch baseline lists the
-- patches that are approved for installation.
--
-- 'compliantLowCount', 'awsSsmComplianceSummary_compliantLowCount' - For the patches that are compliant, the number that have a severity of
-- @LOW@.
--
-- 'patchGroup', 'awsSsmComplianceSummary_patchGroup' - The identifier of the patch group for which compliance was determined. A
-- patch group uses tags to group EC2 instances that should have the same
-- patch compliance.
--
-- 'compliantMediumCount', 'awsSsmComplianceSummary_compliantMediumCount' - For the patches that are compliant, the number that have a severity of
-- @MEDIUM@.
newAwsSsmComplianceSummary ::
  AwsSsmComplianceSummary
newAwsSsmComplianceSummary =
  AwsSsmComplianceSummary'
    { status = Prelude.Nothing,
      nonCompliantLowCount = Prelude.Nothing,
      compliantHighCount = Prelude.Nothing,
      nonCompliantUnspecifiedCount = Prelude.Nothing,
      executionType = Prelude.Nothing,
      compliantInformationalCount = Prelude.Nothing,
      nonCompliantHighCount = Prelude.Nothing,
      nonCompliantMediumCount = Prelude.Nothing,
      overallSeverity = Prelude.Nothing,
      compliantCriticalCount = Prelude.Nothing,
      nonCompliantInformationalCount = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      compliantUnspecifiedCount = Prelude.Nothing,
      nonCompliantCriticalCount = Prelude.Nothing,
      patchBaselineId = Prelude.Nothing,
      compliantLowCount = Prelude.Nothing,
      patchGroup = Prelude.Nothing,
      compliantMediumCount = Prelude.Nothing
    }

-- | The current patch compliance status.
--
-- The possible status values are:
--
-- -   @COMPLIANT@
--
-- -   @NON_COMPLIANT@
--
-- -   @UNSPECIFIED_DATA@
awsSsmComplianceSummary_status :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_status = Lens.lens (\AwsSsmComplianceSummary' {status} -> status) (\s@AwsSsmComplianceSummary' {} a -> s {status = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @LOW@.
awsSsmComplianceSummary_nonCompliantLowCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantLowCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantLowCount} -> nonCompliantLowCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantLowCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @HIGH@.
awsSsmComplianceSummary_compliantHighCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantHighCount = Lens.lens (\AwsSsmComplianceSummary' {compliantHighCount} -> compliantHighCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantHighCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @UNSPECIFIED@.
awsSsmComplianceSummary_nonCompliantUnspecifiedCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantUnspecifiedCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantUnspecifiedCount} -> nonCompliantUnspecifiedCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantUnspecifiedCount = a} :: AwsSsmComplianceSummary)

-- | The type of execution that was used determine compliance.
awsSsmComplianceSummary_executionType :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_executionType = Lens.lens (\AwsSsmComplianceSummary' {executionType} -> executionType) (\s@AwsSsmComplianceSummary' {} a -> s {executionType = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @INFORMATIONAL@.
awsSsmComplianceSummary_compliantInformationalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantInformationalCount = Lens.lens (\AwsSsmComplianceSummary' {compliantInformationalCount} -> compliantInformationalCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantInformationalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @HIGH@.
awsSsmComplianceSummary_nonCompliantHighCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantHighCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantHighCount} -> nonCompliantHighCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantHighCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @MEDIUM@.
awsSsmComplianceSummary_nonCompliantMediumCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantMediumCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantMediumCount} -> nonCompliantMediumCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantMediumCount = a} :: AwsSsmComplianceSummary)

-- | The highest severity for the patches.
awsSsmComplianceSummary_overallSeverity :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_overallSeverity = Lens.lens (\AwsSsmComplianceSummary' {overallSeverity} -> overallSeverity) (\s@AwsSsmComplianceSummary' {} a -> s {overallSeverity = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @CRITICAL@.
awsSsmComplianceSummary_compliantCriticalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantCriticalCount = Lens.lens (\AwsSsmComplianceSummary' {compliantCriticalCount} -> compliantCriticalCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantCriticalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @INFORMATIONAL@.
awsSsmComplianceSummary_nonCompliantInformationalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantInformationalCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantInformationalCount} -> nonCompliantInformationalCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantInformationalCount = a} :: AwsSsmComplianceSummary)

-- | The type of resource for which the compliance was determined. For
-- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
awsSsmComplianceSummary_complianceType :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_complianceType = Lens.lens (\AwsSsmComplianceSummary' {complianceType} -> complianceType) (\s@AwsSsmComplianceSummary' {} a -> s {complianceType = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @UNSPECIFIED@.
awsSsmComplianceSummary_compliantUnspecifiedCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantUnspecifiedCount = Lens.lens (\AwsSsmComplianceSummary' {compliantUnspecifiedCount} -> compliantUnspecifiedCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantUnspecifiedCount = a} :: AwsSsmComplianceSummary)

-- | For the patch items that are noncompliant, the number of items that have
-- a severity of @CRITICAL@.
awsSsmComplianceSummary_nonCompliantCriticalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantCriticalCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantCriticalCount} -> nonCompliantCriticalCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantCriticalCount = a} :: AwsSsmComplianceSummary)

-- | The identifier of the patch baseline. The patch baseline lists the
-- patches that are approved for installation.
awsSsmComplianceSummary_patchBaselineId :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_patchBaselineId = Lens.lens (\AwsSsmComplianceSummary' {patchBaselineId} -> patchBaselineId) (\s@AwsSsmComplianceSummary' {} a -> s {patchBaselineId = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @LOW@.
awsSsmComplianceSummary_compliantLowCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantLowCount = Lens.lens (\AwsSsmComplianceSummary' {compliantLowCount} -> compliantLowCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantLowCount = a} :: AwsSsmComplianceSummary)

-- | The identifier of the patch group for which compliance was determined. A
-- patch group uses tags to group EC2 instances that should have the same
-- patch compliance.
awsSsmComplianceSummary_patchGroup :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_patchGroup = Lens.lens (\AwsSsmComplianceSummary' {patchGroup} -> patchGroup) (\s@AwsSsmComplianceSummary' {} a -> s {patchGroup = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @MEDIUM@.
awsSsmComplianceSummary_compliantMediumCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantMediumCount = Lens.lens (\AwsSsmComplianceSummary' {compliantMediumCount} -> compliantMediumCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantMediumCount = a} :: AwsSsmComplianceSummary)

instance Core.FromJSON AwsSsmComplianceSummary where
  parseJSON =
    Core.withObject
      "AwsSsmComplianceSummary"
      ( \x ->
          AwsSsmComplianceSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "NonCompliantLowCount")
            Prelude.<*> (x Core..:? "CompliantHighCount")
            Prelude.<*> (x Core..:? "NonCompliantUnspecifiedCount")
            Prelude.<*> (x Core..:? "ExecutionType")
            Prelude.<*> (x Core..:? "CompliantInformationalCount")
            Prelude.<*> (x Core..:? "NonCompliantHighCount")
            Prelude.<*> (x Core..:? "NonCompliantMediumCount")
            Prelude.<*> (x Core..:? "OverallSeverity")
            Prelude.<*> (x Core..:? "CompliantCriticalCount")
            Prelude.<*> (x Core..:? "NonCompliantInformationalCount")
            Prelude.<*> (x Core..:? "ComplianceType")
            Prelude.<*> (x Core..:? "CompliantUnspecifiedCount")
            Prelude.<*> (x Core..:? "NonCompliantCriticalCount")
            Prelude.<*> (x Core..:? "PatchBaselineId")
            Prelude.<*> (x Core..:? "CompliantLowCount")
            Prelude.<*> (x Core..:? "PatchGroup")
            Prelude.<*> (x Core..:? "CompliantMediumCount")
      )

instance Prelude.Hashable AwsSsmComplianceSummary where
  hashWithSalt _salt AwsSsmComplianceSummary' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` nonCompliantLowCount
      `Prelude.hashWithSalt` compliantHighCount
      `Prelude.hashWithSalt` nonCompliantUnspecifiedCount
      `Prelude.hashWithSalt` executionType
      `Prelude.hashWithSalt` compliantInformationalCount
      `Prelude.hashWithSalt` nonCompliantHighCount
      `Prelude.hashWithSalt` nonCompliantMediumCount
      `Prelude.hashWithSalt` overallSeverity
      `Prelude.hashWithSalt` compliantCriticalCount
      `Prelude.hashWithSalt` nonCompliantInformationalCount
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` compliantUnspecifiedCount
      `Prelude.hashWithSalt` nonCompliantCriticalCount
      `Prelude.hashWithSalt` patchBaselineId
      `Prelude.hashWithSalt` compliantLowCount
      `Prelude.hashWithSalt` patchGroup
      `Prelude.hashWithSalt` compliantMediumCount

instance Prelude.NFData AwsSsmComplianceSummary where
  rnf AwsSsmComplianceSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf nonCompliantLowCount
      `Prelude.seq` Prelude.rnf compliantHighCount
      `Prelude.seq` Prelude.rnf nonCompliantUnspecifiedCount
      `Prelude.seq` Prelude.rnf executionType
      `Prelude.seq` Prelude.rnf compliantInformationalCount
      `Prelude.seq` Prelude.rnf nonCompliantHighCount
      `Prelude.seq` Prelude.rnf nonCompliantMediumCount
      `Prelude.seq` Prelude.rnf overallSeverity
      `Prelude.seq` Prelude.rnf compliantCriticalCount
      `Prelude.seq` Prelude.rnf nonCompliantInformationalCount
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf compliantUnspecifiedCount
      `Prelude.seq` Prelude.rnf nonCompliantCriticalCount
      `Prelude.seq` Prelude.rnf patchBaselineId
      `Prelude.seq` Prelude.rnf compliantLowCount
      `Prelude.seq` Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf compliantMediumCount

instance Core.ToJSON AwsSsmComplianceSummary where
  toJSON AwsSsmComplianceSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("NonCompliantLowCount" Core..=)
              Prelude.<$> nonCompliantLowCount,
            ("CompliantHighCount" Core..=)
              Prelude.<$> compliantHighCount,
            ("NonCompliantUnspecifiedCount" Core..=)
              Prelude.<$> nonCompliantUnspecifiedCount,
            ("ExecutionType" Core..=) Prelude.<$> executionType,
            ("CompliantInformationalCount" Core..=)
              Prelude.<$> compliantInformationalCount,
            ("NonCompliantHighCount" Core..=)
              Prelude.<$> nonCompliantHighCount,
            ("NonCompliantMediumCount" Core..=)
              Prelude.<$> nonCompliantMediumCount,
            ("OverallSeverity" Core..=)
              Prelude.<$> overallSeverity,
            ("CompliantCriticalCount" Core..=)
              Prelude.<$> compliantCriticalCount,
            ("NonCompliantInformationalCount" Core..=)
              Prelude.<$> nonCompliantInformationalCount,
            ("ComplianceType" Core..=)
              Prelude.<$> complianceType,
            ("CompliantUnspecifiedCount" Core..=)
              Prelude.<$> compliantUnspecifiedCount,
            ("NonCompliantCriticalCount" Core..=)
              Prelude.<$> nonCompliantCriticalCount,
            ("PatchBaselineId" Core..=)
              Prelude.<$> patchBaselineId,
            ("CompliantLowCount" Core..=)
              Prelude.<$> compliantLowCount,
            ("PatchGroup" Core..=) Prelude.<$> patchGroup,
            ("CompliantMediumCount" Core..=)
              Prelude.<$> compliantMediumCount
          ]
      )
