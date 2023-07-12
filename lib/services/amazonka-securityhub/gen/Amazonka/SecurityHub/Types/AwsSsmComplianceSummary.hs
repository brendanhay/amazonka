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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSsmComplianceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details about the compliance status for a patch.
--
-- /See:/ 'newAwsSsmComplianceSummary' smart constructor.
data AwsSsmComplianceSummary = AwsSsmComplianceSummary'
  { -- | The type of resource for which the compliance was determined. For
    -- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | For the patches that are compliant, the number that have a severity of
    -- @CRITICAL@.
    compliantCriticalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @HIGH@.
    compliantHighCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @INFORMATIONAL@.
    compliantInformationalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @LOW@.
    compliantLowCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @MEDIUM@.
    compliantMediumCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are compliant, the number that have a severity of
    -- @UNSPECIFIED@.
    compliantUnspecifiedCount :: Prelude.Maybe Prelude.Int,
    -- | The type of execution that was used determine compliance.
    executionType :: Prelude.Maybe Prelude.Text,
    -- | For the patch items that are noncompliant, the number of items that have
    -- a severity of @CRITICAL@.
    nonCompliantCriticalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @HIGH@.
    nonCompliantHighCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @INFORMATIONAL@.
    nonCompliantInformationalCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @LOW@.
    nonCompliantLowCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @MEDIUM@.
    nonCompliantMediumCount :: Prelude.Maybe Prelude.Int,
    -- | For the patches that are noncompliant, the number that have a severity
    -- of @UNSPECIFIED@.
    nonCompliantUnspecifiedCount :: Prelude.Maybe Prelude.Int,
    -- | The highest severity for the patches. Valid values are as follows:
    --
    -- -   @CRITICAL@
    --
    -- -   @HIGH@
    --
    -- -   @MEDIUM@
    --
    -- -   @LOW@
    --
    -- -   @INFORMATIONAL@
    --
    -- -   @UNSPECIFIED@
    overallSeverity :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the patch baseline. The patch baseline lists the
    -- patches that are approved for installation.
    patchBaselineId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the patch group for which compliance was determined. A
    -- patch group uses tags to group EC2 instances that should have the same
    -- patch compliance.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | The current patch compliance status. Valid values are as follows:
    --
    -- -   @COMPLIANT@
    --
    -- -   @NON_COMPLIANT@
    --
    -- -   @UNSPECIFIED_DATA@
    status :: Prelude.Maybe Prelude.Text
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
-- 'complianceType', 'awsSsmComplianceSummary_complianceType' - The type of resource for which the compliance was determined. For
-- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
--
-- 'compliantCriticalCount', 'awsSsmComplianceSummary_compliantCriticalCount' - For the patches that are compliant, the number that have a severity of
-- @CRITICAL@.
--
-- 'compliantHighCount', 'awsSsmComplianceSummary_compliantHighCount' - For the patches that are compliant, the number that have a severity of
-- @HIGH@.
--
-- 'compliantInformationalCount', 'awsSsmComplianceSummary_compliantInformationalCount' - For the patches that are compliant, the number that have a severity of
-- @INFORMATIONAL@.
--
-- 'compliantLowCount', 'awsSsmComplianceSummary_compliantLowCount' - For the patches that are compliant, the number that have a severity of
-- @LOW@.
--
-- 'compliantMediumCount', 'awsSsmComplianceSummary_compliantMediumCount' - For the patches that are compliant, the number that have a severity of
-- @MEDIUM@.
--
-- 'compliantUnspecifiedCount', 'awsSsmComplianceSummary_compliantUnspecifiedCount' - For the patches that are compliant, the number that have a severity of
-- @UNSPECIFIED@.
--
-- 'executionType', 'awsSsmComplianceSummary_executionType' - The type of execution that was used determine compliance.
--
-- 'nonCompliantCriticalCount', 'awsSsmComplianceSummary_nonCompliantCriticalCount' - For the patch items that are noncompliant, the number of items that have
-- a severity of @CRITICAL@.
--
-- 'nonCompliantHighCount', 'awsSsmComplianceSummary_nonCompliantHighCount' - For the patches that are noncompliant, the number that have a severity
-- of @HIGH@.
--
-- 'nonCompliantInformationalCount', 'awsSsmComplianceSummary_nonCompliantInformationalCount' - For the patches that are noncompliant, the number that have a severity
-- of @INFORMATIONAL@.
--
-- 'nonCompliantLowCount', 'awsSsmComplianceSummary_nonCompliantLowCount' - For the patches that are noncompliant, the number that have a severity
-- of @LOW@.
--
-- 'nonCompliantMediumCount', 'awsSsmComplianceSummary_nonCompliantMediumCount' - For the patches that are noncompliant, the number that have a severity
-- of @MEDIUM@.
--
-- 'nonCompliantUnspecifiedCount', 'awsSsmComplianceSummary_nonCompliantUnspecifiedCount' - For the patches that are noncompliant, the number that have a severity
-- of @UNSPECIFIED@.
--
-- 'overallSeverity', 'awsSsmComplianceSummary_overallSeverity' - The highest severity for the patches. Valid values are as follows:
--
-- -   @CRITICAL@
--
-- -   @HIGH@
--
-- -   @MEDIUM@
--
-- -   @LOW@
--
-- -   @INFORMATIONAL@
--
-- -   @UNSPECIFIED@
--
-- 'patchBaselineId', 'awsSsmComplianceSummary_patchBaselineId' - The identifier of the patch baseline. The patch baseline lists the
-- patches that are approved for installation.
--
-- 'patchGroup', 'awsSsmComplianceSummary_patchGroup' - The identifier of the patch group for which compliance was determined. A
-- patch group uses tags to group EC2 instances that should have the same
-- patch compliance.
--
-- 'status', 'awsSsmComplianceSummary_status' - The current patch compliance status. Valid values are as follows:
--
-- -   @COMPLIANT@
--
-- -   @NON_COMPLIANT@
--
-- -   @UNSPECIFIED_DATA@
newAwsSsmComplianceSummary ::
  AwsSsmComplianceSummary
newAwsSsmComplianceSummary =
  AwsSsmComplianceSummary'
    { complianceType =
        Prelude.Nothing,
      compliantCriticalCount = Prelude.Nothing,
      compliantHighCount = Prelude.Nothing,
      compliantInformationalCount = Prelude.Nothing,
      compliantLowCount = Prelude.Nothing,
      compliantMediumCount = Prelude.Nothing,
      compliantUnspecifiedCount = Prelude.Nothing,
      executionType = Prelude.Nothing,
      nonCompliantCriticalCount = Prelude.Nothing,
      nonCompliantHighCount = Prelude.Nothing,
      nonCompliantInformationalCount = Prelude.Nothing,
      nonCompliantLowCount = Prelude.Nothing,
      nonCompliantMediumCount = Prelude.Nothing,
      nonCompliantUnspecifiedCount = Prelude.Nothing,
      overallSeverity = Prelude.Nothing,
      patchBaselineId = Prelude.Nothing,
      patchGroup = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The type of resource for which the compliance was determined. For
-- @AwsSsmPatchCompliance@, @ComplianceType@ is @Patch@.
awsSsmComplianceSummary_complianceType :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_complianceType = Lens.lens (\AwsSsmComplianceSummary' {complianceType} -> complianceType) (\s@AwsSsmComplianceSummary' {} a -> s {complianceType = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @CRITICAL@.
awsSsmComplianceSummary_compliantCriticalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantCriticalCount = Lens.lens (\AwsSsmComplianceSummary' {compliantCriticalCount} -> compliantCriticalCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantCriticalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @HIGH@.
awsSsmComplianceSummary_compliantHighCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantHighCount = Lens.lens (\AwsSsmComplianceSummary' {compliantHighCount} -> compliantHighCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantHighCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @INFORMATIONAL@.
awsSsmComplianceSummary_compliantInformationalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantInformationalCount = Lens.lens (\AwsSsmComplianceSummary' {compliantInformationalCount} -> compliantInformationalCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantInformationalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @LOW@.
awsSsmComplianceSummary_compliantLowCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantLowCount = Lens.lens (\AwsSsmComplianceSummary' {compliantLowCount} -> compliantLowCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantLowCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @MEDIUM@.
awsSsmComplianceSummary_compliantMediumCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantMediumCount = Lens.lens (\AwsSsmComplianceSummary' {compliantMediumCount} -> compliantMediumCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantMediumCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are compliant, the number that have a severity of
-- @UNSPECIFIED@.
awsSsmComplianceSummary_compliantUnspecifiedCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_compliantUnspecifiedCount = Lens.lens (\AwsSsmComplianceSummary' {compliantUnspecifiedCount} -> compliantUnspecifiedCount) (\s@AwsSsmComplianceSummary' {} a -> s {compliantUnspecifiedCount = a} :: AwsSsmComplianceSummary)

-- | The type of execution that was used determine compliance.
awsSsmComplianceSummary_executionType :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_executionType = Lens.lens (\AwsSsmComplianceSummary' {executionType} -> executionType) (\s@AwsSsmComplianceSummary' {} a -> s {executionType = a} :: AwsSsmComplianceSummary)

-- | For the patch items that are noncompliant, the number of items that have
-- a severity of @CRITICAL@.
awsSsmComplianceSummary_nonCompliantCriticalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantCriticalCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantCriticalCount} -> nonCompliantCriticalCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantCriticalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @HIGH@.
awsSsmComplianceSummary_nonCompliantHighCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantHighCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantHighCount} -> nonCompliantHighCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantHighCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @INFORMATIONAL@.
awsSsmComplianceSummary_nonCompliantInformationalCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantInformationalCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantInformationalCount} -> nonCompliantInformationalCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantInformationalCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @LOW@.
awsSsmComplianceSummary_nonCompliantLowCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantLowCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantLowCount} -> nonCompliantLowCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantLowCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @MEDIUM@.
awsSsmComplianceSummary_nonCompliantMediumCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantMediumCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantMediumCount} -> nonCompliantMediumCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantMediumCount = a} :: AwsSsmComplianceSummary)

-- | For the patches that are noncompliant, the number that have a severity
-- of @UNSPECIFIED@.
awsSsmComplianceSummary_nonCompliantUnspecifiedCount :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Int)
awsSsmComplianceSummary_nonCompliantUnspecifiedCount = Lens.lens (\AwsSsmComplianceSummary' {nonCompliantUnspecifiedCount} -> nonCompliantUnspecifiedCount) (\s@AwsSsmComplianceSummary' {} a -> s {nonCompliantUnspecifiedCount = a} :: AwsSsmComplianceSummary)

-- | The highest severity for the patches. Valid values are as follows:
--
-- -   @CRITICAL@
--
-- -   @HIGH@
--
-- -   @MEDIUM@
--
-- -   @LOW@
--
-- -   @INFORMATIONAL@
--
-- -   @UNSPECIFIED@
awsSsmComplianceSummary_overallSeverity :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_overallSeverity = Lens.lens (\AwsSsmComplianceSummary' {overallSeverity} -> overallSeverity) (\s@AwsSsmComplianceSummary' {} a -> s {overallSeverity = a} :: AwsSsmComplianceSummary)

-- | The identifier of the patch baseline. The patch baseline lists the
-- patches that are approved for installation.
awsSsmComplianceSummary_patchBaselineId :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_patchBaselineId = Lens.lens (\AwsSsmComplianceSummary' {patchBaselineId} -> patchBaselineId) (\s@AwsSsmComplianceSummary' {} a -> s {patchBaselineId = a} :: AwsSsmComplianceSummary)

-- | The identifier of the patch group for which compliance was determined. A
-- patch group uses tags to group EC2 instances that should have the same
-- patch compliance.
awsSsmComplianceSummary_patchGroup :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_patchGroup = Lens.lens (\AwsSsmComplianceSummary' {patchGroup} -> patchGroup) (\s@AwsSsmComplianceSummary' {} a -> s {patchGroup = a} :: AwsSsmComplianceSummary)

-- | The current patch compliance status. Valid values are as follows:
--
-- -   @COMPLIANT@
--
-- -   @NON_COMPLIANT@
--
-- -   @UNSPECIFIED_DATA@
awsSsmComplianceSummary_status :: Lens.Lens' AwsSsmComplianceSummary (Prelude.Maybe Prelude.Text)
awsSsmComplianceSummary_status = Lens.lens (\AwsSsmComplianceSummary' {status} -> status) (\s@AwsSsmComplianceSummary' {} a -> s {status = a} :: AwsSsmComplianceSummary)

instance Data.FromJSON AwsSsmComplianceSummary where
  parseJSON =
    Data.withObject
      "AwsSsmComplianceSummary"
      ( \x ->
          AwsSsmComplianceSummary'
            Prelude.<$> (x Data..:? "ComplianceType")
            Prelude.<*> (x Data..:? "CompliantCriticalCount")
            Prelude.<*> (x Data..:? "CompliantHighCount")
            Prelude.<*> (x Data..:? "CompliantInformationalCount")
            Prelude.<*> (x Data..:? "CompliantLowCount")
            Prelude.<*> (x Data..:? "CompliantMediumCount")
            Prelude.<*> (x Data..:? "CompliantUnspecifiedCount")
            Prelude.<*> (x Data..:? "ExecutionType")
            Prelude.<*> (x Data..:? "NonCompliantCriticalCount")
            Prelude.<*> (x Data..:? "NonCompliantHighCount")
            Prelude.<*> (x Data..:? "NonCompliantInformationalCount")
            Prelude.<*> (x Data..:? "NonCompliantLowCount")
            Prelude.<*> (x Data..:? "NonCompliantMediumCount")
            Prelude.<*> (x Data..:? "NonCompliantUnspecifiedCount")
            Prelude.<*> (x Data..:? "OverallSeverity")
            Prelude.<*> (x Data..:? "PatchBaselineId")
            Prelude.<*> (x Data..:? "PatchGroup")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AwsSsmComplianceSummary where
  hashWithSalt _salt AwsSsmComplianceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` compliantCriticalCount
      `Prelude.hashWithSalt` compliantHighCount
      `Prelude.hashWithSalt` compliantInformationalCount
      `Prelude.hashWithSalt` compliantLowCount
      `Prelude.hashWithSalt` compliantMediumCount
      `Prelude.hashWithSalt` compliantUnspecifiedCount
      `Prelude.hashWithSalt` executionType
      `Prelude.hashWithSalt` nonCompliantCriticalCount
      `Prelude.hashWithSalt` nonCompliantHighCount
      `Prelude.hashWithSalt` nonCompliantInformationalCount
      `Prelude.hashWithSalt` nonCompliantLowCount
      `Prelude.hashWithSalt` nonCompliantMediumCount
      `Prelude.hashWithSalt` nonCompliantUnspecifiedCount
      `Prelude.hashWithSalt` overallSeverity
      `Prelude.hashWithSalt` patchBaselineId
      `Prelude.hashWithSalt` patchGroup
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsSsmComplianceSummary where
  rnf AwsSsmComplianceSummary' {..} =
    Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf compliantCriticalCount
      `Prelude.seq` Prelude.rnf compliantHighCount
      `Prelude.seq` Prelude.rnf compliantInformationalCount
      `Prelude.seq` Prelude.rnf compliantLowCount
      `Prelude.seq` Prelude.rnf compliantMediumCount
      `Prelude.seq` Prelude.rnf compliantUnspecifiedCount
      `Prelude.seq` Prelude.rnf executionType
      `Prelude.seq` Prelude.rnf nonCompliantCriticalCount
      `Prelude.seq` Prelude.rnf nonCompliantHighCount
      `Prelude.seq` Prelude.rnf nonCompliantInformationalCount
      `Prelude.seq` Prelude.rnf nonCompliantLowCount
      `Prelude.seq` Prelude.rnf nonCompliantMediumCount
      `Prelude.seq` Prelude.rnf nonCompliantUnspecifiedCount
      `Prelude.seq` Prelude.rnf overallSeverity
      `Prelude.seq` Prelude.rnf patchBaselineId
      `Prelude.seq` Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsSsmComplianceSummary where
  toJSON AwsSsmComplianceSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComplianceType" Data..=)
              Prelude.<$> complianceType,
            ("CompliantCriticalCount" Data..=)
              Prelude.<$> compliantCriticalCount,
            ("CompliantHighCount" Data..=)
              Prelude.<$> compliantHighCount,
            ("CompliantInformationalCount" Data..=)
              Prelude.<$> compliantInformationalCount,
            ("CompliantLowCount" Data..=)
              Prelude.<$> compliantLowCount,
            ("CompliantMediumCount" Data..=)
              Prelude.<$> compliantMediumCount,
            ("CompliantUnspecifiedCount" Data..=)
              Prelude.<$> compliantUnspecifiedCount,
            ("ExecutionType" Data..=) Prelude.<$> executionType,
            ("NonCompliantCriticalCount" Data..=)
              Prelude.<$> nonCompliantCriticalCount,
            ("NonCompliantHighCount" Data..=)
              Prelude.<$> nonCompliantHighCount,
            ("NonCompliantInformationalCount" Data..=)
              Prelude.<$> nonCompliantInformationalCount,
            ("NonCompliantLowCount" Data..=)
              Prelude.<$> nonCompliantLowCount,
            ("NonCompliantMediumCount" Data..=)
              Prelude.<$> nonCompliantMediumCount,
            ("NonCompliantUnspecifiedCount" Data..=)
              Prelude.<$> nonCompliantUnspecifiedCount,
            ("OverallSeverity" Data..=)
              Prelude.<$> overallSeverity,
            ("PatchBaselineId" Data..=)
              Prelude.<$> patchBaselineId,
            ("PatchGroup" Data..=) Prelude.<$> patchGroup,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
