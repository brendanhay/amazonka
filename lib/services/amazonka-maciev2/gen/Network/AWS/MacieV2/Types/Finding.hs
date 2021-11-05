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
-- Module      : Amazonka.MacieV2.Types.Finding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Finding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.ClassificationDetails
import Amazonka.MacieV2.Types.FindingCategory
import Amazonka.MacieV2.Types.FindingType
import Amazonka.MacieV2.Types.PolicyDetails
import Amazonka.MacieV2.Types.ResourcesAffected
import Amazonka.MacieV2.Types.Severity
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of a finding.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | The details of a sensitive data finding. This value is null for a policy
    -- finding.
    classificationDetails :: Prelude.Maybe ClassificationDetails,
    -- | The details of a policy finding. This value is null for a sensitive data
    -- finding.
    policyDetails :: Prelude.Maybe PolicyDetails,
    -- | The date and time, in UTC and extended ISO 8601 format, when the finding
    -- was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The category of the finding. Possible values are: CLASSIFICATION, for a
    -- sensitive data finding; and, POLICY, for a policy finding.
    category :: Prelude.Maybe FindingCategory,
    -- | The severity level and score for the finding.
    severity :: Prelude.Maybe Severity,
    -- | The total number of occurrences of the finding. For sensitive data
    -- findings, this value is always 1. All sensitive data findings are
    -- considered new (unique) because they derive from individual
    -- classification jobs.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The version of the schema that was used to define the data structures in
    -- the finding.
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The resources that the finding applies to.
    resourcesAffected :: Prelude.Maybe ResourcesAffected,
    -- | The unique identifier for the Amazon Web Services account that the
    -- finding applies to. This is typically the account that owns the affected
    -- resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services partition that Amazon Macie created the finding
    -- in.
    partition :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the finding. This is a random string that
    -- Amazon Macie generates and assigns to a finding when it creates the
    -- finding.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that Amazon Macie created the finding in.
    region :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the finding
    -- was last updated. For sensitive data findings, this value is the same as
    -- the value for the createdAt property. All sensitive data findings are
    -- considered new (unique) because they derive from individual
    -- classification jobs.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The brief description of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | The type of the finding.
    type' :: Prelude.Maybe FindingType,
    -- | Specifies whether the finding is archived (suppressed).
    archived :: Prelude.Maybe Prelude.Bool,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the finding is a sample finding. A /sample finding/ is
    -- a finding that uses example data to demonstrate what a finding might
    -- contain.
    sample :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Finding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classificationDetails', 'finding_classificationDetails' - The details of a sensitive data finding. This value is null for a policy
-- finding.
--
-- 'policyDetails', 'finding_policyDetails' - The details of a policy finding. This value is null for a sensitive data
-- finding.
--
-- 'createdAt', 'finding_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the finding
-- was created.
--
-- 'category', 'finding_category' - The category of the finding. Possible values are: CLASSIFICATION, for a
-- sensitive data finding; and, POLICY, for a policy finding.
--
-- 'severity', 'finding_severity' - The severity level and score for the finding.
--
-- 'count', 'finding_count' - The total number of occurrences of the finding. For sensitive data
-- findings, this value is always 1. All sensitive data findings are
-- considered new (unique) because they derive from individual
-- classification jobs.
--
-- 'schemaVersion', 'finding_schemaVersion' - The version of the schema that was used to define the data structures in
-- the finding.
--
-- 'resourcesAffected', 'finding_resourcesAffected' - The resources that the finding applies to.
--
-- 'accountId', 'finding_accountId' - The unique identifier for the Amazon Web Services account that the
-- finding applies to. This is typically the account that owns the affected
-- resource.
--
-- 'partition', 'finding_partition' - The Amazon Web Services partition that Amazon Macie created the finding
-- in.
--
-- 'id', 'finding_id' - The unique identifier for the finding. This is a random string that
-- Amazon Macie generates and assigns to a finding when it creates the
-- finding.
--
-- 'region', 'finding_region' - The Amazon Web Services Region that Amazon Macie created the finding in.
--
-- 'updatedAt', 'finding_updatedAt' - The date and time, in UTC and extended ISO 8601 format, when the finding
-- was last updated. For sensitive data findings, this value is the same as
-- the value for the createdAt property. All sensitive data findings are
-- considered new (unique) because they derive from individual
-- classification jobs.
--
-- 'title', 'finding_title' - The brief description of the finding.
--
-- 'type'', 'finding_type' - The type of the finding.
--
-- 'archived', 'finding_archived' - Specifies whether the finding is archived (suppressed).
--
-- 'description', 'finding_description' - The description of the finding.
--
-- 'sample', 'finding_sample' - Specifies whether the finding is a sample finding. A /sample finding/ is
-- a finding that uses example data to demonstrate what a finding might
-- contain.
newFinding ::
  Finding
newFinding =
  Finding'
    { classificationDetails = Prelude.Nothing,
      policyDetails = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      category = Prelude.Nothing,
      severity = Prelude.Nothing,
      count = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      resourcesAffected = Prelude.Nothing,
      accountId = Prelude.Nothing,
      partition = Prelude.Nothing,
      id = Prelude.Nothing,
      region = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      title = Prelude.Nothing,
      type' = Prelude.Nothing,
      archived = Prelude.Nothing,
      description = Prelude.Nothing,
      sample = Prelude.Nothing
    }

-- | The details of a sensitive data finding. This value is null for a policy
-- finding.
finding_classificationDetails :: Lens.Lens' Finding (Prelude.Maybe ClassificationDetails)
finding_classificationDetails = Lens.lens (\Finding' {classificationDetails} -> classificationDetails) (\s@Finding' {} a -> s {classificationDetails = a} :: Finding)

-- | The details of a policy finding. This value is null for a sensitive data
-- finding.
finding_policyDetails :: Lens.Lens' Finding (Prelude.Maybe PolicyDetails)
finding_policyDetails = Lens.lens (\Finding' {policyDetails} -> policyDetails) (\s@Finding' {} a -> s {policyDetails = a} :: Finding)

-- | The date and time, in UTC and extended ISO 8601 format, when the finding
-- was created.
finding_createdAt :: Lens.Lens' Finding (Prelude.Maybe Prelude.UTCTime)
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Prelude.. Lens.mapping Core._Time

-- | The category of the finding. Possible values are: CLASSIFICATION, for a
-- sensitive data finding; and, POLICY, for a policy finding.
finding_category :: Lens.Lens' Finding (Prelude.Maybe FindingCategory)
finding_category = Lens.lens (\Finding' {category} -> category) (\s@Finding' {} a -> s {category = a} :: Finding)

-- | The severity level and score for the finding.
finding_severity :: Lens.Lens' Finding (Prelude.Maybe Severity)
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The total number of occurrences of the finding. For sensitive data
-- findings, this value is always 1. All sensitive data findings are
-- considered new (unique) because they derive from individual
-- classification jobs.
finding_count :: Lens.Lens' Finding (Prelude.Maybe Prelude.Integer)
finding_count = Lens.lens (\Finding' {count} -> count) (\s@Finding' {} a -> s {count = a} :: Finding)

-- | The version of the schema that was used to define the data structures in
-- the finding.
finding_schemaVersion :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_schemaVersion = Lens.lens (\Finding' {schemaVersion} -> schemaVersion) (\s@Finding' {} a -> s {schemaVersion = a} :: Finding)

-- | The resources that the finding applies to.
finding_resourcesAffected :: Lens.Lens' Finding (Prelude.Maybe ResourcesAffected)
finding_resourcesAffected = Lens.lens (\Finding' {resourcesAffected} -> resourcesAffected) (\s@Finding' {} a -> s {resourcesAffected = a} :: Finding)

-- | The unique identifier for the Amazon Web Services account that the
-- finding applies to. This is typically the account that owns the affected
-- resource.
finding_accountId :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_accountId = Lens.lens (\Finding' {accountId} -> accountId) (\s@Finding' {} a -> s {accountId = a} :: Finding)

-- | The Amazon Web Services partition that Amazon Macie created the finding
-- in.
finding_partition :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_partition = Lens.lens (\Finding' {partition} -> partition) (\s@Finding' {} a -> s {partition = a} :: Finding)

-- | The unique identifier for the finding. This is a random string that
-- Amazon Macie generates and assigns to a finding when it creates the
-- finding.
finding_id :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The Amazon Web Services Region that Amazon Macie created the finding in.
finding_region :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_region = Lens.lens (\Finding' {region} -> region) (\s@Finding' {} a -> s {region = a} :: Finding)

-- | The date and time, in UTC and extended ISO 8601 format, when the finding
-- was last updated. For sensitive data findings, this value is the same as
-- the value for the createdAt property. All sensitive data findings are
-- considered new (unique) because they derive from individual
-- classification jobs.
finding_updatedAt :: Lens.Lens' Finding (Prelude.Maybe Prelude.UTCTime)
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Lens.mapping Core._Time

-- | The brief description of the finding.
finding_title :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | The type of the finding.
finding_type :: Lens.Lens' Finding (Prelude.Maybe FindingType)
finding_type = Lens.lens (\Finding' {type'} -> type') (\s@Finding' {} a -> s {type' = a} :: Finding)

-- | Specifies whether the finding is archived (suppressed).
finding_archived :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_archived = Lens.lens (\Finding' {archived} -> archived) (\s@Finding' {} a -> s {archived = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | Specifies whether the finding is a sample finding. A /sample finding/ is
-- a finding that uses example data to demonstrate what a finding might
-- contain.
finding_sample :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_sample = Lens.lens (\Finding' {sample} -> sample) (\s@Finding' {} a -> s {sample = a} :: Finding)

instance Core.FromJSON Finding where
  parseJSON =
    Core.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Core..:? "classificationDetails")
            Prelude.<*> (x Core..:? "policyDetails")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "category")
            Prelude.<*> (x Core..:? "severity")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "schemaVersion")
            Prelude.<*> (x Core..:? "resourcesAffected")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "partition")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "title")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "archived")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "sample")
      )

instance Prelude.Hashable Finding

instance Prelude.NFData Finding
