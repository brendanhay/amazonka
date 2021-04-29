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
-- Module      : Network.AWS.GuardDuty.Types.Finding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Finding where

import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.ServiceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the finding, which is generated when abnormal
-- or suspicious activity is detected.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | The title of the finding.
    title :: Prelude.Maybe Prelude.Text,
    service :: Prelude.Maybe ServiceInfo,
    -- | The partition associated with the finding.
    partition :: Prelude.Maybe Prelude.Text,
    -- | The confidence score for the finding.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account in which the finding was generated.
    accountId :: Prelude.Text,
    -- | The ARN of the finding.
    arn :: Prelude.Text,
    -- | The time and date when the finding was created.
    createdAt :: Prelude.Text,
    -- | The ID of the finding.
    id :: Prelude.Text,
    -- | The Region where the finding was generated.
    region :: Prelude.Text,
    resource :: Resource,
    -- | The version of the schema used for the finding.
    schemaVersion :: Prelude.Text,
    -- | The severity of the finding.
    severity :: Prelude.Double,
    -- | The type of finding.
    type' :: Prelude.Text,
    -- | The time and date when the finding was last updated.
    updatedAt :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Finding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'finding_title' - The title of the finding.
--
-- 'service', 'finding_service' - Undocumented member.
--
-- 'partition', 'finding_partition' - The partition associated with the finding.
--
-- 'confidence', 'finding_confidence' - The confidence score for the finding.
--
-- 'description', 'finding_description' - The description of the finding.
--
-- 'accountId', 'finding_accountId' - The ID of the account in which the finding was generated.
--
-- 'arn', 'finding_arn' - The ARN of the finding.
--
-- 'createdAt', 'finding_createdAt' - The time and date when the finding was created.
--
-- 'id', 'finding_id' - The ID of the finding.
--
-- 'region', 'finding_region' - The Region where the finding was generated.
--
-- 'resource', 'finding_resource' - Undocumented member.
--
-- 'schemaVersion', 'finding_schemaVersion' - The version of the schema used for the finding.
--
-- 'severity', 'finding_severity' - The severity of the finding.
--
-- 'type'', 'finding_type' - The type of finding.
--
-- 'updatedAt', 'finding_updatedAt' - The time and date when the finding was last updated.
newFinding ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'region'
  Prelude.Text ->
  -- | 'resource'
  Resource ->
  -- | 'schemaVersion'
  Prelude.Text ->
  -- | 'severity'
  Prelude.Double ->
  -- | 'type''
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.Text ->
  Finding
newFinding
  pAccountId_
  pArn_
  pCreatedAt_
  pId_
  pRegion_
  pResource_
  pSchemaVersion_
  pSeverity_
  pType_
  pUpdatedAt_ =
    Finding'
      { title = Prelude.Nothing,
        service = Prelude.Nothing,
        partition = Prelude.Nothing,
        confidence = Prelude.Nothing,
        description = Prelude.Nothing,
        accountId = pAccountId_,
        arn = pArn_,
        createdAt = pCreatedAt_,
        id = pId_,
        region = pRegion_,
        resource = pResource_,
        schemaVersion = pSchemaVersion_,
        severity = pSeverity_,
        type' = pType_,
        updatedAt = pUpdatedAt_
      }

-- | The title of the finding.
finding_title :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | Undocumented member.
finding_service :: Lens.Lens' Finding (Prelude.Maybe ServiceInfo)
finding_service = Lens.lens (\Finding' {service} -> service) (\s@Finding' {} a -> s {service = a} :: Finding)

-- | The partition associated with the finding.
finding_partition :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_partition = Lens.lens (\Finding' {partition} -> partition) (\s@Finding' {} a -> s {partition = a} :: Finding)

-- | The confidence score for the finding.
finding_confidence :: Lens.Lens' Finding (Prelude.Maybe Prelude.Double)
finding_confidence = Lens.lens (\Finding' {confidence} -> confidence) (\s@Finding' {} a -> s {confidence = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | The ID of the account in which the finding was generated.
finding_accountId :: Lens.Lens' Finding Prelude.Text
finding_accountId = Lens.lens (\Finding' {accountId} -> accountId) (\s@Finding' {} a -> s {accountId = a} :: Finding)

-- | The ARN of the finding.
finding_arn :: Lens.Lens' Finding Prelude.Text
finding_arn = Lens.lens (\Finding' {arn} -> arn) (\s@Finding' {} a -> s {arn = a} :: Finding)

-- | The time and date when the finding was created.
finding_createdAt :: Lens.Lens' Finding Prelude.Text
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding)

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding Prelude.Text
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The Region where the finding was generated.
finding_region :: Lens.Lens' Finding Prelude.Text
finding_region = Lens.lens (\Finding' {region} -> region) (\s@Finding' {} a -> s {region = a} :: Finding)

-- | Undocumented member.
finding_resource :: Lens.Lens' Finding Resource
finding_resource = Lens.lens (\Finding' {resource} -> resource) (\s@Finding' {} a -> s {resource = a} :: Finding)

-- | The version of the schema used for the finding.
finding_schemaVersion :: Lens.Lens' Finding Prelude.Text
finding_schemaVersion = Lens.lens (\Finding' {schemaVersion} -> schemaVersion) (\s@Finding' {} a -> s {schemaVersion = a} :: Finding)

-- | The severity of the finding.
finding_severity :: Lens.Lens' Finding Prelude.Double
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The type of finding.
finding_type :: Lens.Lens' Finding Prelude.Text
finding_type = Lens.lens (\Finding' {type'} -> type') (\s@Finding' {} a -> s {type' = a} :: Finding)

-- | The time and date when the finding was last updated.
finding_updatedAt :: Lens.Lens' Finding Prelude.Text
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding)

instance Prelude.FromJSON Finding where
  parseJSON =
    Prelude.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Prelude..:? "title")
            Prelude.<*> (x Prelude..:? "service")
            Prelude.<*> (x Prelude..:? "partition")
            Prelude.<*> (x Prelude..:? "confidence")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..: "accountId")
            Prelude.<*> (x Prelude..: "arn")
            Prelude.<*> (x Prelude..: "createdAt")
            Prelude.<*> (x Prelude..: "id")
            Prelude.<*> (x Prelude..: "region")
            Prelude.<*> (x Prelude..: "resource")
            Prelude.<*> (x Prelude..: "schemaVersion")
            Prelude.<*> (x Prelude..: "severity")
            Prelude.<*> (x Prelude..: "type")
            Prelude.<*> (x Prelude..: "updatedAt")
      )

instance Prelude.Hashable Finding

instance Prelude.NFData Finding
