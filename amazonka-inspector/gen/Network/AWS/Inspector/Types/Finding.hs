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
-- Module      : Network.AWS.Inspector.Types.Finding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Finding where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Severity
import qualified Network.AWS.Lens as Lens

-- | Contains information about an Amazon Inspector finding. This data type
-- is used as the response element in the DescribeFindings action.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | A collection of attributes of the host from which the finding is
    -- generated.
    assetAttributes :: Core.Maybe AssetAttributes,
    -- | The finding severity. Values can be set to High, Medium, Low, and
    -- Informational.
    severity :: Core.Maybe Severity,
    -- | The name of the finding.
    title :: Core.Maybe Core.Text,
    -- | The type of the host from which the finding is generated.
    assetType :: Core.Maybe AssetType,
    -- | The numeric value of the finding severity.
    numericSeverity :: Core.Maybe Core.Double,
    -- | The ID of the finding.
    id :: Core.Maybe Core.Text,
    -- | The data element is set to \"Inspector\".
    service :: Core.Maybe Core.Text,
    -- | This data type is used in the Finding data type.
    serviceAttributes :: Core.Maybe InspectorServiceAttributes,
    -- | This data element is currently not used.
    confidence :: Core.Maybe Core.Natural,
    -- | The recommendation for the finding.
    recommendation :: Core.Maybe Core.Text,
    -- | This data element is currently not used.
    indicatorOfCompromise :: Core.Maybe Core.Bool,
    -- | The description of the finding.
    description :: Core.Maybe Core.Text,
    -- | The schema version of this data type.
    schemaVersion :: Core.Maybe Core.Natural,
    -- | The ARN that specifies the finding.
    arn :: Core.Text,
    -- | The system-defined attributes for the finding.
    attributes :: [Attribute],
    -- | The user-defined attributes that are assigned to the finding.
    userAttributes :: [Attribute],
    -- | The time when the finding was generated.
    createdAt :: Core.POSIX,
    -- | The time when AddAttributesToFindings is called.
    updatedAt :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Finding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetAttributes', 'finding_assetAttributes' - A collection of attributes of the host from which the finding is
-- generated.
--
-- 'severity', 'finding_severity' - The finding severity. Values can be set to High, Medium, Low, and
-- Informational.
--
-- 'title', 'finding_title' - The name of the finding.
--
-- 'assetType', 'finding_assetType' - The type of the host from which the finding is generated.
--
-- 'numericSeverity', 'finding_numericSeverity' - The numeric value of the finding severity.
--
-- 'id', 'finding_id' - The ID of the finding.
--
-- 'service', 'finding_service' - The data element is set to \"Inspector\".
--
-- 'serviceAttributes', 'finding_serviceAttributes' - This data type is used in the Finding data type.
--
-- 'confidence', 'finding_confidence' - This data element is currently not used.
--
-- 'recommendation', 'finding_recommendation' - The recommendation for the finding.
--
-- 'indicatorOfCompromise', 'finding_indicatorOfCompromise' - This data element is currently not used.
--
-- 'description', 'finding_description' - The description of the finding.
--
-- 'schemaVersion', 'finding_schemaVersion' - The schema version of this data type.
--
-- 'arn', 'finding_arn' - The ARN that specifies the finding.
--
-- 'attributes', 'finding_attributes' - The system-defined attributes for the finding.
--
-- 'userAttributes', 'finding_userAttributes' - The user-defined attributes that are assigned to the finding.
--
-- 'createdAt', 'finding_createdAt' - The time when the finding was generated.
--
-- 'updatedAt', 'finding_updatedAt' - The time when AddAttributesToFindings is called.
newFinding ::
  -- | 'arn'
  Core.Text ->
  -- | 'createdAt'
  Core.UTCTime ->
  -- | 'updatedAt'
  Core.UTCTime ->
  Finding
newFinding pArn_ pCreatedAt_ pUpdatedAt_ =
  Finding'
    { assetAttributes = Core.Nothing,
      severity = Core.Nothing,
      title = Core.Nothing,
      assetType = Core.Nothing,
      numericSeverity = Core.Nothing,
      id = Core.Nothing,
      service = Core.Nothing,
      serviceAttributes = Core.Nothing,
      confidence = Core.Nothing,
      recommendation = Core.Nothing,
      indicatorOfCompromise = Core.Nothing,
      description = Core.Nothing,
      schemaVersion = Core.Nothing,
      arn = pArn_,
      attributes = Core.mempty,
      userAttributes = Core.mempty,
      createdAt = Core._Time Lens.# pCreatedAt_,
      updatedAt = Core._Time Lens.# pUpdatedAt_
    }

-- | A collection of attributes of the host from which the finding is
-- generated.
finding_assetAttributes :: Lens.Lens' Finding (Core.Maybe AssetAttributes)
finding_assetAttributes = Lens.lens (\Finding' {assetAttributes} -> assetAttributes) (\s@Finding' {} a -> s {assetAttributes = a} :: Finding)

-- | The finding severity. Values can be set to High, Medium, Low, and
-- Informational.
finding_severity :: Lens.Lens' Finding (Core.Maybe Severity)
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The name of the finding.
finding_title :: Lens.Lens' Finding (Core.Maybe Core.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | The type of the host from which the finding is generated.
finding_assetType :: Lens.Lens' Finding (Core.Maybe AssetType)
finding_assetType = Lens.lens (\Finding' {assetType} -> assetType) (\s@Finding' {} a -> s {assetType = a} :: Finding)

-- | The numeric value of the finding severity.
finding_numericSeverity :: Lens.Lens' Finding (Core.Maybe Core.Double)
finding_numericSeverity = Lens.lens (\Finding' {numericSeverity} -> numericSeverity) (\s@Finding' {} a -> s {numericSeverity = a} :: Finding)

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding (Core.Maybe Core.Text)
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The data element is set to \"Inspector\".
finding_service :: Lens.Lens' Finding (Core.Maybe Core.Text)
finding_service = Lens.lens (\Finding' {service} -> service) (\s@Finding' {} a -> s {service = a} :: Finding)

-- | This data type is used in the Finding data type.
finding_serviceAttributes :: Lens.Lens' Finding (Core.Maybe InspectorServiceAttributes)
finding_serviceAttributes = Lens.lens (\Finding' {serviceAttributes} -> serviceAttributes) (\s@Finding' {} a -> s {serviceAttributes = a} :: Finding)

-- | This data element is currently not used.
finding_confidence :: Lens.Lens' Finding (Core.Maybe Core.Natural)
finding_confidence = Lens.lens (\Finding' {confidence} -> confidence) (\s@Finding' {} a -> s {confidence = a} :: Finding)

-- | The recommendation for the finding.
finding_recommendation :: Lens.Lens' Finding (Core.Maybe Core.Text)
finding_recommendation = Lens.lens (\Finding' {recommendation} -> recommendation) (\s@Finding' {} a -> s {recommendation = a} :: Finding)

-- | This data element is currently not used.
finding_indicatorOfCompromise :: Lens.Lens' Finding (Core.Maybe Core.Bool)
finding_indicatorOfCompromise = Lens.lens (\Finding' {indicatorOfCompromise} -> indicatorOfCompromise) (\s@Finding' {} a -> s {indicatorOfCompromise = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding (Core.Maybe Core.Text)
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | The schema version of this data type.
finding_schemaVersion :: Lens.Lens' Finding (Core.Maybe Core.Natural)
finding_schemaVersion = Lens.lens (\Finding' {schemaVersion} -> schemaVersion) (\s@Finding' {} a -> s {schemaVersion = a} :: Finding)

-- | The ARN that specifies the finding.
finding_arn :: Lens.Lens' Finding Core.Text
finding_arn = Lens.lens (\Finding' {arn} -> arn) (\s@Finding' {} a -> s {arn = a} :: Finding)

-- | The system-defined attributes for the finding.
finding_attributes :: Lens.Lens' Finding [Attribute]
finding_attributes = Lens.lens (\Finding' {attributes} -> attributes) (\s@Finding' {} a -> s {attributes = a} :: Finding) Core.. Lens._Coerce

-- | The user-defined attributes that are assigned to the finding.
finding_userAttributes :: Lens.Lens' Finding [Attribute]
finding_userAttributes = Lens.lens (\Finding' {userAttributes} -> userAttributes) (\s@Finding' {} a -> s {userAttributes = a} :: Finding) Core.. Lens._Coerce

-- | The time when the finding was generated.
finding_createdAt :: Lens.Lens' Finding Core.UTCTime
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Core.. Core._Time

-- | The time when AddAttributesToFindings is called.
finding_updatedAt :: Lens.Lens' Finding Core.UTCTime
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Core.. Core._Time

instance Core.FromJSON Finding where
  parseJSON =
    Core.withObject
      "Finding"
      ( \x ->
          Finding'
            Core.<$> (x Core..:? "assetAttributes")
            Core.<*> (x Core..:? "severity")
            Core.<*> (x Core..:? "title")
            Core.<*> (x Core..:? "assetType")
            Core.<*> (x Core..:? "numericSeverity")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "service")
            Core.<*> (x Core..:? "serviceAttributes")
            Core.<*> (x Core..:? "confidence")
            Core.<*> (x Core..:? "recommendation")
            Core.<*> (x Core..:? "indicatorOfCompromise")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "schemaVersion")
            Core.<*> (x Core..: "arn")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "userAttributes" Core..!= Core.mempty)
            Core.<*> (x Core..: "createdAt")
            Core.<*> (x Core..: "updatedAt")
      )

instance Core.Hashable Finding

instance Core.NFData Finding
