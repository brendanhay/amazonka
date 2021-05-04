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
-- Module      : Network.AWS.Inspector.Types.Finding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Finding where

import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Severity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an Amazon Inspector finding. This data type
-- is used as the response element in the DescribeFindings action.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | A collection of attributes of the host from which the finding is
    -- generated.
    assetAttributes :: Prelude.Maybe AssetAttributes,
    -- | The finding severity. Values can be set to High, Medium, Low, and
    -- Informational.
    severity :: Prelude.Maybe Severity,
    -- | The name of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | The type of the host from which the finding is generated.
    assetType :: Prelude.Maybe AssetType,
    -- | The numeric value of the finding severity.
    numericSeverity :: Prelude.Maybe Prelude.Double,
    -- | The ID of the finding.
    id :: Prelude.Maybe Prelude.Text,
    -- | The data element is set to \"Inspector\".
    service :: Prelude.Maybe Prelude.Text,
    -- | This data type is used in the Finding data type.
    serviceAttributes :: Prelude.Maybe InspectorServiceAttributes,
    -- | This data element is currently not used.
    confidence :: Prelude.Maybe Prelude.Natural,
    -- | The recommendation for the finding.
    recommendation :: Prelude.Maybe Prelude.Text,
    -- | This data element is currently not used.
    indicatorOfCompromise :: Prelude.Maybe Prelude.Bool,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema version of this data type.
    schemaVersion :: Prelude.Maybe Prelude.Natural,
    -- | The ARN that specifies the finding.
    arn :: Prelude.Text,
    -- | The system-defined attributes for the finding.
    attributes :: [Attribute],
    -- | The user-defined attributes that are assigned to the finding.
    userAttributes :: [Attribute],
    -- | The time when the finding was generated.
    createdAt :: Prelude.POSIX,
    -- | The time when AddAttributesToFindings is called.
    updatedAt :: Prelude.POSIX
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
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  Finding
newFinding pArn_ pCreatedAt_ pUpdatedAt_ =
  Finding'
    { assetAttributes = Prelude.Nothing,
      severity = Prelude.Nothing,
      title = Prelude.Nothing,
      assetType = Prelude.Nothing,
      numericSeverity = Prelude.Nothing,
      id = Prelude.Nothing,
      service = Prelude.Nothing,
      serviceAttributes = Prelude.Nothing,
      confidence = Prelude.Nothing,
      recommendation = Prelude.Nothing,
      indicatorOfCompromise = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      arn = pArn_,
      attributes = Prelude.mempty,
      userAttributes = Prelude.mempty,
      createdAt = Prelude._Time Lens.# pCreatedAt_,
      updatedAt = Prelude._Time Lens.# pUpdatedAt_
    }

-- | A collection of attributes of the host from which the finding is
-- generated.
finding_assetAttributes :: Lens.Lens' Finding (Prelude.Maybe AssetAttributes)
finding_assetAttributes = Lens.lens (\Finding' {assetAttributes} -> assetAttributes) (\s@Finding' {} a -> s {assetAttributes = a} :: Finding)

-- | The finding severity. Values can be set to High, Medium, Low, and
-- Informational.
finding_severity :: Lens.Lens' Finding (Prelude.Maybe Severity)
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The name of the finding.
finding_title :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | The type of the host from which the finding is generated.
finding_assetType :: Lens.Lens' Finding (Prelude.Maybe AssetType)
finding_assetType = Lens.lens (\Finding' {assetType} -> assetType) (\s@Finding' {} a -> s {assetType = a} :: Finding)

-- | The numeric value of the finding severity.
finding_numericSeverity :: Lens.Lens' Finding (Prelude.Maybe Prelude.Double)
finding_numericSeverity = Lens.lens (\Finding' {numericSeverity} -> numericSeverity) (\s@Finding' {} a -> s {numericSeverity = a} :: Finding)

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | The data element is set to \"Inspector\".
finding_service :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_service = Lens.lens (\Finding' {service} -> service) (\s@Finding' {} a -> s {service = a} :: Finding)

-- | This data type is used in the Finding data type.
finding_serviceAttributes :: Lens.Lens' Finding (Prelude.Maybe InspectorServiceAttributes)
finding_serviceAttributes = Lens.lens (\Finding' {serviceAttributes} -> serviceAttributes) (\s@Finding' {} a -> s {serviceAttributes = a} :: Finding)

-- | This data element is currently not used.
finding_confidence :: Lens.Lens' Finding (Prelude.Maybe Prelude.Natural)
finding_confidence = Lens.lens (\Finding' {confidence} -> confidence) (\s@Finding' {} a -> s {confidence = a} :: Finding)

-- | The recommendation for the finding.
finding_recommendation :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_recommendation = Lens.lens (\Finding' {recommendation} -> recommendation) (\s@Finding' {} a -> s {recommendation = a} :: Finding)

-- | This data element is currently not used.
finding_indicatorOfCompromise :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_indicatorOfCompromise = Lens.lens (\Finding' {indicatorOfCompromise} -> indicatorOfCompromise) (\s@Finding' {} a -> s {indicatorOfCompromise = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | The schema version of this data type.
finding_schemaVersion :: Lens.Lens' Finding (Prelude.Maybe Prelude.Natural)
finding_schemaVersion = Lens.lens (\Finding' {schemaVersion} -> schemaVersion) (\s@Finding' {} a -> s {schemaVersion = a} :: Finding)

-- | The ARN that specifies the finding.
finding_arn :: Lens.Lens' Finding Prelude.Text
finding_arn = Lens.lens (\Finding' {arn} -> arn) (\s@Finding' {} a -> s {arn = a} :: Finding)

-- | The system-defined attributes for the finding.
finding_attributes :: Lens.Lens' Finding [Attribute]
finding_attributes = Lens.lens (\Finding' {attributes} -> attributes) (\s@Finding' {} a -> s {attributes = a} :: Finding) Prelude.. Prelude._Coerce

-- | The user-defined attributes that are assigned to the finding.
finding_userAttributes :: Lens.Lens' Finding [Attribute]
finding_userAttributes = Lens.lens (\Finding' {userAttributes} -> userAttributes) (\s@Finding' {} a -> s {userAttributes = a} :: Finding) Prelude.. Prelude._Coerce

-- | The time when the finding was generated.
finding_createdAt :: Lens.Lens' Finding Prelude.UTCTime
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Prelude.. Prelude._Time

-- | The time when AddAttributesToFindings is called.
finding_updatedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Prelude._Time

instance Prelude.FromJSON Finding where
  parseJSON =
    Prelude.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Prelude..:? "assetAttributes")
            Prelude.<*> (x Prelude..:? "severity")
            Prelude.<*> (x Prelude..:? "title")
            Prelude.<*> (x Prelude..:? "assetType")
            Prelude.<*> (x Prelude..:? "numericSeverity")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "service")
            Prelude.<*> (x Prelude..:? "serviceAttributes")
            Prelude.<*> (x Prelude..:? "confidence")
            Prelude.<*> (x Prelude..:? "recommendation")
            Prelude.<*> (x Prelude..:? "indicatorOfCompromise")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "schemaVersion")
            Prelude.<*> (x Prelude..: "arn")
            Prelude.<*> ( x Prelude..:? "attributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "userAttributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "createdAt")
            Prelude.<*> (x Prelude..: "updatedAt")
      )

instance Prelude.Hashable Finding

instance Prelude.NFData Finding
