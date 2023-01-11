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
-- Module      : Amazonka.Inspector.Types.Finding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.Finding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types.AssetAttributes
import Amazonka.Inspector.Types.AssetType
import Amazonka.Inspector.Types.Attribute
import Amazonka.Inspector.Types.InspectorServiceAttributes
import Amazonka.Inspector.Types.Severity
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Inspector finding. This data type
-- is used as the response element in the DescribeFindings action.
--
-- /See:/ 'newFinding' smart constructor.
data Finding = Finding'
  { -- | A collection of attributes of the host from which the finding is
    -- generated.
    assetAttributes :: Prelude.Maybe AssetAttributes,
    -- | The type of the host from which the finding is generated.
    assetType :: Prelude.Maybe AssetType,
    -- | This data element is currently not used.
    confidence :: Prelude.Maybe Prelude.Natural,
    -- | The description of the finding.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the finding.
    id :: Prelude.Maybe Prelude.Text,
    -- | This data element is currently not used.
    indicatorOfCompromise :: Prelude.Maybe Prelude.Bool,
    -- | The numeric value of the finding severity.
    numericSeverity :: Prelude.Maybe Prelude.Double,
    -- | The recommendation for the finding.
    recommendation :: Prelude.Maybe Prelude.Text,
    -- | The schema version of this data type.
    schemaVersion :: Prelude.Maybe Prelude.Natural,
    -- | The data element is set to \"Inspector\".
    service :: Prelude.Maybe Prelude.Text,
    -- | This data type is used in the Finding data type.
    serviceAttributes :: Prelude.Maybe InspectorServiceAttributes,
    -- | The finding severity. Values can be set to High, Medium, Low, and
    -- Informational.
    severity :: Prelude.Maybe Severity,
    -- | The name of the finding.
    title :: Prelude.Maybe Prelude.Text,
    -- | The ARN that specifies the finding.
    arn :: Prelude.Text,
    -- | The system-defined attributes for the finding.
    attributes :: [Attribute],
    -- | The user-defined attributes that are assigned to the finding.
    userAttributes :: [Attribute],
    -- | The time when the finding was generated.
    createdAt :: Data.POSIX,
    -- | The time when AddAttributesToFindings is called.
    updatedAt :: Data.POSIX
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
-- 'assetAttributes', 'finding_assetAttributes' - A collection of attributes of the host from which the finding is
-- generated.
--
-- 'assetType', 'finding_assetType' - The type of the host from which the finding is generated.
--
-- 'confidence', 'finding_confidence' - This data element is currently not used.
--
-- 'description', 'finding_description' - The description of the finding.
--
-- 'id', 'finding_id' - The ID of the finding.
--
-- 'indicatorOfCompromise', 'finding_indicatorOfCompromise' - This data element is currently not used.
--
-- 'numericSeverity', 'finding_numericSeverity' - The numeric value of the finding severity.
--
-- 'recommendation', 'finding_recommendation' - The recommendation for the finding.
--
-- 'schemaVersion', 'finding_schemaVersion' - The schema version of this data type.
--
-- 'service', 'finding_service' - The data element is set to \"Inspector\".
--
-- 'serviceAttributes', 'finding_serviceAttributes' - This data type is used in the Finding data type.
--
-- 'severity', 'finding_severity' - The finding severity. Values can be set to High, Medium, Low, and
-- Informational.
--
-- 'title', 'finding_title' - The name of the finding.
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
      assetType = Prelude.Nothing,
      confidence = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      indicatorOfCompromise = Prelude.Nothing,
      numericSeverity = Prelude.Nothing,
      recommendation = Prelude.Nothing,
      schemaVersion = Prelude.Nothing,
      service = Prelude.Nothing,
      serviceAttributes = Prelude.Nothing,
      severity = Prelude.Nothing,
      title = Prelude.Nothing,
      arn = pArn_,
      attributes = Prelude.mempty,
      userAttributes = Prelude.mempty,
      createdAt = Data._Time Lens.# pCreatedAt_,
      updatedAt = Data._Time Lens.# pUpdatedAt_
    }

-- | A collection of attributes of the host from which the finding is
-- generated.
finding_assetAttributes :: Lens.Lens' Finding (Prelude.Maybe AssetAttributes)
finding_assetAttributes = Lens.lens (\Finding' {assetAttributes} -> assetAttributes) (\s@Finding' {} a -> s {assetAttributes = a} :: Finding)

-- | The type of the host from which the finding is generated.
finding_assetType :: Lens.Lens' Finding (Prelude.Maybe AssetType)
finding_assetType = Lens.lens (\Finding' {assetType} -> assetType) (\s@Finding' {} a -> s {assetType = a} :: Finding)

-- | This data element is currently not used.
finding_confidence :: Lens.Lens' Finding (Prelude.Maybe Prelude.Natural)
finding_confidence = Lens.lens (\Finding' {confidence} -> confidence) (\s@Finding' {} a -> s {confidence = a} :: Finding)

-- | The description of the finding.
finding_description :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_description = Lens.lens (\Finding' {description} -> description) (\s@Finding' {} a -> s {description = a} :: Finding)

-- | The ID of the finding.
finding_id :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_id = Lens.lens (\Finding' {id} -> id) (\s@Finding' {} a -> s {id = a} :: Finding)

-- | This data element is currently not used.
finding_indicatorOfCompromise :: Lens.Lens' Finding (Prelude.Maybe Prelude.Bool)
finding_indicatorOfCompromise = Lens.lens (\Finding' {indicatorOfCompromise} -> indicatorOfCompromise) (\s@Finding' {} a -> s {indicatorOfCompromise = a} :: Finding)

-- | The numeric value of the finding severity.
finding_numericSeverity :: Lens.Lens' Finding (Prelude.Maybe Prelude.Double)
finding_numericSeverity = Lens.lens (\Finding' {numericSeverity} -> numericSeverity) (\s@Finding' {} a -> s {numericSeverity = a} :: Finding)

-- | The recommendation for the finding.
finding_recommendation :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_recommendation = Lens.lens (\Finding' {recommendation} -> recommendation) (\s@Finding' {} a -> s {recommendation = a} :: Finding)

-- | The schema version of this data type.
finding_schemaVersion :: Lens.Lens' Finding (Prelude.Maybe Prelude.Natural)
finding_schemaVersion = Lens.lens (\Finding' {schemaVersion} -> schemaVersion) (\s@Finding' {} a -> s {schemaVersion = a} :: Finding)

-- | The data element is set to \"Inspector\".
finding_service :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_service = Lens.lens (\Finding' {service} -> service) (\s@Finding' {} a -> s {service = a} :: Finding)

-- | This data type is used in the Finding data type.
finding_serviceAttributes :: Lens.Lens' Finding (Prelude.Maybe InspectorServiceAttributes)
finding_serviceAttributes = Lens.lens (\Finding' {serviceAttributes} -> serviceAttributes) (\s@Finding' {} a -> s {serviceAttributes = a} :: Finding)

-- | The finding severity. Values can be set to High, Medium, Low, and
-- Informational.
finding_severity :: Lens.Lens' Finding (Prelude.Maybe Severity)
finding_severity = Lens.lens (\Finding' {severity} -> severity) (\s@Finding' {} a -> s {severity = a} :: Finding)

-- | The name of the finding.
finding_title :: Lens.Lens' Finding (Prelude.Maybe Prelude.Text)
finding_title = Lens.lens (\Finding' {title} -> title) (\s@Finding' {} a -> s {title = a} :: Finding)

-- | The ARN that specifies the finding.
finding_arn :: Lens.Lens' Finding Prelude.Text
finding_arn = Lens.lens (\Finding' {arn} -> arn) (\s@Finding' {} a -> s {arn = a} :: Finding)

-- | The system-defined attributes for the finding.
finding_attributes :: Lens.Lens' Finding [Attribute]
finding_attributes = Lens.lens (\Finding' {attributes} -> attributes) (\s@Finding' {} a -> s {attributes = a} :: Finding) Prelude.. Lens.coerced

-- | The user-defined attributes that are assigned to the finding.
finding_userAttributes :: Lens.Lens' Finding [Attribute]
finding_userAttributes = Lens.lens (\Finding' {userAttributes} -> userAttributes) (\s@Finding' {} a -> s {userAttributes = a} :: Finding) Prelude.. Lens.coerced

-- | The time when the finding was generated.
finding_createdAt :: Lens.Lens' Finding Prelude.UTCTime
finding_createdAt = Lens.lens (\Finding' {createdAt} -> createdAt) (\s@Finding' {} a -> s {createdAt = a} :: Finding) Prelude.. Data._Time

-- | The time when AddAttributesToFindings is called.
finding_updatedAt :: Lens.Lens' Finding Prelude.UTCTime
finding_updatedAt = Lens.lens (\Finding' {updatedAt} -> updatedAt) (\s@Finding' {} a -> s {updatedAt = a} :: Finding) Prelude.. Data._Time

instance Data.FromJSON Finding where
  parseJSON =
    Data.withObject
      "Finding"
      ( \x ->
          Finding'
            Prelude.<$> (x Data..:? "assetAttributes")
            Prelude.<*> (x Data..:? "assetType")
            Prelude.<*> (x Data..:? "confidence")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "indicatorOfCompromise")
            Prelude.<*> (x Data..:? "numericSeverity")
            Prelude.<*> (x Data..:? "recommendation")
            Prelude.<*> (x Data..:? "schemaVersion")
            Prelude.<*> (x Data..:? "service")
            Prelude.<*> (x Data..:? "serviceAttributes")
            Prelude.<*> (x Data..:? "severity")
            Prelude.<*> (x Data..:? "title")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "userAttributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
      )

instance Prelude.Hashable Finding where
  hashWithSalt _salt Finding' {..} =
    _salt `Prelude.hashWithSalt` assetAttributes
      `Prelude.hashWithSalt` assetType
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indicatorOfCompromise
      `Prelude.hashWithSalt` numericSeverity
      `Prelude.hashWithSalt` recommendation
      `Prelude.hashWithSalt` schemaVersion
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` serviceAttributes
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Finding where
  rnf Finding' {..} =
    Prelude.rnf assetAttributes
      `Prelude.seq` Prelude.rnf assetType
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indicatorOfCompromise
      `Prelude.seq` Prelude.rnf numericSeverity
      `Prelude.seq` Prelude.rnf recommendation
      `Prelude.seq` Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf serviceAttributes
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf userAttributes
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
