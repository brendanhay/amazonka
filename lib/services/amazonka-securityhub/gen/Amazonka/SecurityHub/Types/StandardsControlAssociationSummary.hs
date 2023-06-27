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
-- Module      : Amazonka.SecurityHub.Types.StandardsControlAssociationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsControlAssociationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociationStatus

-- | An array that provides the enablement status and other details for each
-- control that applies to each enabled standard.
--
-- /See:/ 'newStandardsControlAssociationSummary' smart constructor.
data StandardsControlAssociationSummary = StandardsControlAssociationSummary'
  { -- | The requirement that underlies this control in the compliance framework
    -- related to the standard.
    relatedRequirements :: Prelude.Maybe [Prelude.Text],
    -- | The description of a control. This typically summarizes how Security Hub
    -- evaluates the control and the conditions under which it produces a
    -- failed finding. The parameter may reference a specific standard.
    standardsControlDescription :: Prelude.Maybe Prelude.Text,
    -- | The title of a control.
    standardsControlTitle :: Prelude.Maybe Prelude.Text,
    -- | The last time that a control\'s enablement status in a specified
    -- standard was updated.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The reason for updating the control\'s enablement status in a specified
    -- standard.
    updatedReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a standard.
    standardsArn :: Prelude.Text,
    -- | A unique standard-agnostic identifier for a control. Values for this
    -- field typically consist of an Amazon Web Service and a number, such as
    -- APIGateway.5. This field doesn\'t reference a specific standard.
    securityControlId :: Prelude.Text,
    -- | The ARN of a control, such as
    -- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
    -- This parameter doesn\'t mention a specific standard.
    securityControlArn :: Prelude.Text,
    -- | The enablement status of a control in a specific standard.
    associationStatus :: AssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsControlAssociationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relatedRequirements', 'standardsControlAssociationSummary_relatedRequirements' - The requirement that underlies this control in the compliance framework
-- related to the standard.
--
-- 'standardsControlDescription', 'standardsControlAssociationSummary_standardsControlDescription' - The description of a control. This typically summarizes how Security Hub
-- evaluates the control and the conditions under which it produces a
-- failed finding. The parameter may reference a specific standard.
--
-- 'standardsControlTitle', 'standardsControlAssociationSummary_standardsControlTitle' - The title of a control.
--
-- 'updatedAt', 'standardsControlAssociationSummary_updatedAt' - The last time that a control\'s enablement status in a specified
-- standard was updated.
--
-- 'updatedReason', 'standardsControlAssociationSummary_updatedReason' - The reason for updating the control\'s enablement status in a specified
-- standard.
--
-- 'standardsArn', 'standardsControlAssociationSummary_standardsArn' - The Amazon Resource Name (ARN) of a standard.
--
-- 'securityControlId', 'standardsControlAssociationSummary_securityControlId' - A unique standard-agnostic identifier for a control. Values for this
-- field typically consist of an Amazon Web Service and a number, such as
-- APIGateway.5. This field doesn\'t reference a specific standard.
--
-- 'securityControlArn', 'standardsControlAssociationSummary_securityControlArn' - The ARN of a control, such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
--
-- 'associationStatus', 'standardsControlAssociationSummary_associationStatus' - The enablement status of a control in a specific standard.
newStandardsControlAssociationSummary ::
  -- | 'standardsArn'
  Prelude.Text ->
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'securityControlArn'
  Prelude.Text ->
  -- | 'associationStatus'
  AssociationStatus ->
  StandardsControlAssociationSummary
newStandardsControlAssociationSummary
  pStandardsArn_
  pSecurityControlId_
  pSecurityControlArn_
  pAssociationStatus_ =
    StandardsControlAssociationSummary'
      { relatedRequirements =
          Prelude.Nothing,
        standardsControlDescription =
          Prelude.Nothing,
        standardsControlTitle = Prelude.Nothing,
        updatedAt = Prelude.Nothing,
        updatedReason = Prelude.Nothing,
        standardsArn = pStandardsArn_,
        securityControlId = pSecurityControlId_,
        securityControlArn =
          pSecurityControlArn_,
        associationStatus = pAssociationStatus_
      }

-- | The requirement that underlies this control in the compliance framework
-- related to the standard.
standardsControlAssociationSummary_relatedRequirements :: Lens.Lens' StandardsControlAssociationSummary (Prelude.Maybe [Prelude.Text])
standardsControlAssociationSummary_relatedRequirements = Lens.lens (\StandardsControlAssociationSummary' {relatedRequirements} -> relatedRequirements) (\s@StandardsControlAssociationSummary' {} a -> s {relatedRequirements = a} :: StandardsControlAssociationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The description of a control. This typically summarizes how Security Hub
-- evaluates the control and the conditions under which it produces a
-- failed finding. The parameter may reference a specific standard.
standardsControlAssociationSummary_standardsControlDescription :: Lens.Lens' StandardsControlAssociationSummary (Prelude.Maybe Prelude.Text)
standardsControlAssociationSummary_standardsControlDescription = Lens.lens (\StandardsControlAssociationSummary' {standardsControlDescription} -> standardsControlDescription) (\s@StandardsControlAssociationSummary' {} a -> s {standardsControlDescription = a} :: StandardsControlAssociationSummary)

-- | The title of a control.
standardsControlAssociationSummary_standardsControlTitle :: Lens.Lens' StandardsControlAssociationSummary (Prelude.Maybe Prelude.Text)
standardsControlAssociationSummary_standardsControlTitle = Lens.lens (\StandardsControlAssociationSummary' {standardsControlTitle} -> standardsControlTitle) (\s@StandardsControlAssociationSummary' {} a -> s {standardsControlTitle = a} :: StandardsControlAssociationSummary)

-- | The last time that a control\'s enablement status in a specified
-- standard was updated.
standardsControlAssociationSummary_updatedAt :: Lens.Lens' StandardsControlAssociationSummary (Prelude.Maybe Prelude.UTCTime)
standardsControlAssociationSummary_updatedAt = Lens.lens (\StandardsControlAssociationSummary' {updatedAt} -> updatedAt) (\s@StandardsControlAssociationSummary' {} a -> s {updatedAt = a} :: StandardsControlAssociationSummary) Prelude.. Lens.mapping Data._Time

-- | The reason for updating the control\'s enablement status in a specified
-- standard.
standardsControlAssociationSummary_updatedReason :: Lens.Lens' StandardsControlAssociationSummary (Prelude.Maybe Prelude.Text)
standardsControlAssociationSummary_updatedReason = Lens.lens (\StandardsControlAssociationSummary' {updatedReason} -> updatedReason) (\s@StandardsControlAssociationSummary' {} a -> s {updatedReason = a} :: StandardsControlAssociationSummary)

-- | The Amazon Resource Name (ARN) of a standard.
standardsControlAssociationSummary_standardsArn :: Lens.Lens' StandardsControlAssociationSummary Prelude.Text
standardsControlAssociationSummary_standardsArn = Lens.lens (\StandardsControlAssociationSummary' {standardsArn} -> standardsArn) (\s@StandardsControlAssociationSummary' {} a -> s {standardsArn = a} :: StandardsControlAssociationSummary)

-- | A unique standard-agnostic identifier for a control. Values for this
-- field typically consist of an Amazon Web Service and a number, such as
-- APIGateway.5. This field doesn\'t reference a specific standard.
standardsControlAssociationSummary_securityControlId :: Lens.Lens' StandardsControlAssociationSummary Prelude.Text
standardsControlAssociationSummary_securityControlId = Lens.lens (\StandardsControlAssociationSummary' {securityControlId} -> securityControlId) (\s@StandardsControlAssociationSummary' {} a -> s {securityControlId = a} :: StandardsControlAssociationSummary)

-- | The ARN of a control, such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
standardsControlAssociationSummary_securityControlArn :: Lens.Lens' StandardsControlAssociationSummary Prelude.Text
standardsControlAssociationSummary_securityControlArn = Lens.lens (\StandardsControlAssociationSummary' {securityControlArn} -> securityControlArn) (\s@StandardsControlAssociationSummary' {} a -> s {securityControlArn = a} :: StandardsControlAssociationSummary)

-- | The enablement status of a control in a specific standard.
standardsControlAssociationSummary_associationStatus :: Lens.Lens' StandardsControlAssociationSummary AssociationStatus
standardsControlAssociationSummary_associationStatus = Lens.lens (\StandardsControlAssociationSummary' {associationStatus} -> associationStatus) (\s@StandardsControlAssociationSummary' {} a -> s {associationStatus = a} :: StandardsControlAssociationSummary)

instance
  Data.FromJSON
    StandardsControlAssociationSummary
  where
  parseJSON =
    Data.withObject
      "StandardsControlAssociationSummary"
      ( \x ->
          StandardsControlAssociationSummary'
            Prelude.<$> ( x
                            Data..:? "RelatedRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StandardsControlDescription")
            Prelude.<*> (x Data..:? "StandardsControlTitle")
            Prelude.<*> (x Data..:? "UpdatedAt")
            Prelude.<*> (x Data..:? "UpdatedReason")
            Prelude.<*> (x Data..: "StandardsArn")
            Prelude.<*> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "SecurityControlArn")
            Prelude.<*> (x Data..: "AssociationStatus")
      )

instance
  Prelude.Hashable
    StandardsControlAssociationSummary
  where
  hashWithSalt
    _salt
    StandardsControlAssociationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` relatedRequirements
        `Prelude.hashWithSalt` standardsControlDescription
        `Prelude.hashWithSalt` standardsControlTitle
        `Prelude.hashWithSalt` updatedAt
        `Prelude.hashWithSalt` updatedReason
        `Prelude.hashWithSalt` standardsArn
        `Prelude.hashWithSalt` securityControlId
        `Prelude.hashWithSalt` securityControlArn
        `Prelude.hashWithSalt` associationStatus

instance
  Prelude.NFData
    StandardsControlAssociationSummary
  where
  rnf StandardsControlAssociationSummary' {..} =
    Prelude.rnf relatedRequirements
      `Prelude.seq` Prelude.rnf standardsControlDescription
      `Prelude.seq` Prelude.rnf standardsControlTitle
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf updatedReason
      `Prelude.seq` Prelude.rnf standardsArn
      `Prelude.seq` Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf securityControlArn
      `Prelude.seq` Prelude.rnf associationStatus
