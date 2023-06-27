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
-- Module      : Amazonka.SecurityHub.Types.StandardsControlAssociationDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsControlAssociationDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociationStatus

-- | Provides details about a control\'s enablement status in a specified
-- standard.
--
-- /See:/ 'newStandardsControlAssociationDetail' smart constructor.
data StandardsControlAssociationDetail = StandardsControlAssociationDetail'
  { -- | The requirement that underlies a control in the compliance framework
    -- related to the standard.
    relatedRequirements :: Prelude.Maybe [Prelude.Text],
    -- | Provides the input parameter that Security Hub uses to call the
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_UpdateStandardsControl.html UpdateStandardsControl>
    -- API. This API can be used to enable or disable a control in a specified
    -- standard.
    standardsControlArns :: Prelude.Maybe [Prelude.Text],
    -- | The description of a control. This typically summarizes how Security Hub
    -- evaluates the control and the conditions under which it produces a
    -- failed finding. This parameter may reference a specific standard.
    standardsControlDescription :: Prelude.Maybe Prelude.Text,
    -- | The title of a control. This field may reference a specific standard.
    standardsControlTitle :: Prelude.Maybe Prelude.Text,
    -- | The time at which the enablement status of the control in the specified
    -- standard was last updated.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The reason for updating the enablement status of a control in a
    -- specified standard.
    updatedReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a security standard.
    standardsArn :: Prelude.Text,
    -- | The unique identifier of a security control across standards. Values for
    -- this field typically consist of an Amazon Web Service name and a number,
    -- such as APIGateway.3.
    securityControlId :: Prelude.Text,
    -- | The ARN of a security control across standards, such as
    -- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
    -- This parameter doesn\'t mention a specific standard.
    securityControlArn :: Prelude.Text,
    -- | Specifies whether a control is enabled or disabled in a specified
    -- standard.
    associationStatus :: AssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsControlAssociationDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relatedRequirements', 'standardsControlAssociationDetail_relatedRequirements' - The requirement that underlies a control in the compliance framework
-- related to the standard.
--
-- 'standardsControlArns', 'standardsControlAssociationDetail_standardsControlArns' - Provides the input parameter that Security Hub uses to call the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_UpdateStandardsControl.html UpdateStandardsControl>
-- API. This API can be used to enable or disable a control in a specified
-- standard.
--
-- 'standardsControlDescription', 'standardsControlAssociationDetail_standardsControlDescription' - The description of a control. This typically summarizes how Security Hub
-- evaluates the control and the conditions under which it produces a
-- failed finding. This parameter may reference a specific standard.
--
-- 'standardsControlTitle', 'standardsControlAssociationDetail_standardsControlTitle' - The title of a control. This field may reference a specific standard.
--
-- 'updatedAt', 'standardsControlAssociationDetail_updatedAt' - The time at which the enablement status of the control in the specified
-- standard was last updated.
--
-- 'updatedReason', 'standardsControlAssociationDetail_updatedReason' - The reason for updating the enablement status of a control in a
-- specified standard.
--
-- 'standardsArn', 'standardsControlAssociationDetail_standardsArn' - The Amazon Resource Name (ARN) of a security standard.
--
-- 'securityControlId', 'standardsControlAssociationDetail_securityControlId' - The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number,
-- such as APIGateway.3.
--
-- 'securityControlArn', 'standardsControlAssociationDetail_securityControlArn' - The ARN of a security control across standards, such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
--
-- 'associationStatus', 'standardsControlAssociationDetail_associationStatus' - Specifies whether a control is enabled or disabled in a specified
-- standard.
newStandardsControlAssociationDetail ::
  -- | 'standardsArn'
  Prelude.Text ->
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'securityControlArn'
  Prelude.Text ->
  -- | 'associationStatus'
  AssociationStatus ->
  StandardsControlAssociationDetail
newStandardsControlAssociationDetail
  pStandardsArn_
  pSecurityControlId_
  pSecurityControlArn_
  pAssociationStatus_ =
    StandardsControlAssociationDetail'
      { relatedRequirements =
          Prelude.Nothing,
        standardsControlArns = Prelude.Nothing,
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

-- | The requirement that underlies a control in the compliance framework
-- related to the standard.
standardsControlAssociationDetail_relatedRequirements :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe [Prelude.Text])
standardsControlAssociationDetail_relatedRequirements = Lens.lens (\StandardsControlAssociationDetail' {relatedRequirements} -> relatedRequirements) (\s@StandardsControlAssociationDetail' {} a -> s {relatedRequirements = a} :: StandardsControlAssociationDetail) Prelude.. Lens.mapping Lens.coerced

-- | Provides the input parameter that Security Hub uses to call the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_UpdateStandardsControl.html UpdateStandardsControl>
-- API. This API can be used to enable or disable a control in a specified
-- standard.
standardsControlAssociationDetail_standardsControlArns :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe [Prelude.Text])
standardsControlAssociationDetail_standardsControlArns = Lens.lens (\StandardsControlAssociationDetail' {standardsControlArns} -> standardsControlArns) (\s@StandardsControlAssociationDetail' {} a -> s {standardsControlArns = a} :: StandardsControlAssociationDetail) Prelude.. Lens.mapping Lens.coerced

-- | The description of a control. This typically summarizes how Security Hub
-- evaluates the control and the conditions under which it produces a
-- failed finding. This parameter may reference a specific standard.
standardsControlAssociationDetail_standardsControlDescription :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe Prelude.Text)
standardsControlAssociationDetail_standardsControlDescription = Lens.lens (\StandardsControlAssociationDetail' {standardsControlDescription} -> standardsControlDescription) (\s@StandardsControlAssociationDetail' {} a -> s {standardsControlDescription = a} :: StandardsControlAssociationDetail)

-- | The title of a control. This field may reference a specific standard.
standardsControlAssociationDetail_standardsControlTitle :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe Prelude.Text)
standardsControlAssociationDetail_standardsControlTitle = Lens.lens (\StandardsControlAssociationDetail' {standardsControlTitle} -> standardsControlTitle) (\s@StandardsControlAssociationDetail' {} a -> s {standardsControlTitle = a} :: StandardsControlAssociationDetail)

-- | The time at which the enablement status of the control in the specified
-- standard was last updated.
standardsControlAssociationDetail_updatedAt :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe Prelude.UTCTime)
standardsControlAssociationDetail_updatedAt = Lens.lens (\StandardsControlAssociationDetail' {updatedAt} -> updatedAt) (\s@StandardsControlAssociationDetail' {} a -> s {updatedAt = a} :: StandardsControlAssociationDetail) Prelude.. Lens.mapping Data._Time

-- | The reason for updating the enablement status of a control in a
-- specified standard.
standardsControlAssociationDetail_updatedReason :: Lens.Lens' StandardsControlAssociationDetail (Prelude.Maybe Prelude.Text)
standardsControlAssociationDetail_updatedReason = Lens.lens (\StandardsControlAssociationDetail' {updatedReason} -> updatedReason) (\s@StandardsControlAssociationDetail' {} a -> s {updatedReason = a} :: StandardsControlAssociationDetail)

-- | The Amazon Resource Name (ARN) of a security standard.
standardsControlAssociationDetail_standardsArn :: Lens.Lens' StandardsControlAssociationDetail Prelude.Text
standardsControlAssociationDetail_standardsArn = Lens.lens (\StandardsControlAssociationDetail' {standardsArn} -> standardsArn) (\s@StandardsControlAssociationDetail' {} a -> s {standardsArn = a} :: StandardsControlAssociationDetail)

-- | The unique identifier of a security control across standards. Values for
-- this field typically consist of an Amazon Web Service name and a number,
-- such as APIGateway.3.
standardsControlAssociationDetail_securityControlId :: Lens.Lens' StandardsControlAssociationDetail Prelude.Text
standardsControlAssociationDetail_securityControlId = Lens.lens (\StandardsControlAssociationDetail' {securityControlId} -> securityControlId) (\s@StandardsControlAssociationDetail' {} a -> s {securityControlId = a} :: StandardsControlAssociationDetail)

-- | The ARN of a security control across standards, such as
-- @arn:aws:securityhub:eu-central-1:123456789012:security-control\/S3.1@.
-- This parameter doesn\'t mention a specific standard.
standardsControlAssociationDetail_securityControlArn :: Lens.Lens' StandardsControlAssociationDetail Prelude.Text
standardsControlAssociationDetail_securityControlArn = Lens.lens (\StandardsControlAssociationDetail' {securityControlArn} -> securityControlArn) (\s@StandardsControlAssociationDetail' {} a -> s {securityControlArn = a} :: StandardsControlAssociationDetail)

-- | Specifies whether a control is enabled or disabled in a specified
-- standard.
standardsControlAssociationDetail_associationStatus :: Lens.Lens' StandardsControlAssociationDetail AssociationStatus
standardsControlAssociationDetail_associationStatus = Lens.lens (\StandardsControlAssociationDetail' {associationStatus} -> associationStatus) (\s@StandardsControlAssociationDetail' {} a -> s {associationStatus = a} :: StandardsControlAssociationDetail)

instance
  Data.FromJSON
    StandardsControlAssociationDetail
  where
  parseJSON =
    Data.withObject
      "StandardsControlAssociationDetail"
      ( \x ->
          StandardsControlAssociationDetail'
            Prelude.<$> ( x
                            Data..:? "RelatedRequirements"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "StandardsControlArns"
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
    StandardsControlAssociationDetail
  where
  hashWithSalt
    _salt
    StandardsControlAssociationDetail' {..} =
      _salt
        `Prelude.hashWithSalt` relatedRequirements
        `Prelude.hashWithSalt` standardsControlArns
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
    StandardsControlAssociationDetail
  where
  rnf StandardsControlAssociationDetail' {..} =
    Prelude.rnf relatedRequirements
      `Prelude.seq` Prelude.rnf standardsControlArns
      `Prelude.seq` Prelude.rnf standardsControlDescription
      `Prelude.seq` Prelude.rnf standardsControlTitle
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf updatedReason
      `Prelude.seq` Prelude.rnf standardsArn
      `Prelude.seq` Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf securityControlArn
      `Prelude.seq` Prelude.rnf associationStatus
