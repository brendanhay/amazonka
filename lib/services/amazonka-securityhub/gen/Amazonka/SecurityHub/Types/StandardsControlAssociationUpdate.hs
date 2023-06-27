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
-- Module      : Amazonka.SecurityHub.Types.StandardsControlAssociationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsControlAssociationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociationStatus

-- | An array of requested updates to the enablement status of controls in
-- specified standards. The objects in the array include a security control
-- ID, the Amazon Resource Name (ARN) of the standard, the requested
-- enablement status, and the reason for updating the enablement status.
--
-- /See:/ 'newStandardsControlAssociationUpdate' smart constructor.
data StandardsControlAssociationUpdate = StandardsControlAssociationUpdate'
  { -- | The reason for updating the control\'s enablement status in the
    -- standard.
    updatedReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the standard in which you want to
    -- update the control\'s enablement status.
    standardsArn :: Prelude.Text,
    -- | The unique identifier for the security control whose enablement status
    -- you want to update.
    securityControlId :: Prelude.Text,
    -- | The desired enablement status of the control in the standard.
    associationStatus :: AssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsControlAssociationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatedReason', 'standardsControlAssociationUpdate_updatedReason' - The reason for updating the control\'s enablement status in the
-- standard.
--
-- 'standardsArn', 'standardsControlAssociationUpdate_standardsArn' - The Amazon Resource Name (ARN) of the standard in which you want to
-- update the control\'s enablement status.
--
-- 'securityControlId', 'standardsControlAssociationUpdate_securityControlId' - The unique identifier for the security control whose enablement status
-- you want to update.
--
-- 'associationStatus', 'standardsControlAssociationUpdate_associationStatus' - The desired enablement status of the control in the standard.
newStandardsControlAssociationUpdate ::
  -- | 'standardsArn'
  Prelude.Text ->
  -- | 'securityControlId'
  Prelude.Text ->
  -- | 'associationStatus'
  AssociationStatus ->
  StandardsControlAssociationUpdate
newStandardsControlAssociationUpdate
  pStandardsArn_
  pSecurityControlId_
  pAssociationStatus_ =
    StandardsControlAssociationUpdate'
      { updatedReason =
          Prelude.Nothing,
        standardsArn = pStandardsArn_,
        securityControlId = pSecurityControlId_,
        associationStatus = pAssociationStatus_
      }

-- | The reason for updating the control\'s enablement status in the
-- standard.
standardsControlAssociationUpdate_updatedReason :: Lens.Lens' StandardsControlAssociationUpdate (Prelude.Maybe Prelude.Text)
standardsControlAssociationUpdate_updatedReason = Lens.lens (\StandardsControlAssociationUpdate' {updatedReason} -> updatedReason) (\s@StandardsControlAssociationUpdate' {} a -> s {updatedReason = a} :: StandardsControlAssociationUpdate)

-- | The Amazon Resource Name (ARN) of the standard in which you want to
-- update the control\'s enablement status.
standardsControlAssociationUpdate_standardsArn :: Lens.Lens' StandardsControlAssociationUpdate Prelude.Text
standardsControlAssociationUpdate_standardsArn = Lens.lens (\StandardsControlAssociationUpdate' {standardsArn} -> standardsArn) (\s@StandardsControlAssociationUpdate' {} a -> s {standardsArn = a} :: StandardsControlAssociationUpdate)

-- | The unique identifier for the security control whose enablement status
-- you want to update.
standardsControlAssociationUpdate_securityControlId :: Lens.Lens' StandardsControlAssociationUpdate Prelude.Text
standardsControlAssociationUpdate_securityControlId = Lens.lens (\StandardsControlAssociationUpdate' {securityControlId} -> securityControlId) (\s@StandardsControlAssociationUpdate' {} a -> s {securityControlId = a} :: StandardsControlAssociationUpdate)

-- | The desired enablement status of the control in the standard.
standardsControlAssociationUpdate_associationStatus :: Lens.Lens' StandardsControlAssociationUpdate AssociationStatus
standardsControlAssociationUpdate_associationStatus = Lens.lens (\StandardsControlAssociationUpdate' {associationStatus} -> associationStatus) (\s@StandardsControlAssociationUpdate' {} a -> s {associationStatus = a} :: StandardsControlAssociationUpdate)

instance
  Data.FromJSON
    StandardsControlAssociationUpdate
  where
  parseJSON =
    Data.withObject
      "StandardsControlAssociationUpdate"
      ( \x ->
          StandardsControlAssociationUpdate'
            Prelude.<$> (x Data..:? "UpdatedReason")
            Prelude.<*> (x Data..: "StandardsArn")
            Prelude.<*> (x Data..: "SecurityControlId")
            Prelude.<*> (x Data..: "AssociationStatus")
      )

instance
  Prelude.Hashable
    StandardsControlAssociationUpdate
  where
  hashWithSalt
    _salt
    StandardsControlAssociationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` updatedReason
        `Prelude.hashWithSalt` standardsArn
        `Prelude.hashWithSalt` securityControlId
        `Prelude.hashWithSalt` associationStatus

instance
  Prelude.NFData
    StandardsControlAssociationUpdate
  where
  rnf StandardsControlAssociationUpdate' {..} =
    Prelude.rnf updatedReason
      `Prelude.seq` Prelude.rnf standardsArn
      `Prelude.seq` Prelude.rnf securityControlId
      `Prelude.seq` Prelude.rnf associationStatus

instance
  Data.ToJSON
    StandardsControlAssociationUpdate
  where
  toJSON StandardsControlAssociationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("UpdatedReason" Data..=) Prelude.<$> updatedReason,
            Prelude.Just ("StandardsArn" Data..= standardsArn),
            Prelude.Just
              ("SecurityControlId" Data..= securityControlId),
            Prelude.Just
              ("AssociationStatus" Data..= associationStatus)
          ]
      )
