{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CleanRooms.UpdateConfiguredTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a configured table association.
module Amazonka.CleanRooms.UpdateConfiguredTableAssociation
  ( -- * Creating a Request
    UpdateConfiguredTableAssociation (..),
    newUpdateConfiguredTableAssociation,

    -- * Request Lenses
    updateConfiguredTableAssociation_description,
    updateConfiguredTableAssociation_roleArn,
    updateConfiguredTableAssociation_configuredTableAssociationIdentifier,
    updateConfiguredTableAssociation_membershipIdentifier,

    -- * Destructuring the Response
    UpdateConfiguredTableAssociationResponse (..),
    newUpdateConfiguredTableAssociationResponse,

    -- * Response Lenses
    updateConfiguredTableAssociationResponse_httpStatus,
    updateConfiguredTableAssociationResponse_configuredTableAssociation,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfiguredTableAssociation' smart constructor.
data UpdateConfiguredTableAssociation = UpdateConfiguredTableAssociation'
  { -- | A new description for the configured table association.
    description :: Prelude.Maybe Prelude.Text,
    -- | The service will assume this role to access catalog metadata and query
    -- the table.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the configured table association to update.
    -- Currently accepts the configured table association ID.
    configuredTableAssociationIdentifier :: Prelude.Text,
    -- | The unique ID for the membership that the configured table association
    -- belongs to.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateConfiguredTableAssociation_description' - A new description for the configured table association.
--
-- 'roleArn', 'updateConfiguredTableAssociation_roleArn' - The service will assume this role to access catalog metadata and query
-- the table.
--
-- 'configuredTableAssociationIdentifier', 'updateConfiguredTableAssociation_configuredTableAssociationIdentifier' - The unique identifier for the configured table association to update.
-- Currently accepts the configured table association ID.
--
-- 'membershipIdentifier', 'updateConfiguredTableAssociation_membershipIdentifier' - The unique ID for the membership that the configured table association
-- belongs to.
newUpdateConfiguredTableAssociation ::
  -- | 'configuredTableAssociationIdentifier'
  Prelude.Text ->
  -- | 'membershipIdentifier'
  Prelude.Text ->
  UpdateConfiguredTableAssociation
newUpdateConfiguredTableAssociation
  pConfiguredTableAssociationIdentifier_
  pMembershipIdentifier_ =
    UpdateConfiguredTableAssociation'
      { description =
          Prelude.Nothing,
        roleArn = Prelude.Nothing,
        configuredTableAssociationIdentifier =
          pConfiguredTableAssociationIdentifier_,
        membershipIdentifier =
          pMembershipIdentifier_
      }

-- | A new description for the configured table association.
updateConfiguredTableAssociation_description :: Lens.Lens' UpdateConfiguredTableAssociation (Prelude.Maybe Prelude.Text)
updateConfiguredTableAssociation_description = Lens.lens (\UpdateConfiguredTableAssociation' {description} -> description) (\s@UpdateConfiguredTableAssociation' {} a -> s {description = a} :: UpdateConfiguredTableAssociation)

-- | The service will assume this role to access catalog metadata and query
-- the table.
updateConfiguredTableAssociation_roleArn :: Lens.Lens' UpdateConfiguredTableAssociation (Prelude.Maybe Prelude.Text)
updateConfiguredTableAssociation_roleArn = Lens.lens (\UpdateConfiguredTableAssociation' {roleArn} -> roleArn) (\s@UpdateConfiguredTableAssociation' {} a -> s {roleArn = a} :: UpdateConfiguredTableAssociation)

-- | The unique identifier for the configured table association to update.
-- Currently accepts the configured table association ID.
updateConfiguredTableAssociation_configuredTableAssociationIdentifier :: Lens.Lens' UpdateConfiguredTableAssociation Prelude.Text
updateConfiguredTableAssociation_configuredTableAssociationIdentifier = Lens.lens (\UpdateConfiguredTableAssociation' {configuredTableAssociationIdentifier} -> configuredTableAssociationIdentifier) (\s@UpdateConfiguredTableAssociation' {} a -> s {configuredTableAssociationIdentifier = a} :: UpdateConfiguredTableAssociation)

-- | The unique ID for the membership that the configured table association
-- belongs to.
updateConfiguredTableAssociation_membershipIdentifier :: Lens.Lens' UpdateConfiguredTableAssociation Prelude.Text
updateConfiguredTableAssociation_membershipIdentifier = Lens.lens (\UpdateConfiguredTableAssociation' {membershipIdentifier} -> membershipIdentifier) (\s@UpdateConfiguredTableAssociation' {} a -> s {membershipIdentifier = a} :: UpdateConfiguredTableAssociation)

instance
  Core.AWSRequest
    UpdateConfiguredTableAssociation
  where
  type
    AWSResponse UpdateConfiguredTableAssociation =
      UpdateConfiguredTableAssociationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConfiguredTableAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTableAssociation")
      )

instance
  Prelude.Hashable
    UpdateConfiguredTableAssociation
  where
  hashWithSalt
    _salt
    UpdateConfiguredTableAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` configuredTableAssociationIdentifier
        `Prelude.hashWithSalt` membershipIdentifier

instance
  Prelude.NFData
    UpdateConfiguredTableAssociation
  where
  rnf UpdateConfiguredTableAssociation' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf configuredTableAssociationIdentifier
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance
  Data.ToHeaders
    UpdateConfiguredTableAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConfiguredTableAssociation where
  toJSON UpdateConfiguredTableAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )

instance Data.ToPath UpdateConfiguredTableAssociation where
  toPath UpdateConfiguredTableAssociation' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/configuredTableAssociations/",
        Data.toBS configuredTableAssociationIdentifier
      ]

instance
  Data.ToQuery
    UpdateConfiguredTableAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConfiguredTableAssociationResponse' smart constructor.
data UpdateConfiguredTableAssociationResponse = UpdateConfiguredTableAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire updated configured table association.
    configuredTableAssociation :: ConfiguredTableAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTableAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfiguredTableAssociationResponse_httpStatus' - The response's http status code.
--
-- 'configuredTableAssociation', 'updateConfiguredTableAssociationResponse_configuredTableAssociation' - The entire updated configured table association.
newUpdateConfiguredTableAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTableAssociation'
  ConfiguredTableAssociation ->
  UpdateConfiguredTableAssociationResponse
newUpdateConfiguredTableAssociationResponse
  pHttpStatus_
  pConfiguredTableAssociation_ =
    UpdateConfiguredTableAssociationResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTableAssociation =
          pConfiguredTableAssociation_
      }

-- | The response's http status code.
updateConfiguredTableAssociationResponse_httpStatus :: Lens.Lens' UpdateConfiguredTableAssociationResponse Prelude.Int
updateConfiguredTableAssociationResponse_httpStatus = Lens.lens (\UpdateConfiguredTableAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateConfiguredTableAssociationResponse' {} a -> s {httpStatus = a} :: UpdateConfiguredTableAssociationResponse)

-- | The entire updated configured table association.
updateConfiguredTableAssociationResponse_configuredTableAssociation :: Lens.Lens' UpdateConfiguredTableAssociationResponse ConfiguredTableAssociation
updateConfiguredTableAssociationResponse_configuredTableAssociation = Lens.lens (\UpdateConfiguredTableAssociationResponse' {configuredTableAssociation} -> configuredTableAssociation) (\s@UpdateConfiguredTableAssociationResponse' {} a -> s {configuredTableAssociation = a} :: UpdateConfiguredTableAssociationResponse)

instance
  Prelude.NFData
    UpdateConfiguredTableAssociationResponse
  where
  rnf UpdateConfiguredTableAssociationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTableAssociation
