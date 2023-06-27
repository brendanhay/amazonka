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
-- Module      : Amazonka.CleanRooms.DeleteConfiguredTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configured table association.
module Amazonka.CleanRooms.DeleteConfiguredTableAssociation
  ( -- * Creating a Request
    DeleteConfiguredTableAssociation (..),
    newDeleteConfiguredTableAssociation,

    -- * Request Lenses
    deleteConfiguredTableAssociation_configuredTableAssociationIdentifier,
    deleteConfiguredTableAssociation_membershipIdentifier,

    -- * Destructuring the Response
    DeleteConfiguredTableAssociationResponse (..),
    newDeleteConfiguredTableAssociationResponse,

    -- * Response Lenses
    deleteConfiguredTableAssociationResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfiguredTableAssociation' smart constructor.
data DeleteConfiguredTableAssociation = DeleteConfiguredTableAssociation'
  { -- | The unique ID for the configured table association to be deleted.
    -- Currently accepts the configured table ID.
    configuredTableAssociationIdentifier :: Prelude.Text,
    -- | A unique identifier for the membership that the configured table
    -- association belongs to. Currently accepts the membership ID.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableAssociationIdentifier', 'deleteConfiguredTableAssociation_configuredTableAssociationIdentifier' - The unique ID for the configured table association to be deleted.
-- Currently accepts the configured table ID.
--
-- 'membershipIdentifier', 'deleteConfiguredTableAssociation_membershipIdentifier' - A unique identifier for the membership that the configured table
-- association belongs to. Currently accepts the membership ID.
newDeleteConfiguredTableAssociation ::
  -- | 'configuredTableAssociationIdentifier'
  Prelude.Text ->
  -- | 'membershipIdentifier'
  Prelude.Text ->
  DeleteConfiguredTableAssociation
newDeleteConfiguredTableAssociation
  pConfiguredTableAssociationIdentifier_
  pMembershipIdentifier_ =
    DeleteConfiguredTableAssociation'
      { configuredTableAssociationIdentifier =
          pConfiguredTableAssociationIdentifier_,
        membershipIdentifier =
          pMembershipIdentifier_
      }

-- | The unique ID for the configured table association to be deleted.
-- Currently accepts the configured table ID.
deleteConfiguredTableAssociation_configuredTableAssociationIdentifier :: Lens.Lens' DeleteConfiguredTableAssociation Prelude.Text
deleteConfiguredTableAssociation_configuredTableAssociationIdentifier = Lens.lens (\DeleteConfiguredTableAssociation' {configuredTableAssociationIdentifier} -> configuredTableAssociationIdentifier) (\s@DeleteConfiguredTableAssociation' {} a -> s {configuredTableAssociationIdentifier = a} :: DeleteConfiguredTableAssociation)

-- | A unique identifier for the membership that the configured table
-- association belongs to. Currently accepts the membership ID.
deleteConfiguredTableAssociation_membershipIdentifier :: Lens.Lens' DeleteConfiguredTableAssociation Prelude.Text
deleteConfiguredTableAssociation_membershipIdentifier = Lens.lens (\DeleteConfiguredTableAssociation' {membershipIdentifier} -> membershipIdentifier) (\s@DeleteConfiguredTableAssociation' {} a -> s {membershipIdentifier = a} :: DeleteConfiguredTableAssociation)

instance
  Core.AWSRequest
    DeleteConfiguredTableAssociation
  where
  type
    AWSResponse DeleteConfiguredTableAssociation =
      DeleteConfiguredTableAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConfiguredTableAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteConfiguredTableAssociation
  where
  hashWithSalt
    _salt
    DeleteConfiguredTableAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableAssociationIdentifier
        `Prelude.hashWithSalt` membershipIdentifier

instance
  Prelude.NFData
    DeleteConfiguredTableAssociation
  where
  rnf DeleteConfiguredTableAssociation' {..} =
    Prelude.rnf configuredTableAssociationIdentifier
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance
  Data.ToHeaders
    DeleteConfiguredTableAssociation
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

instance Data.ToPath DeleteConfiguredTableAssociation where
  toPath DeleteConfiguredTableAssociation' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/configuredTableAssociations/",
        Data.toBS configuredTableAssociationIdentifier
      ]

instance
  Data.ToQuery
    DeleteConfiguredTableAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfiguredTableAssociationResponse' smart constructor.
data DeleteConfiguredTableAssociationResponse = DeleteConfiguredTableAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTableAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfiguredTableAssociationResponse_httpStatus' - The response's http status code.
newDeleteConfiguredTableAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfiguredTableAssociationResponse
newDeleteConfiguredTableAssociationResponse
  pHttpStatus_ =
    DeleteConfiguredTableAssociationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfiguredTableAssociationResponse_httpStatus :: Lens.Lens' DeleteConfiguredTableAssociationResponse Prelude.Int
deleteConfiguredTableAssociationResponse_httpStatus = Lens.lens (\DeleteConfiguredTableAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteConfiguredTableAssociationResponse' {} a -> s {httpStatus = a} :: DeleteConfiguredTableAssociationResponse)

instance
  Prelude.NFData
    DeleteConfiguredTableAssociationResponse
  where
  rnf DeleteConfiguredTableAssociationResponse' {..} =
    Prelude.rnf httpStatus
