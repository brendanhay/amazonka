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
-- Module      : Amazonka.CleanRooms.GetConfiguredTableAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a configured table association.
module Amazonka.CleanRooms.GetConfiguredTableAssociation
  ( -- * Creating a Request
    GetConfiguredTableAssociation (..),
    newGetConfiguredTableAssociation,

    -- * Request Lenses
    getConfiguredTableAssociation_configuredTableAssociationIdentifier,
    getConfiguredTableAssociation_membershipIdentifier,

    -- * Destructuring the Response
    GetConfiguredTableAssociationResponse (..),
    newGetConfiguredTableAssociationResponse,

    -- * Response Lenses
    getConfiguredTableAssociationResponse_httpStatus,
    getConfiguredTableAssociationResponse_configuredTableAssociation,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConfiguredTableAssociation' smart constructor.
data GetConfiguredTableAssociation = GetConfiguredTableAssociation'
  { -- | The unique ID for the configured table association to retrieve.
    -- Currently accepts the configured table ID.
    configuredTableAssociationIdentifier :: Prelude.Text,
    -- | A unique identifier for the membership that the configured table
    -- association belongs to. Currently accepts the membership ID.
    membershipIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTableAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableAssociationIdentifier', 'getConfiguredTableAssociation_configuredTableAssociationIdentifier' - The unique ID for the configured table association to retrieve.
-- Currently accepts the configured table ID.
--
-- 'membershipIdentifier', 'getConfiguredTableAssociation_membershipIdentifier' - A unique identifier for the membership that the configured table
-- association belongs to. Currently accepts the membership ID.
newGetConfiguredTableAssociation ::
  -- | 'configuredTableAssociationIdentifier'
  Prelude.Text ->
  -- | 'membershipIdentifier'
  Prelude.Text ->
  GetConfiguredTableAssociation
newGetConfiguredTableAssociation
  pConfiguredTableAssociationIdentifier_
  pMembershipIdentifier_ =
    GetConfiguredTableAssociation'
      { configuredTableAssociationIdentifier =
          pConfiguredTableAssociationIdentifier_,
        membershipIdentifier =
          pMembershipIdentifier_
      }

-- | The unique ID for the configured table association to retrieve.
-- Currently accepts the configured table ID.
getConfiguredTableAssociation_configuredTableAssociationIdentifier :: Lens.Lens' GetConfiguredTableAssociation Prelude.Text
getConfiguredTableAssociation_configuredTableAssociationIdentifier = Lens.lens (\GetConfiguredTableAssociation' {configuredTableAssociationIdentifier} -> configuredTableAssociationIdentifier) (\s@GetConfiguredTableAssociation' {} a -> s {configuredTableAssociationIdentifier = a} :: GetConfiguredTableAssociation)

-- | A unique identifier for the membership that the configured table
-- association belongs to. Currently accepts the membership ID.
getConfiguredTableAssociation_membershipIdentifier :: Lens.Lens' GetConfiguredTableAssociation Prelude.Text
getConfiguredTableAssociation_membershipIdentifier = Lens.lens (\GetConfiguredTableAssociation' {membershipIdentifier} -> membershipIdentifier) (\s@GetConfiguredTableAssociation' {} a -> s {membershipIdentifier = a} :: GetConfiguredTableAssociation)

instance
  Core.AWSRequest
    GetConfiguredTableAssociation
  where
  type
    AWSResponse GetConfiguredTableAssociation =
      GetConfiguredTableAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfiguredTableAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTableAssociation")
      )

instance
  Prelude.Hashable
    GetConfiguredTableAssociation
  where
  hashWithSalt _salt GetConfiguredTableAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` configuredTableAssociationIdentifier
      `Prelude.hashWithSalt` membershipIdentifier

instance Prelude.NFData GetConfiguredTableAssociation where
  rnf GetConfiguredTableAssociation' {..} =
    Prelude.rnf configuredTableAssociationIdentifier
      `Prelude.seq` Prelude.rnf membershipIdentifier

instance Data.ToHeaders GetConfiguredTableAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConfiguredTableAssociation where
  toPath GetConfiguredTableAssociation' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/configuredTableAssociations/",
        Data.toBS configuredTableAssociationIdentifier
      ]

instance Data.ToQuery GetConfiguredTableAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfiguredTableAssociationResponse' smart constructor.
data GetConfiguredTableAssociationResponse = GetConfiguredTableAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire configured table association object.
    configuredTableAssociation :: ConfiguredTableAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTableAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getConfiguredTableAssociationResponse_httpStatus' - The response's http status code.
--
-- 'configuredTableAssociation', 'getConfiguredTableAssociationResponse_configuredTableAssociation' - The entire configured table association object.
newGetConfiguredTableAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTableAssociation'
  ConfiguredTableAssociation ->
  GetConfiguredTableAssociationResponse
newGetConfiguredTableAssociationResponse
  pHttpStatus_
  pConfiguredTableAssociation_ =
    GetConfiguredTableAssociationResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTableAssociation =
          pConfiguredTableAssociation_
      }

-- | The response's http status code.
getConfiguredTableAssociationResponse_httpStatus :: Lens.Lens' GetConfiguredTableAssociationResponse Prelude.Int
getConfiguredTableAssociationResponse_httpStatus = Lens.lens (\GetConfiguredTableAssociationResponse' {httpStatus} -> httpStatus) (\s@GetConfiguredTableAssociationResponse' {} a -> s {httpStatus = a} :: GetConfiguredTableAssociationResponse)

-- | The entire configured table association object.
getConfiguredTableAssociationResponse_configuredTableAssociation :: Lens.Lens' GetConfiguredTableAssociationResponse ConfiguredTableAssociation
getConfiguredTableAssociationResponse_configuredTableAssociation = Lens.lens (\GetConfiguredTableAssociationResponse' {configuredTableAssociation} -> configuredTableAssociation) (\s@GetConfiguredTableAssociationResponse' {} a -> s {configuredTableAssociation = a} :: GetConfiguredTableAssociationResponse)

instance
  Prelude.NFData
    GetConfiguredTableAssociationResponse
  where
  rnf GetConfiguredTableAssociationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTableAssociation
