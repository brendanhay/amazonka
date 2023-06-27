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
-- Module      : Amazonka.CleanRooms.UpdateProtectedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the processing of a currently running query.
module Amazonka.CleanRooms.UpdateProtectedQuery
  ( -- * Creating a Request
    UpdateProtectedQuery (..),
    newUpdateProtectedQuery,

    -- * Request Lenses
    updateProtectedQuery_membershipIdentifier,
    updateProtectedQuery_protectedQueryIdentifier,
    updateProtectedQuery_targetStatus,

    -- * Destructuring the Response
    UpdateProtectedQueryResponse (..),
    newUpdateProtectedQueryResponse,

    -- * Response Lenses
    updateProtectedQueryResponse_httpStatus,
    updateProtectedQueryResponse_protectedQuery,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProtectedQuery' smart constructor.
data UpdateProtectedQuery = UpdateProtectedQuery'
  { -- | The identifier for a member of a protected query instance.
    membershipIdentifier :: Prelude.Text,
    -- | The identifier for a protected query instance.
    protectedQueryIdentifier :: Prelude.Text,
    -- | The target status of a query. Used to update the execution status of a
    -- currently running query.
    targetStatus :: TargetProtectedQueryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProtectedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipIdentifier', 'updateProtectedQuery_membershipIdentifier' - The identifier for a member of a protected query instance.
--
-- 'protectedQueryIdentifier', 'updateProtectedQuery_protectedQueryIdentifier' - The identifier for a protected query instance.
--
-- 'targetStatus', 'updateProtectedQuery_targetStatus' - The target status of a query. Used to update the execution status of a
-- currently running query.
newUpdateProtectedQuery ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  -- | 'protectedQueryIdentifier'
  Prelude.Text ->
  -- | 'targetStatus'
  TargetProtectedQueryStatus ->
  UpdateProtectedQuery
newUpdateProtectedQuery
  pMembershipIdentifier_
  pProtectedQueryIdentifier_
  pTargetStatus_ =
    UpdateProtectedQuery'
      { membershipIdentifier =
          pMembershipIdentifier_,
        protectedQueryIdentifier =
          pProtectedQueryIdentifier_,
        targetStatus = pTargetStatus_
      }

-- | The identifier for a member of a protected query instance.
updateProtectedQuery_membershipIdentifier :: Lens.Lens' UpdateProtectedQuery Prelude.Text
updateProtectedQuery_membershipIdentifier = Lens.lens (\UpdateProtectedQuery' {membershipIdentifier} -> membershipIdentifier) (\s@UpdateProtectedQuery' {} a -> s {membershipIdentifier = a} :: UpdateProtectedQuery)

-- | The identifier for a protected query instance.
updateProtectedQuery_protectedQueryIdentifier :: Lens.Lens' UpdateProtectedQuery Prelude.Text
updateProtectedQuery_protectedQueryIdentifier = Lens.lens (\UpdateProtectedQuery' {protectedQueryIdentifier} -> protectedQueryIdentifier) (\s@UpdateProtectedQuery' {} a -> s {protectedQueryIdentifier = a} :: UpdateProtectedQuery)

-- | The target status of a query. Used to update the execution status of a
-- currently running query.
updateProtectedQuery_targetStatus :: Lens.Lens' UpdateProtectedQuery TargetProtectedQueryStatus
updateProtectedQuery_targetStatus = Lens.lens (\UpdateProtectedQuery' {targetStatus} -> targetStatus) (\s@UpdateProtectedQuery' {} a -> s {targetStatus = a} :: UpdateProtectedQuery)

instance Core.AWSRequest UpdateProtectedQuery where
  type
    AWSResponse UpdateProtectedQuery =
      UpdateProtectedQueryResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProtectedQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "protectedQuery")
      )

instance Prelude.Hashable UpdateProtectedQuery where
  hashWithSalt _salt UpdateProtectedQuery' {..} =
    _salt
      `Prelude.hashWithSalt` membershipIdentifier
      `Prelude.hashWithSalt` protectedQueryIdentifier
      `Prelude.hashWithSalt` targetStatus

instance Prelude.NFData UpdateProtectedQuery where
  rnf UpdateProtectedQuery' {..} =
    Prelude.rnf membershipIdentifier
      `Prelude.seq` Prelude.rnf protectedQueryIdentifier
      `Prelude.seq` Prelude.rnf targetStatus

instance Data.ToHeaders UpdateProtectedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProtectedQuery where
  toJSON UpdateProtectedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("targetStatus" Data..= targetStatus)]
      )

instance Data.ToPath UpdateProtectedQuery where
  toPath UpdateProtectedQuery' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/protectedQueries/",
        Data.toBS protectedQueryIdentifier
      ]

instance Data.ToQuery UpdateProtectedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProtectedQueryResponse' smart constructor.
data UpdateProtectedQueryResponse = UpdateProtectedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The protected query output.
    protectedQuery :: ProtectedQuery
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProtectedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProtectedQueryResponse_httpStatus' - The response's http status code.
--
-- 'protectedQuery', 'updateProtectedQueryResponse_protectedQuery' - The protected query output.
newUpdateProtectedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'protectedQuery'
  ProtectedQuery ->
  UpdateProtectedQueryResponse
newUpdateProtectedQueryResponse
  pHttpStatus_
  pProtectedQuery_ =
    UpdateProtectedQueryResponse'
      { httpStatus =
          pHttpStatus_,
        protectedQuery = pProtectedQuery_
      }

-- | The response's http status code.
updateProtectedQueryResponse_httpStatus :: Lens.Lens' UpdateProtectedQueryResponse Prelude.Int
updateProtectedQueryResponse_httpStatus = Lens.lens (\UpdateProtectedQueryResponse' {httpStatus} -> httpStatus) (\s@UpdateProtectedQueryResponse' {} a -> s {httpStatus = a} :: UpdateProtectedQueryResponse)

-- | The protected query output.
updateProtectedQueryResponse_protectedQuery :: Lens.Lens' UpdateProtectedQueryResponse ProtectedQuery
updateProtectedQueryResponse_protectedQuery = Lens.lens (\UpdateProtectedQueryResponse' {protectedQuery} -> protectedQuery) (\s@UpdateProtectedQueryResponse' {} a -> s {protectedQuery = a} :: UpdateProtectedQueryResponse)

instance Prelude.NFData UpdateProtectedQueryResponse where
  rnf UpdateProtectedQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protectedQuery
