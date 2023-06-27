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
-- Module      : Amazonka.CleanRooms.GetProtectedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns query processing metadata.
module Amazonka.CleanRooms.GetProtectedQuery
  ( -- * Creating a Request
    GetProtectedQuery (..),
    newGetProtectedQuery,

    -- * Request Lenses
    getProtectedQuery_membershipIdentifier,
    getProtectedQuery_protectedQueryIdentifier,

    -- * Destructuring the Response
    GetProtectedQueryResponse (..),
    newGetProtectedQueryResponse,

    -- * Response Lenses
    getProtectedQueryResponse_httpStatus,
    getProtectedQueryResponse_protectedQuery,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProtectedQuery' smart constructor.
data GetProtectedQuery = GetProtectedQuery'
  { -- | The identifier for a membership in a protected query instance.
    membershipIdentifier :: Prelude.Text,
    -- | The identifier for a protected query instance.
    protectedQueryIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProtectedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipIdentifier', 'getProtectedQuery_membershipIdentifier' - The identifier for a membership in a protected query instance.
--
-- 'protectedQueryIdentifier', 'getProtectedQuery_protectedQueryIdentifier' - The identifier for a protected query instance.
newGetProtectedQuery ::
  -- | 'membershipIdentifier'
  Prelude.Text ->
  -- | 'protectedQueryIdentifier'
  Prelude.Text ->
  GetProtectedQuery
newGetProtectedQuery
  pMembershipIdentifier_
  pProtectedQueryIdentifier_ =
    GetProtectedQuery'
      { membershipIdentifier =
          pMembershipIdentifier_,
        protectedQueryIdentifier =
          pProtectedQueryIdentifier_
      }

-- | The identifier for a membership in a protected query instance.
getProtectedQuery_membershipIdentifier :: Lens.Lens' GetProtectedQuery Prelude.Text
getProtectedQuery_membershipIdentifier = Lens.lens (\GetProtectedQuery' {membershipIdentifier} -> membershipIdentifier) (\s@GetProtectedQuery' {} a -> s {membershipIdentifier = a} :: GetProtectedQuery)

-- | The identifier for a protected query instance.
getProtectedQuery_protectedQueryIdentifier :: Lens.Lens' GetProtectedQuery Prelude.Text
getProtectedQuery_protectedQueryIdentifier = Lens.lens (\GetProtectedQuery' {protectedQueryIdentifier} -> protectedQueryIdentifier) (\s@GetProtectedQuery' {} a -> s {protectedQueryIdentifier = a} :: GetProtectedQuery)

instance Core.AWSRequest GetProtectedQuery where
  type
    AWSResponse GetProtectedQuery =
      GetProtectedQueryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProtectedQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "protectedQuery")
      )

instance Prelude.Hashable GetProtectedQuery where
  hashWithSalt _salt GetProtectedQuery' {..} =
    _salt
      `Prelude.hashWithSalt` membershipIdentifier
      `Prelude.hashWithSalt` protectedQueryIdentifier

instance Prelude.NFData GetProtectedQuery where
  rnf GetProtectedQuery' {..} =
    Prelude.rnf membershipIdentifier
      `Prelude.seq` Prelude.rnf protectedQueryIdentifier

instance Data.ToHeaders GetProtectedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProtectedQuery where
  toPath GetProtectedQuery' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/protectedQueries/",
        Data.toBS protectedQueryIdentifier
      ]

instance Data.ToQuery GetProtectedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProtectedQueryResponse' smart constructor.
data GetProtectedQueryResponse = GetProtectedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The query processing metadata.
    protectedQuery :: ProtectedQuery
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProtectedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getProtectedQueryResponse_httpStatus' - The response's http status code.
--
-- 'protectedQuery', 'getProtectedQueryResponse_protectedQuery' - The query processing metadata.
newGetProtectedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'protectedQuery'
  ProtectedQuery ->
  GetProtectedQueryResponse
newGetProtectedQueryResponse
  pHttpStatus_
  pProtectedQuery_ =
    GetProtectedQueryResponse'
      { httpStatus =
          pHttpStatus_,
        protectedQuery = pProtectedQuery_
      }

-- | The response's http status code.
getProtectedQueryResponse_httpStatus :: Lens.Lens' GetProtectedQueryResponse Prelude.Int
getProtectedQueryResponse_httpStatus = Lens.lens (\GetProtectedQueryResponse' {httpStatus} -> httpStatus) (\s@GetProtectedQueryResponse' {} a -> s {httpStatus = a} :: GetProtectedQueryResponse)

-- | The query processing metadata.
getProtectedQueryResponse_protectedQuery :: Lens.Lens' GetProtectedQueryResponse ProtectedQuery
getProtectedQueryResponse_protectedQuery = Lens.lens (\GetProtectedQueryResponse' {protectedQuery} -> protectedQuery) (\s@GetProtectedQueryResponse' {} a -> s {protectedQuery = a} :: GetProtectedQueryResponse)

instance Prelude.NFData GetProtectedQueryResponse where
  rnf GetProtectedQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protectedQuery
