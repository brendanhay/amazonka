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
-- Module      : Amazonka.CleanRooms.StartProtectedQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a protected query that is started by AWS Clean Rooms.
module Amazonka.CleanRooms.StartProtectedQuery
  ( -- * Creating a Request
    StartProtectedQuery (..),
    newStartProtectedQuery,

    -- * Request Lenses
    startProtectedQuery_type,
    startProtectedQuery_membershipIdentifier,
    startProtectedQuery_sqlParameters,
    startProtectedQuery_resultConfiguration,

    -- * Destructuring the Response
    StartProtectedQueryResponse (..),
    newStartProtectedQueryResponse,

    -- * Response Lenses
    startProtectedQueryResponse_httpStatus,
    startProtectedQueryResponse_protectedQuery,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartProtectedQuery' smart constructor.
data StartProtectedQuery = StartProtectedQuery'
  { -- | The type of the protected query to be started.
    type' :: ProtectedQueryType,
    -- | A unique identifier for the membership to run this query against.
    -- Currently accepts a membership ID.
    membershipIdentifier :: Prelude.Text,
    -- | The protected SQL query parameters.
    sqlParameters :: Data.Sensitive ProtectedQuerySQLParameters,
    -- | The details needed to write the query results.
    resultConfiguration :: ProtectedQueryResultConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProtectedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'startProtectedQuery_type' - The type of the protected query to be started.
--
-- 'membershipIdentifier', 'startProtectedQuery_membershipIdentifier' - A unique identifier for the membership to run this query against.
-- Currently accepts a membership ID.
--
-- 'sqlParameters', 'startProtectedQuery_sqlParameters' - The protected SQL query parameters.
--
-- 'resultConfiguration', 'startProtectedQuery_resultConfiguration' - The details needed to write the query results.
newStartProtectedQuery ::
  -- | 'type''
  ProtectedQueryType ->
  -- | 'membershipIdentifier'
  Prelude.Text ->
  -- | 'sqlParameters'
  ProtectedQuerySQLParameters ->
  -- | 'resultConfiguration'
  ProtectedQueryResultConfiguration ->
  StartProtectedQuery
newStartProtectedQuery
  pType_
  pMembershipIdentifier_
  pSqlParameters_
  pResultConfiguration_ =
    StartProtectedQuery'
      { type' = pType_,
        membershipIdentifier = pMembershipIdentifier_,
        sqlParameters =
          Data._Sensitive Lens.# pSqlParameters_,
        resultConfiguration = pResultConfiguration_
      }

-- | The type of the protected query to be started.
startProtectedQuery_type :: Lens.Lens' StartProtectedQuery ProtectedQueryType
startProtectedQuery_type = Lens.lens (\StartProtectedQuery' {type'} -> type') (\s@StartProtectedQuery' {} a -> s {type' = a} :: StartProtectedQuery)

-- | A unique identifier for the membership to run this query against.
-- Currently accepts a membership ID.
startProtectedQuery_membershipIdentifier :: Lens.Lens' StartProtectedQuery Prelude.Text
startProtectedQuery_membershipIdentifier = Lens.lens (\StartProtectedQuery' {membershipIdentifier} -> membershipIdentifier) (\s@StartProtectedQuery' {} a -> s {membershipIdentifier = a} :: StartProtectedQuery)

-- | The protected SQL query parameters.
startProtectedQuery_sqlParameters :: Lens.Lens' StartProtectedQuery ProtectedQuerySQLParameters
startProtectedQuery_sqlParameters = Lens.lens (\StartProtectedQuery' {sqlParameters} -> sqlParameters) (\s@StartProtectedQuery' {} a -> s {sqlParameters = a} :: StartProtectedQuery) Prelude.. Data._Sensitive

-- | The details needed to write the query results.
startProtectedQuery_resultConfiguration :: Lens.Lens' StartProtectedQuery ProtectedQueryResultConfiguration
startProtectedQuery_resultConfiguration = Lens.lens (\StartProtectedQuery' {resultConfiguration} -> resultConfiguration) (\s@StartProtectedQuery' {} a -> s {resultConfiguration = a} :: StartProtectedQuery)

instance Core.AWSRequest StartProtectedQuery where
  type
    AWSResponse StartProtectedQuery =
      StartProtectedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartProtectedQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "protectedQuery")
      )

instance Prelude.Hashable StartProtectedQuery where
  hashWithSalt _salt StartProtectedQuery' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` membershipIdentifier
      `Prelude.hashWithSalt` sqlParameters
      `Prelude.hashWithSalt` resultConfiguration

instance Prelude.NFData StartProtectedQuery where
  rnf StartProtectedQuery' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf membershipIdentifier
      `Prelude.seq` Prelude.rnf sqlParameters
      `Prelude.seq` Prelude.rnf resultConfiguration

instance Data.ToHeaders StartProtectedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartProtectedQuery where
  toJSON StartProtectedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("type" Data..= type'),
            Prelude.Just ("sqlParameters" Data..= sqlParameters),
            Prelude.Just
              ("resultConfiguration" Data..= resultConfiguration)
          ]
      )

instance Data.ToPath StartProtectedQuery where
  toPath StartProtectedQuery' {..} =
    Prelude.mconcat
      [ "/memberships/",
        Data.toBS membershipIdentifier,
        "/protectedQueries"
      ]

instance Data.ToQuery StartProtectedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartProtectedQueryResponse' smart constructor.
data StartProtectedQueryResponse = StartProtectedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The protected query.
    protectedQuery :: ProtectedQuery
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartProtectedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startProtectedQueryResponse_httpStatus' - The response's http status code.
--
-- 'protectedQuery', 'startProtectedQueryResponse_protectedQuery' - The protected query.
newStartProtectedQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'protectedQuery'
  ProtectedQuery ->
  StartProtectedQueryResponse
newStartProtectedQueryResponse
  pHttpStatus_
  pProtectedQuery_ =
    StartProtectedQueryResponse'
      { httpStatus =
          pHttpStatus_,
        protectedQuery = pProtectedQuery_
      }

-- | The response's http status code.
startProtectedQueryResponse_httpStatus :: Lens.Lens' StartProtectedQueryResponse Prelude.Int
startProtectedQueryResponse_httpStatus = Lens.lens (\StartProtectedQueryResponse' {httpStatus} -> httpStatus) (\s@StartProtectedQueryResponse' {} a -> s {httpStatus = a} :: StartProtectedQueryResponse)

-- | The protected query.
startProtectedQueryResponse_protectedQuery :: Lens.Lens' StartProtectedQueryResponse ProtectedQuery
startProtectedQueryResponse_protectedQuery = Lens.lens (\StartProtectedQueryResponse' {protectedQuery} -> protectedQuery) (\s@StartProtectedQueryResponse' {} a -> s {protectedQuery = a} :: StartProtectedQueryResponse)

instance Prelude.NFData StartProtectedQueryResponse where
  rnf StartProtectedQueryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf protectedQuery
