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
-- Module      : Amazonka.Glue.ListStatements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists statements for the session.
module Amazonka.Glue.ListStatements
  ( -- * Creating a Request
    ListStatements (..),
    newListStatements,

    -- * Request Lenses
    listStatements_nextToken,
    listStatements_requestOrigin,
    listStatements_sessionId,

    -- * Destructuring the Response
    ListStatementsResponse (..),
    newListStatementsResponse,

    -- * Response Lenses
    listStatementsResponse_nextToken,
    listStatementsResponse_statements,
    listStatementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStatements' smart constructor.
data ListStatements = ListStatements'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The origin of the request to list statements.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The Session ID of the statements.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStatements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStatements_nextToken' - A continuation token, if this is a continuation call.
--
-- 'requestOrigin', 'listStatements_requestOrigin' - The origin of the request to list statements.
--
-- 'sessionId', 'listStatements_sessionId' - The Session ID of the statements.
newListStatements ::
  -- | 'sessionId'
  Prelude.Text ->
  ListStatements
newListStatements pSessionId_ =
  ListStatements'
    { nextToken = Prelude.Nothing,
      requestOrigin = Prelude.Nothing,
      sessionId = pSessionId_
    }

-- | A continuation token, if this is a continuation call.
listStatements_nextToken :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Text)
listStatements_nextToken = Lens.lens (\ListStatements' {nextToken} -> nextToken) (\s@ListStatements' {} a -> s {nextToken = a} :: ListStatements)

-- | The origin of the request to list statements.
listStatements_requestOrigin :: Lens.Lens' ListStatements (Prelude.Maybe Prelude.Text)
listStatements_requestOrigin = Lens.lens (\ListStatements' {requestOrigin} -> requestOrigin) (\s@ListStatements' {} a -> s {requestOrigin = a} :: ListStatements)

-- | The Session ID of the statements.
listStatements_sessionId :: Lens.Lens' ListStatements Prelude.Text
listStatements_sessionId = Lens.lens (\ListStatements' {sessionId} -> sessionId) (\s@ListStatements' {} a -> s {sessionId = a} :: ListStatements)

instance Core.AWSRequest ListStatements where
  type
    AWSResponse ListStatements =
      ListStatementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStatementsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Statements" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStatements where
  hashWithSalt _salt ListStatements' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` sessionId

instance Prelude.NFData ListStatements where
  rnf ListStatements' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf sessionId

instance Data.ToHeaders ListStatements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ListStatements" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStatements where
  toJSON ListStatements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            Prelude.Just ("SessionId" Data..= sessionId)
          ]
      )

instance Data.ToPath ListStatements where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStatements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStatementsResponse' smart constructor.
data ListStatementsResponse = ListStatementsResponse'
  { -- | A continuation token, if not all statements have yet been returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the list of statements.
    statements :: Prelude.Maybe [Statement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStatementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStatementsResponse_nextToken' - A continuation token, if not all statements have yet been returned.
--
-- 'statements', 'listStatementsResponse_statements' - Returns the list of statements.
--
-- 'httpStatus', 'listStatementsResponse_httpStatus' - The response's http status code.
newListStatementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStatementsResponse
newListStatementsResponse pHttpStatus_ =
  ListStatementsResponse'
    { nextToken =
        Prelude.Nothing,
      statements = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all statements have yet been returned.
listStatementsResponse_nextToken :: Lens.Lens' ListStatementsResponse (Prelude.Maybe Prelude.Text)
listStatementsResponse_nextToken = Lens.lens (\ListStatementsResponse' {nextToken} -> nextToken) (\s@ListStatementsResponse' {} a -> s {nextToken = a} :: ListStatementsResponse)

-- | Returns the list of statements.
listStatementsResponse_statements :: Lens.Lens' ListStatementsResponse (Prelude.Maybe [Statement])
listStatementsResponse_statements = Lens.lens (\ListStatementsResponse' {statements} -> statements) (\s@ListStatementsResponse' {} a -> s {statements = a} :: ListStatementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStatementsResponse_httpStatus :: Lens.Lens' ListStatementsResponse Prelude.Int
listStatementsResponse_httpStatus = Lens.lens (\ListStatementsResponse' {httpStatus} -> httpStatus) (\s@ListStatementsResponse' {} a -> s {httpStatus = a} :: ListStatementsResponse)

instance Prelude.NFData ListStatementsResponse where
  rnf ListStatementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf statements
      `Prelude.seq` Prelude.rnf httpStatus
