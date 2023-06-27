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
-- Module      : Amazonka.FinSpace.ListKxEnvironments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of kdb environments created in an account.
--
-- This operation returns paginated results.
module Amazonka.FinSpace.ListKxEnvironments
  ( -- * Creating a Request
    ListKxEnvironments (..),
    newListKxEnvironments,

    -- * Request Lenses
    listKxEnvironments_maxResults,
    listKxEnvironments_nextToken,

    -- * Destructuring the Response
    ListKxEnvironmentsResponse (..),
    newListKxEnvironmentsResponse,

    -- * Response Lenses
    listKxEnvironmentsResponse_environments,
    listKxEnvironmentsResponse_nextToken,
    listKxEnvironmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKxEnvironments' smart constructor.
data ListKxEnvironments = ListKxEnvironments'
  { -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKxEnvironments_maxResults' - The maximum number of results to return in this request.
--
-- 'nextToken', 'listKxEnvironments_nextToken' - A token that indicates where a results page should begin.
newListKxEnvironments ::
  ListKxEnvironments
newListKxEnvironments =
  ListKxEnvironments'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in this request.
listKxEnvironments_maxResults :: Lens.Lens' ListKxEnvironments (Prelude.Maybe Prelude.Int)
listKxEnvironments_maxResults = Lens.lens (\ListKxEnvironments' {maxResults} -> maxResults) (\s@ListKxEnvironments' {} a -> s {maxResults = a} :: ListKxEnvironments)

-- | A token that indicates where a results page should begin.
listKxEnvironments_nextToken :: Lens.Lens' ListKxEnvironments (Prelude.Maybe Prelude.Text)
listKxEnvironments_nextToken = Lens.lens (\ListKxEnvironments' {nextToken} -> nextToken) (\s@ListKxEnvironments' {} a -> s {nextToken = a} :: ListKxEnvironments)

instance Core.AWSPager ListKxEnvironments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKxEnvironmentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listKxEnvironmentsResponse_environments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listKxEnvironments_nextToken
          Lens..~ rs
          Lens.^? listKxEnvironmentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListKxEnvironments where
  type
    AWSResponse ListKxEnvironments =
      ListKxEnvironmentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKxEnvironmentsResponse'
            Prelude.<$> (x Data..?> "environments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKxEnvironments where
  hashWithSalt _salt ListKxEnvironments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKxEnvironments where
  rnf ListKxEnvironments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListKxEnvironments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKxEnvironments where
  toPath = Prelude.const "/kx/environments"

instance Data.ToQuery ListKxEnvironments where
  toQuery ListKxEnvironments' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListKxEnvironmentsResponse' smart constructor.
data ListKxEnvironmentsResponse = ListKxEnvironmentsResponse'
  { -- | A list of environments in an account.
    environments :: Prelude.Maybe [KxEnvironment],
    -- | A token that indicates where a results page should begin.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKxEnvironmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environments', 'listKxEnvironmentsResponse_environments' - A list of environments in an account.
--
-- 'nextToken', 'listKxEnvironmentsResponse_nextToken' - A token that indicates where a results page should begin.
--
-- 'httpStatus', 'listKxEnvironmentsResponse_httpStatus' - The response's http status code.
newListKxEnvironmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKxEnvironmentsResponse
newListKxEnvironmentsResponse pHttpStatus_ =
  ListKxEnvironmentsResponse'
    { environments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of environments in an account.
listKxEnvironmentsResponse_environments :: Lens.Lens' ListKxEnvironmentsResponse (Prelude.Maybe [KxEnvironment])
listKxEnvironmentsResponse_environments = Lens.lens (\ListKxEnvironmentsResponse' {environments} -> environments) (\s@ListKxEnvironmentsResponse' {} a -> s {environments = a} :: ListKxEnvironmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates where a results page should begin.
listKxEnvironmentsResponse_nextToken :: Lens.Lens' ListKxEnvironmentsResponse (Prelude.Maybe Prelude.Text)
listKxEnvironmentsResponse_nextToken = Lens.lens (\ListKxEnvironmentsResponse' {nextToken} -> nextToken) (\s@ListKxEnvironmentsResponse' {} a -> s {nextToken = a} :: ListKxEnvironmentsResponse)

-- | The response's http status code.
listKxEnvironmentsResponse_httpStatus :: Lens.Lens' ListKxEnvironmentsResponse Prelude.Int
listKxEnvironmentsResponse_httpStatus = Lens.lens (\ListKxEnvironmentsResponse' {httpStatus} -> httpStatus) (\s@ListKxEnvironmentsResponse' {} a -> s {httpStatus = a} :: ListKxEnvironmentsResponse)

instance Prelude.NFData ListKxEnvironmentsResponse where
  rnf ListKxEnvironmentsResponse' {..} =
    Prelude.rnf environments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
