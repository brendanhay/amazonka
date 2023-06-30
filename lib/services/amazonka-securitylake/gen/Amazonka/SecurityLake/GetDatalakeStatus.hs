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
-- Module      : Amazonka.SecurityLake.GetDatalakeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a snapshot of the current Region, including whether Amazon
-- Security Lake is enabled for those accounts and which sources Security
-- Lake is collecting data from.
--
-- This operation returns paginated results.
module Amazonka.SecurityLake.GetDatalakeStatus
  ( -- * Creating a Request
    GetDatalakeStatus (..),
    newGetDatalakeStatus,

    -- * Request Lenses
    getDatalakeStatus_accountSet,
    getDatalakeStatus_maxAccountResults,
    getDatalakeStatus_nextToken,

    -- * Destructuring the Response
    GetDatalakeStatusResponse (..),
    newGetDatalakeStatusResponse,

    -- * Response Lenses
    getDatalakeStatusResponse_nextToken,
    getDatalakeStatusResponse_httpStatus,
    getDatalakeStatusResponse_accountSourcesList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDatalakeStatus' smart constructor.
data GetDatalakeStatus = GetDatalakeStatus'
  { -- | The Amazon Web Services account ID for which a static snapshot of the
    -- current Amazon Web Services Region, including enabled accounts and log
    -- sources, is retrieved.
    accountSet :: Prelude.Maybe [Prelude.Text],
    -- | The maximum limit of accounts for which the static snapshot of the
    -- current Region, including enabled accounts and log sources, is
    -- retrieved.
    maxAccountResults :: Prelude.Maybe Prelude.Int,
    -- | Lists if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountSet', 'getDatalakeStatus_accountSet' - The Amazon Web Services account ID for which a static snapshot of the
-- current Amazon Web Services Region, including enabled accounts and log
-- sources, is retrieved.
--
-- 'maxAccountResults', 'getDatalakeStatus_maxAccountResults' - The maximum limit of accounts for which the static snapshot of the
-- current Region, including enabled accounts and log sources, is
-- retrieved.
--
-- 'nextToken', 'getDatalakeStatus_nextToken' - Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
newGetDatalakeStatus ::
  GetDatalakeStatus
newGetDatalakeStatus =
  GetDatalakeStatus'
    { accountSet = Prelude.Nothing,
      maxAccountResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Web Services account ID for which a static snapshot of the
-- current Amazon Web Services Region, including enabled accounts and log
-- sources, is retrieved.
getDatalakeStatus_accountSet :: Lens.Lens' GetDatalakeStatus (Prelude.Maybe [Prelude.Text])
getDatalakeStatus_accountSet = Lens.lens (\GetDatalakeStatus' {accountSet} -> accountSet) (\s@GetDatalakeStatus' {} a -> s {accountSet = a} :: GetDatalakeStatus) Prelude.. Lens.mapping Lens.coerced

-- | The maximum limit of accounts for which the static snapshot of the
-- current Region, including enabled accounts and log sources, is
-- retrieved.
getDatalakeStatus_maxAccountResults :: Lens.Lens' GetDatalakeStatus (Prelude.Maybe Prelude.Int)
getDatalakeStatus_maxAccountResults = Lens.lens (\GetDatalakeStatus' {maxAccountResults} -> maxAccountResults) (\s@GetDatalakeStatus' {} a -> s {maxAccountResults = a} :: GetDatalakeStatus)

-- | Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
getDatalakeStatus_nextToken :: Lens.Lens' GetDatalakeStatus (Prelude.Maybe Prelude.Text)
getDatalakeStatus_nextToken = Lens.lens (\GetDatalakeStatus' {nextToken} -> nextToken) (\s@GetDatalakeStatus' {} a -> s {nextToken = a} :: GetDatalakeStatus)

instance Core.AWSPager GetDatalakeStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDatalakeStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. getDatalakeStatusResponse_accountSourcesList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getDatalakeStatus_nextToken
          Lens..~ rs
          Lens.^? getDatalakeStatusResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetDatalakeStatus where
  type
    AWSResponse GetDatalakeStatus =
      GetDatalakeStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDatalakeStatusResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "accountSourcesList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetDatalakeStatus where
  hashWithSalt _salt GetDatalakeStatus' {..} =
    _salt
      `Prelude.hashWithSalt` accountSet
      `Prelude.hashWithSalt` maxAccountResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDatalakeStatus where
  rnf GetDatalakeStatus' {..} =
    Prelude.rnf accountSet
      `Prelude.seq` Prelude.rnf maxAccountResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetDatalakeStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDatalakeStatus where
  toJSON GetDatalakeStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accountSet" Data..=) Prelude.<$> accountSet,
            ("maxAccountResults" Data..=)
              Prelude.<$> maxAccountResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetDatalakeStatus where
  toPath = Prelude.const "/v1/datalake/status"

instance Data.ToQuery GetDatalakeStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDatalakeStatusResponse' smart constructor.
data GetDatalakeStatusResponse = GetDatalakeStatusResponse'
  { -- | Lists if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of enabled accounts and enabled sources.
    accountSourcesList :: [AccountSources]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDatalakeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDatalakeStatusResponse_nextToken' - Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'httpStatus', 'getDatalakeStatusResponse_httpStatus' - The response's http status code.
--
-- 'accountSourcesList', 'getDatalakeStatusResponse_accountSourcesList' - The list of enabled accounts and enabled sources.
newGetDatalakeStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDatalakeStatusResponse
newGetDatalakeStatusResponse pHttpStatus_ =
  GetDatalakeStatusResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accountSourcesList = Prelude.mempty
    }

-- | Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
getDatalakeStatusResponse_nextToken :: Lens.Lens' GetDatalakeStatusResponse (Prelude.Maybe Prelude.Text)
getDatalakeStatusResponse_nextToken = Lens.lens (\GetDatalakeStatusResponse' {nextToken} -> nextToken) (\s@GetDatalakeStatusResponse' {} a -> s {nextToken = a} :: GetDatalakeStatusResponse)

-- | The response's http status code.
getDatalakeStatusResponse_httpStatus :: Lens.Lens' GetDatalakeStatusResponse Prelude.Int
getDatalakeStatusResponse_httpStatus = Lens.lens (\GetDatalakeStatusResponse' {httpStatus} -> httpStatus) (\s@GetDatalakeStatusResponse' {} a -> s {httpStatus = a} :: GetDatalakeStatusResponse)

-- | The list of enabled accounts and enabled sources.
getDatalakeStatusResponse_accountSourcesList :: Lens.Lens' GetDatalakeStatusResponse [AccountSources]
getDatalakeStatusResponse_accountSourcesList = Lens.lens (\GetDatalakeStatusResponse' {accountSourcesList} -> accountSourcesList) (\s@GetDatalakeStatusResponse' {} a -> s {accountSourcesList = a} :: GetDatalakeStatusResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDatalakeStatusResponse where
  rnf GetDatalakeStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountSourcesList
