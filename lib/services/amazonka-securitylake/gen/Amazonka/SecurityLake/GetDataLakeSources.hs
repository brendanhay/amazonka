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
-- Module      : Amazonka.SecurityLake.GetDataLakeSources
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
module Amazonka.SecurityLake.GetDataLakeSources
  ( -- * Creating a Request
    GetDataLakeSources (..),
    newGetDataLakeSources,

    -- * Request Lenses
    getDataLakeSources_accounts,
    getDataLakeSources_maxResults,
    getDataLakeSources_nextToken,

    -- * Destructuring the Response
    GetDataLakeSourcesResponse (..),
    newGetDataLakeSourcesResponse,

    -- * Response Lenses
    getDataLakeSourcesResponse_dataLakeArn,
    getDataLakeSourcesResponse_dataLakeSources,
    getDataLakeSourcesResponse_nextToken,
    getDataLakeSourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newGetDataLakeSources' smart constructor.
data GetDataLakeSources = GetDataLakeSources'
  { -- | The Amazon Web Services account ID for which a static snapshot of the
    -- current Amazon Web Services Region, including enabled accounts and log
    -- sources, is retrieved.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | The maximum limit of accounts for which the static snapshot of the
    -- current Region, including enabled accounts and log sources, is
    -- retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- Create a value of 'GetDataLakeSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'getDataLakeSources_accounts' - The Amazon Web Services account ID for which a static snapshot of the
-- current Amazon Web Services Region, including enabled accounts and log
-- sources, is retrieved.
--
-- 'maxResults', 'getDataLakeSources_maxResults' - The maximum limit of accounts for which the static snapshot of the
-- current Region, including enabled accounts and log sources, is
-- retrieved.
--
-- 'nextToken', 'getDataLakeSources_nextToken' - Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
newGetDataLakeSources ::
  GetDataLakeSources
newGetDataLakeSources =
  GetDataLakeSources'
    { accounts = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Web Services account ID for which a static snapshot of the
-- current Amazon Web Services Region, including enabled accounts and log
-- sources, is retrieved.
getDataLakeSources_accounts :: Lens.Lens' GetDataLakeSources (Prelude.Maybe [Prelude.Text])
getDataLakeSources_accounts = Lens.lens (\GetDataLakeSources' {accounts} -> accounts) (\s@GetDataLakeSources' {} a -> s {accounts = a} :: GetDataLakeSources) Prelude.. Lens.mapping Lens.coerced

-- | The maximum limit of accounts for which the static snapshot of the
-- current Region, including enabled accounts and log sources, is
-- retrieved.
getDataLakeSources_maxResults :: Lens.Lens' GetDataLakeSources (Prelude.Maybe Prelude.Natural)
getDataLakeSources_maxResults = Lens.lens (\GetDataLakeSources' {maxResults} -> maxResults) (\s@GetDataLakeSources' {} a -> s {maxResults = a} :: GetDataLakeSources)

-- | Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
getDataLakeSources_nextToken :: Lens.Lens' GetDataLakeSources (Prelude.Maybe Prelude.Text)
getDataLakeSources_nextToken = Lens.lens (\GetDataLakeSources' {nextToken} -> nextToken) (\s@GetDataLakeSources' {} a -> s {nextToken = a} :: GetDataLakeSources)

instance Core.AWSPager GetDataLakeSources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDataLakeSourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDataLakeSourcesResponse_dataLakeSources
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getDataLakeSources_nextToken
          Lens..~ rs
          Lens.^? getDataLakeSourcesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetDataLakeSources where
  type
    AWSResponse GetDataLakeSources =
      GetDataLakeSourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataLakeSourcesResponse'
            Prelude.<$> (x Data..?> "dataLakeArn")
            Prelude.<*> ( x
                            Data..?> "dataLakeSources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataLakeSources where
  hashWithSalt _salt GetDataLakeSources' {..} =
    _salt
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDataLakeSources where
  rnf GetDataLakeSources' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetDataLakeSources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataLakeSources where
  toJSON GetDataLakeSources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accounts" Data..=) Prelude.<$> accounts,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetDataLakeSources where
  toPath = Prelude.const "/v1/datalake/sources"

instance Data.ToQuery GetDataLakeSources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataLakeSourcesResponse' smart constructor.
data GetDataLakeSourcesResponse = GetDataLakeSourcesResponse'
  { -- | The Amazon Resource Name (ARN) created by you to provide to the
    -- subscriber. For more information about ARNs and how to use them in
    -- policies, see the
    -- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
    dataLakeArn :: Prelude.Maybe Prelude.Text,
    -- | The list of enabled accounts and enabled sources.
    dataLakeSources :: Prelude.Maybe [DataLakeSource],
    -- | Lists if there are more results available. The value of nextToken is a
    -- unique pagination token for each page. Repeat the call using the
    -- returned token to retrieve the next page. Keep all other arguments
    -- unchanged.
    --
    -- Each pagination token expires after 24 hours. Using an expired
    -- pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataLakeSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakeArn', 'getDataLakeSourcesResponse_dataLakeArn' - The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
--
-- 'dataLakeSources', 'getDataLakeSourcesResponse_dataLakeSources' - The list of enabled accounts and enabled sources.
--
-- 'nextToken', 'getDataLakeSourcesResponse_nextToken' - Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
--
-- 'httpStatus', 'getDataLakeSourcesResponse_httpStatus' - The response's http status code.
newGetDataLakeSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataLakeSourcesResponse
newGetDataLakeSourcesResponse pHttpStatus_ =
  GetDataLakeSourcesResponse'
    { dataLakeArn =
        Prelude.Nothing,
      dataLakeSources = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) created by you to provide to the
-- subscriber. For more information about ARNs and how to use them in
-- policies, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/subscriber-management.html Amazon Security Lake User Guide>.
getDataLakeSourcesResponse_dataLakeArn :: Lens.Lens' GetDataLakeSourcesResponse (Prelude.Maybe Prelude.Text)
getDataLakeSourcesResponse_dataLakeArn = Lens.lens (\GetDataLakeSourcesResponse' {dataLakeArn} -> dataLakeArn) (\s@GetDataLakeSourcesResponse' {} a -> s {dataLakeArn = a} :: GetDataLakeSourcesResponse)

-- | The list of enabled accounts and enabled sources.
getDataLakeSourcesResponse_dataLakeSources :: Lens.Lens' GetDataLakeSourcesResponse (Prelude.Maybe [DataLakeSource])
getDataLakeSourcesResponse_dataLakeSources = Lens.lens (\GetDataLakeSourcesResponse' {dataLakeSources} -> dataLakeSources) (\s@GetDataLakeSourcesResponse' {} a -> s {dataLakeSources = a} :: GetDataLakeSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Lists if there are more results available. The value of nextToken is a
-- unique pagination token for each page. Repeat the call using the
-- returned token to retrieve the next page. Keep all other arguments
-- unchanged.
--
-- Each pagination token expires after 24 hours. Using an expired
-- pagination token will return an HTTP 400 InvalidToken error.
getDataLakeSourcesResponse_nextToken :: Lens.Lens' GetDataLakeSourcesResponse (Prelude.Maybe Prelude.Text)
getDataLakeSourcesResponse_nextToken = Lens.lens (\GetDataLakeSourcesResponse' {nextToken} -> nextToken) (\s@GetDataLakeSourcesResponse' {} a -> s {nextToken = a} :: GetDataLakeSourcesResponse)

-- | The response's http status code.
getDataLakeSourcesResponse_httpStatus :: Lens.Lens' GetDataLakeSourcesResponse Prelude.Int
getDataLakeSourcesResponse_httpStatus = Lens.lens (\GetDataLakeSourcesResponse' {httpStatus} -> httpStatus) (\s@GetDataLakeSourcesResponse' {} a -> s {httpStatus = a} :: GetDataLakeSourcesResponse)

instance Prelude.NFData GetDataLakeSourcesResponse where
  rnf GetDataLakeSourcesResponse' {..} =
    Prelude.rnf dataLakeArn
      `Prelude.seq` Prelude.rnf dataLakeSources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
