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
-- Module      : Amazonka.AppStream.DescribeDirectoryConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Directory Config
-- objects for AppStream 2.0, if the names for these objects are provided.
-- Otherwise, all Directory Config objects in the account are described.
-- These objects include the configuration information required to join
-- fleets and image builders to Microsoft Active Directory domains.
--
-- Although the response syntax in this topic includes the account
-- password, this password is not returned in the actual response.
--
-- This operation returns paginated results.
module Amazonka.AppStream.DescribeDirectoryConfigs
  ( -- * Creating a Request
    DescribeDirectoryConfigs (..),
    newDescribeDirectoryConfigs,

    -- * Request Lenses
    describeDirectoryConfigs_nextToken,
    describeDirectoryConfigs_directoryNames,
    describeDirectoryConfigs_maxResults,

    -- * Destructuring the Response
    DescribeDirectoryConfigsResponse (..),
    newDescribeDirectoryConfigsResponse,

    -- * Response Lenses
    describeDirectoryConfigsResponse_nextToken,
    describeDirectoryConfigsResponse_directoryConfigs,
    describeDirectoryConfigsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDirectoryConfigs' smart constructor.
data DescribeDirectoryConfigs = DescribeDirectoryConfigs'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The directory names.
    directoryNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectoryConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectoryConfigs_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'directoryNames', 'describeDirectoryConfigs_directoryNames' - The directory names.
--
-- 'maxResults', 'describeDirectoryConfigs_maxResults' - The maximum size of each page of results.
newDescribeDirectoryConfigs ::
  DescribeDirectoryConfigs
newDescribeDirectoryConfigs =
  DescribeDirectoryConfigs'
    { nextToken =
        Prelude.Nothing,
      directoryNames = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeDirectoryConfigs_nextToken :: Lens.Lens' DescribeDirectoryConfigs (Prelude.Maybe Prelude.Text)
describeDirectoryConfigs_nextToken = Lens.lens (\DescribeDirectoryConfigs' {nextToken} -> nextToken) (\s@DescribeDirectoryConfigs' {} a -> s {nextToken = a} :: DescribeDirectoryConfigs)

-- | The directory names.
describeDirectoryConfigs_directoryNames :: Lens.Lens' DescribeDirectoryConfigs (Prelude.Maybe [Prelude.Text])
describeDirectoryConfigs_directoryNames = Lens.lens (\DescribeDirectoryConfigs' {directoryNames} -> directoryNames) (\s@DescribeDirectoryConfigs' {} a -> s {directoryNames = a} :: DescribeDirectoryConfigs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of each page of results.
describeDirectoryConfigs_maxResults :: Lens.Lens' DescribeDirectoryConfigs (Prelude.Maybe Prelude.Int)
describeDirectoryConfigs_maxResults = Lens.lens (\DescribeDirectoryConfigs' {maxResults} -> maxResults) (\s@DescribeDirectoryConfigs' {} a -> s {maxResults = a} :: DescribeDirectoryConfigs)

instance Core.AWSPager DescribeDirectoryConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectoryConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectoryConfigsResponse_directoryConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDirectoryConfigs_nextToken
          Lens..~ rs
          Lens.^? describeDirectoryConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDirectoryConfigs where
  type
    AWSResponse DescribeDirectoryConfigs =
      DescribeDirectoryConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectoryConfigsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "DirectoryConfigs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDirectoryConfigs where
  hashWithSalt _salt DescribeDirectoryConfigs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryNames
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeDirectoryConfigs where
  rnf DescribeDirectoryConfigs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryNames
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeDirectoryConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeDirectoryConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDirectoryConfigs where
  toJSON DescribeDirectoryConfigs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("DirectoryNames" Data..=)
              Prelude.<$> directoryNames,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeDirectoryConfigs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDirectoryConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectoryConfigsResponse' smart constructor.
data DescribeDirectoryConfigsResponse = DescribeDirectoryConfigsResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the directory configurations. Note that although the
    -- response syntax in this topic includes the account password, this
    -- password is not returned in the actual response.
    directoryConfigs :: Prelude.Maybe [DirectoryConfig],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectoryConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectoryConfigsResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'directoryConfigs', 'describeDirectoryConfigsResponse_directoryConfigs' - Information about the directory configurations. Note that although the
-- response syntax in this topic includes the account password, this
-- password is not returned in the actual response.
--
-- 'httpStatus', 'describeDirectoryConfigsResponse_httpStatus' - The response's http status code.
newDescribeDirectoryConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectoryConfigsResponse
newDescribeDirectoryConfigsResponse pHttpStatus_ =
  DescribeDirectoryConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      directoryConfigs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeDirectoryConfigsResponse_nextToken :: Lens.Lens' DescribeDirectoryConfigsResponse (Prelude.Maybe Prelude.Text)
describeDirectoryConfigsResponse_nextToken = Lens.lens (\DescribeDirectoryConfigsResponse' {nextToken} -> nextToken) (\s@DescribeDirectoryConfigsResponse' {} a -> s {nextToken = a} :: DescribeDirectoryConfigsResponse)

-- | Information about the directory configurations. Note that although the
-- response syntax in this topic includes the account password, this
-- password is not returned in the actual response.
describeDirectoryConfigsResponse_directoryConfigs :: Lens.Lens' DescribeDirectoryConfigsResponse (Prelude.Maybe [DirectoryConfig])
describeDirectoryConfigsResponse_directoryConfigs = Lens.lens (\DescribeDirectoryConfigsResponse' {directoryConfigs} -> directoryConfigs) (\s@DescribeDirectoryConfigsResponse' {} a -> s {directoryConfigs = a} :: DescribeDirectoryConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDirectoryConfigsResponse_httpStatus :: Lens.Lens' DescribeDirectoryConfigsResponse Prelude.Int
describeDirectoryConfigsResponse_httpStatus = Lens.lens (\DescribeDirectoryConfigsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectoryConfigsResponse' {} a -> s {httpStatus = a} :: DescribeDirectoryConfigsResponse)

instance
  Prelude.NFData
    DescribeDirectoryConfigsResponse
  where
  rnf DescribeDirectoryConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryConfigs
      `Prelude.seq` Prelude.rnf httpStatus
