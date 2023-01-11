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
-- Module      : Amazonka.AppStream.DescribeAppBlocks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more app blocks.
module Amazonka.AppStream.DescribeAppBlocks
  ( -- * Creating a Request
    DescribeAppBlocks (..),
    newDescribeAppBlocks,

    -- * Request Lenses
    describeAppBlocks_arns,
    describeAppBlocks_maxResults,
    describeAppBlocks_nextToken,

    -- * Destructuring the Response
    DescribeAppBlocksResponse (..),
    newDescribeAppBlocksResponse,

    -- * Response Lenses
    describeAppBlocksResponse_appBlocks,
    describeAppBlocksResponse_nextToken,
    describeAppBlocksResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppBlocks' smart constructor.
data DescribeAppBlocks = DescribeAppBlocks'
  { -- | The ARNs of the app blocks.
    arns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppBlocks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arns', 'describeAppBlocks_arns' - The ARNs of the app blocks.
--
-- 'maxResults', 'describeAppBlocks_maxResults' - The maximum size of each page of results.
--
-- 'nextToken', 'describeAppBlocks_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
newDescribeAppBlocks ::
  DescribeAppBlocks
newDescribeAppBlocks =
  DescribeAppBlocks'
    { arns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARNs of the app blocks.
describeAppBlocks_arns :: Lens.Lens' DescribeAppBlocks (Prelude.Maybe [Prelude.Text])
describeAppBlocks_arns = Lens.lens (\DescribeAppBlocks' {arns} -> arns) (\s@DescribeAppBlocks' {} a -> s {arns = a} :: DescribeAppBlocks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum size of each page of results.
describeAppBlocks_maxResults :: Lens.Lens' DescribeAppBlocks (Prelude.Maybe Prelude.Int)
describeAppBlocks_maxResults = Lens.lens (\DescribeAppBlocks' {maxResults} -> maxResults) (\s@DescribeAppBlocks' {} a -> s {maxResults = a} :: DescribeAppBlocks)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeAppBlocks_nextToken :: Lens.Lens' DescribeAppBlocks (Prelude.Maybe Prelude.Text)
describeAppBlocks_nextToken = Lens.lens (\DescribeAppBlocks' {nextToken} -> nextToken) (\s@DescribeAppBlocks' {} a -> s {nextToken = a} :: DescribeAppBlocks)

instance Core.AWSRequest DescribeAppBlocks where
  type
    AWSResponse DescribeAppBlocks =
      DescribeAppBlocksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppBlocksResponse'
            Prelude.<$> (x Data..?> "AppBlocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAppBlocks where
  hashWithSalt _salt DescribeAppBlocks' {..} =
    _salt `Prelude.hashWithSalt` arns
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeAppBlocks where
  rnf DescribeAppBlocks' {..} =
    Prelude.rnf arns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeAppBlocks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DescribeAppBlocks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeAppBlocks where
  toJSON DescribeAppBlocks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arns" Data..=) Prelude.<$> arns,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeAppBlocks where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAppBlocks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppBlocksResponse' smart constructor.
data DescribeAppBlocksResponse = DescribeAppBlocksResponse'
  { -- | The app blocks in the list.
    appBlocks :: Prelude.Maybe [AppBlock],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppBlocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBlocks', 'describeAppBlocksResponse_appBlocks' - The app blocks in the list.
--
-- 'nextToken', 'describeAppBlocksResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'describeAppBlocksResponse_httpStatus' - The response's http status code.
newDescribeAppBlocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppBlocksResponse
newDescribeAppBlocksResponse pHttpStatus_ =
  DescribeAppBlocksResponse'
    { appBlocks =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The app blocks in the list.
describeAppBlocksResponse_appBlocks :: Lens.Lens' DescribeAppBlocksResponse (Prelude.Maybe [AppBlock])
describeAppBlocksResponse_appBlocks = Lens.lens (\DescribeAppBlocksResponse' {appBlocks} -> appBlocks) (\s@DescribeAppBlocksResponse' {} a -> s {appBlocks = a} :: DescribeAppBlocksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
describeAppBlocksResponse_nextToken :: Lens.Lens' DescribeAppBlocksResponse (Prelude.Maybe Prelude.Text)
describeAppBlocksResponse_nextToken = Lens.lens (\DescribeAppBlocksResponse' {nextToken} -> nextToken) (\s@DescribeAppBlocksResponse' {} a -> s {nextToken = a} :: DescribeAppBlocksResponse)

-- | The response's http status code.
describeAppBlocksResponse_httpStatus :: Lens.Lens' DescribeAppBlocksResponse Prelude.Int
describeAppBlocksResponse_httpStatus = Lens.lens (\DescribeAppBlocksResponse' {httpStatus} -> httpStatus) (\s@DescribeAppBlocksResponse' {} a -> s {httpStatus = a} :: DescribeAppBlocksResponse)

instance Prelude.NFData DescribeAppBlocksResponse where
  rnf DescribeAppBlocksResponse' {..} =
    Prelude.rnf appBlocks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
