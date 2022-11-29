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
-- Module      : Amazonka.FSx.DescribeFileCaches
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of a specific Amazon File Cache resource, if a
-- @FileCacheIds@ value is provided for that cache. Otherwise, it returns
-- descriptions of all caches owned by your Amazon Web Services account in
-- the Amazon Web Services Region of the endpoint that you\'re calling.
--
-- When retrieving all cache descriptions, you can optionally specify the
-- @MaxResults@ parameter to limit the number of descriptions in a
-- response. If more cache descriptions remain, the operation returns a
-- @NextToken@ value in the response. In this case, send a later request
-- with the @NextToken@ request parameter set to the value of @NextToken@
-- from the last response.
--
-- This operation is used in an iterative process to retrieve a list of
-- your cache descriptions. @DescribeFileCaches@ is called first without a
-- @NextToken@value. Then the operation continues to be called with the
-- @NextToken@ parameter set to the value of the last @NextToken@ value
-- until a response has no @NextToken@.
--
-- When using this operation, keep the following in mind:
--
-- -   The implementation might return fewer than @MaxResults@ cache
--     descriptions while still including a @NextToken@ value.
--
-- -   The order of caches returned in the response of one
--     @DescribeFileCaches@ call and the order of caches returned across
--     the responses of a multicall iteration is unspecified.
module Amazonka.FSx.DescribeFileCaches
  ( -- * Creating a Request
    DescribeFileCaches (..),
    newDescribeFileCaches,

    -- * Request Lenses
    describeFileCaches_nextToken,
    describeFileCaches_fileCacheIds,
    describeFileCaches_maxResults,

    -- * Destructuring the Response
    DescribeFileCachesResponse (..),
    newDescribeFileCachesResponse,

    -- * Response Lenses
    describeFileCachesResponse_nextToken,
    describeFileCachesResponse_fileCaches,
    describeFileCachesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFileCaches' smart constructor.
data DescribeFileCaches = DescribeFileCaches'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | IDs of the caches whose descriptions you want to retrieve (String).
    fileCacheIds :: Prelude.Maybe [Prelude.Text],
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileCaches' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFileCaches_nextToken' - Undocumented member.
--
-- 'fileCacheIds', 'describeFileCaches_fileCacheIds' - IDs of the caches whose descriptions you want to retrieve (String).
--
-- 'maxResults', 'describeFileCaches_maxResults' - Undocumented member.
newDescribeFileCaches ::
  DescribeFileCaches
newDescribeFileCaches =
  DescribeFileCaches'
    { nextToken = Prelude.Nothing,
      fileCacheIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
describeFileCaches_nextToken :: Lens.Lens' DescribeFileCaches (Prelude.Maybe Prelude.Text)
describeFileCaches_nextToken = Lens.lens (\DescribeFileCaches' {nextToken} -> nextToken) (\s@DescribeFileCaches' {} a -> s {nextToken = a} :: DescribeFileCaches)

-- | IDs of the caches whose descriptions you want to retrieve (String).
describeFileCaches_fileCacheIds :: Lens.Lens' DescribeFileCaches (Prelude.Maybe [Prelude.Text])
describeFileCaches_fileCacheIds = Lens.lens (\DescribeFileCaches' {fileCacheIds} -> fileCacheIds) (\s@DescribeFileCaches' {} a -> s {fileCacheIds = a} :: DescribeFileCaches) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeFileCaches_maxResults :: Lens.Lens' DescribeFileCaches (Prelude.Maybe Prelude.Natural)
describeFileCaches_maxResults = Lens.lens (\DescribeFileCaches' {maxResults} -> maxResults) (\s@DescribeFileCaches' {} a -> s {maxResults = a} :: DescribeFileCaches)

instance Core.AWSRequest DescribeFileCaches where
  type
    AWSResponse DescribeFileCaches =
      DescribeFileCachesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFileCachesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "FileCaches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFileCaches where
  hashWithSalt _salt DescribeFileCaches' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fileCacheIds
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeFileCaches where
  rnf DescribeFileCaches' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fileCacheIds
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeFileCaches where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSSimbaAPIService_v20180301.DescribeFileCaches" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFileCaches where
  toJSON DescribeFileCaches' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("FileCacheIds" Core..=) Prelude.<$> fileCacheIds,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeFileCaches where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFileCaches where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFileCachesResponse' smart constructor.
data DescribeFileCachesResponse = DescribeFileCachesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response object for the @DescribeFileCaches@ operation.
    fileCaches :: Prelude.Maybe [FileCache],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFileCachesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFileCachesResponse_nextToken' - Undocumented member.
--
-- 'fileCaches', 'describeFileCachesResponse_fileCaches' - The response object for the @DescribeFileCaches@ operation.
--
-- 'httpStatus', 'describeFileCachesResponse_httpStatus' - The response's http status code.
newDescribeFileCachesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFileCachesResponse
newDescribeFileCachesResponse pHttpStatus_ =
  DescribeFileCachesResponse'
    { nextToken =
        Prelude.Nothing,
      fileCaches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeFileCachesResponse_nextToken :: Lens.Lens' DescribeFileCachesResponse (Prelude.Maybe Prelude.Text)
describeFileCachesResponse_nextToken = Lens.lens (\DescribeFileCachesResponse' {nextToken} -> nextToken) (\s@DescribeFileCachesResponse' {} a -> s {nextToken = a} :: DescribeFileCachesResponse)

-- | The response object for the @DescribeFileCaches@ operation.
describeFileCachesResponse_fileCaches :: Lens.Lens' DescribeFileCachesResponse (Prelude.Maybe [FileCache])
describeFileCachesResponse_fileCaches = Lens.lens (\DescribeFileCachesResponse' {fileCaches} -> fileCaches) (\s@DescribeFileCachesResponse' {} a -> s {fileCaches = a} :: DescribeFileCachesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFileCachesResponse_httpStatus :: Lens.Lens' DescribeFileCachesResponse Prelude.Int
describeFileCachesResponse_httpStatus = Lens.lens (\DescribeFileCachesResponse' {httpStatus} -> httpStatus) (\s@DescribeFileCachesResponse' {} a -> s {httpStatus = a} :: DescribeFileCachesResponse)

instance Prelude.NFData DescribeFileCachesResponse where
  rnf DescribeFileCachesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fileCaches
      `Prelude.seq` Prelude.rnf httpStatus
