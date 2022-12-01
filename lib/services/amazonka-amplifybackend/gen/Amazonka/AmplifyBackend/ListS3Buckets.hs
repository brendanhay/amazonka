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
-- Module      : Amazonka.AmplifyBackend.ListS3Buckets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The list of S3 buckets in your account.
module Amazonka.AmplifyBackend.ListS3Buckets
  ( -- * Creating a Request
    ListS3Buckets (..),
    newListS3Buckets,

    -- * Request Lenses
    listS3Buckets_nextToken,

    -- * Destructuring the Response
    ListS3BucketsResponse (..),
    newListS3BucketsResponse,

    -- * Response Lenses
    listS3BucketsResponse_nextToken,
    listS3BucketsResponse_buckets,
    listS3BucketsResponse_httpStatus,
  )
where

import Amazonka.AmplifyBackend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for S3Buckets.
--
-- /See:/ 'newListS3Buckets' smart constructor.
data ListS3Buckets = ListS3Buckets'
  { -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListS3Buckets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listS3Buckets_nextToken' - Reserved for future use.
newListS3Buckets ::
  ListS3Buckets
newListS3Buckets =
  ListS3Buckets' {nextToken = Prelude.Nothing}

-- | Reserved for future use.
listS3Buckets_nextToken :: Lens.Lens' ListS3Buckets (Prelude.Maybe Prelude.Text)
listS3Buckets_nextToken = Lens.lens (\ListS3Buckets' {nextToken} -> nextToken) (\s@ListS3Buckets' {} a -> s {nextToken = a} :: ListS3Buckets)

instance Core.AWSRequest ListS3Buckets where
  type
    AWSResponse ListS3Buckets =
      ListS3BucketsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListS3BucketsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "buckets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListS3Buckets where
  hashWithSalt _salt ListS3Buckets' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListS3Buckets where
  rnf ListS3Buckets' {..} = Prelude.rnf nextToken

instance Core.ToHeaders ListS3Buckets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListS3Buckets where
  toJSON ListS3Buckets' {..} =
    Core.object
      ( Prelude.catMaybes
          [("nextToken" Core..=) Prelude.<$> nextToken]
      )

instance Core.ToPath ListS3Buckets where
  toPath = Prelude.const "/s3Buckets"

instance Core.ToQuery ListS3Buckets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListS3BucketsResponse' smart constructor.
data ListS3BucketsResponse = ListS3BucketsResponse'
  { -- | Reserved for future use.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of S3 buckets.
    buckets :: Prelude.Maybe [S3BucketInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListS3BucketsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listS3BucketsResponse_nextToken' - Reserved for future use.
--
-- 'buckets', 'listS3BucketsResponse_buckets' - The list of S3 buckets.
--
-- 'httpStatus', 'listS3BucketsResponse_httpStatus' - The response's http status code.
newListS3BucketsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListS3BucketsResponse
newListS3BucketsResponse pHttpStatus_ =
  ListS3BucketsResponse'
    { nextToken = Prelude.Nothing,
      buckets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reserved for future use.
listS3BucketsResponse_nextToken :: Lens.Lens' ListS3BucketsResponse (Prelude.Maybe Prelude.Text)
listS3BucketsResponse_nextToken = Lens.lens (\ListS3BucketsResponse' {nextToken} -> nextToken) (\s@ListS3BucketsResponse' {} a -> s {nextToken = a} :: ListS3BucketsResponse)

-- | The list of S3 buckets.
listS3BucketsResponse_buckets :: Lens.Lens' ListS3BucketsResponse (Prelude.Maybe [S3BucketInfo])
listS3BucketsResponse_buckets = Lens.lens (\ListS3BucketsResponse' {buckets} -> buckets) (\s@ListS3BucketsResponse' {} a -> s {buckets = a} :: ListS3BucketsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listS3BucketsResponse_httpStatus :: Lens.Lens' ListS3BucketsResponse Prelude.Int
listS3BucketsResponse_httpStatus = Lens.lens (\ListS3BucketsResponse' {httpStatus} -> httpStatus) (\s@ListS3BucketsResponse' {} a -> s {httpStatus = a} :: ListS3BucketsResponse)

instance Prelude.NFData ListS3BucketsResponse where
  rnf ListS3BucketsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf buckets
      `Prelude.seq` Prelude.rnf httpStatus
