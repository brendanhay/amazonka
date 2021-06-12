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
-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.ListBuckets
  ( -- * Creating a Request
    ListBuckets (..),
    newListBuckets,

    -- * Destructuring the Response
    ListBucketsResponse (..),
    newListBucketsResponse,

    -- * Response Lenses
    listBucketsResponse_buckets,
    listBucketsResponse_owner,
    listBucketsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newListBuckets' smart constructor.
data ListBuckets = ListBuckets'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBuckets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListBuckets ::
  ListBuckets
newListBuckets = ListBuckets'

instance Core.AWSRequest ListBuckets where
  type AWSResponse ListBuckets = ListBucketsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListBucketsResponse'
            Core.<$> ( x Core..@? "Buckets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Bucket")
                     )
            Core.<*> (x Core..@? "Owner")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBuckets

instance Core.NFData ListBuckets

instance Core.ToHeaders ListBuckets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListBuckets where
  toPath = Core.const "/"

instance Core.ToQuery ListBuckets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListBucketsResponse' smart constructor.
data ListBucketsResponse = ListBucketsResponse'
  { -- | The list of buckets owned by the requestor.
    buckets :: Core.Maybe [Bucket],
    -- | The owner of the buckets listed.
    owner :: Core.Maybe Owner,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListBucketsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buckets', 'listBucketsResponse_buckets' - The list of buckets owned by the requestor.
--
-- 'owner', 'listBucketsResponse_owner' - The owner of the buckets listed.
--
-- 'httpStatus', 'listBucketsResponse_httpStatus' - The response's http status code.
newListBucketsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListBucketsResponse
newListBucketsResponse pHttpStatus_ =
  ListBucketsResponse'
    { buckets = Core.Nothing,
      owner = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of buckets owned by the requestor.
listBucketsResponse_buckets :: Lens.Lens' ListBucketsResponse (Core.Maybe [Bucket])
listBucketsResponse_buckets = Lens.lens (\ListBucketsResponse' {buckets} -> buckets) (\s@ListBucketsResponse' {} a -> s {buckets = a} :: ListBucketsResponse) Core.. Lens.mapping Lens._Coerce

-- | The owner of the buckets listed.
listBucketsResponse_owner :: Lens.Lens' ListBucketsResponse (Core.Maybe Owner)
listBucketsResponse_owner = Lens.lens (\ListBucketsResponse' {owner} -> owner) (\s@ListBucketsResponse' {} a -> s {owner = a} :: ListBucketsResponse)

-- | The response's http status code.
listBucketsResponse_httpStatus :: Lens.Lens' ListBucketsResponse Core.Int
listBucketsResponse_httpStatus = Lens.lens (\ListBucketsResponse' {httpStatus} -> httpStatus) (\s@ListBucketsResponse' {} a -> s {httpStatus = a} :: ListBucketsResponse)

instance Core.NFData ListBucketsResponse
