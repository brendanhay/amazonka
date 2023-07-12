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
-- Module      : Amazonka.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns torrent files from a bucket. BitTorrent can save you bandwidth
-- when you\'re distributing large files. For more information about
-- BitTorrent, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3Torrent.html Using BitTorrent with Amazon S3>.
--
-- You can get torrent only for objects that are less than 5 GB in size,
-- and that are not encrypted using server-side encryption with a
-- customer-provided encryption key.
--
-- To use GET, you must have READ access to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following action is related to @GetObjectTorrent@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Amazonka.S3.GetObjectTorrent
  ( -- * Creating a Request
    GetObjectTorrent (..),
    newGetObjectTorrent,

    -- * Request Lenses
    getObjectTorrent_expectedBucketOwner,
    getObjectTorrent_requestPayer,
    getObjectTorrent_bucket,
    getObjectTorrent_key,

    -- * Destructuring the Response
    GetObjectTorrentResponse (..),
    newGetObjectTorrentResponse,

    -- * Response Lenses
    getObjectTorrentResponse_requestCharged,
    getObjectTorrentResponse_httpStatus,
    getObjectTorrentResponse_body,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectTorrent' smart constructor.
data GetObjectTorrent = GetObjectTorrent'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The name of the bucket containing the object for which to get the
    -- torrent files.
    bucket :: BucketName,
    -- | The object key for which to get the information.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectTorrent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectTorrent_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'getObjectTorrent_requestPayer' - Undocumented member.
--
-- 'bucket', 'getObjectTorrent_bucket' - The name of the bucket containing the object for which to get the
-- torrent files.
--
-- 'key', 'getObjectTorrent_key' - The object key for which to get the information.
newGetObjectTorrent ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectTorrent
newGetObjectTorrent pBucket_ pKey_ =
  GetObjectTorrent'
    { expectedBucketOwner =
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectTorrent_expectedBucketOwner :: Lens.Lens' GetObjectTorrent (Prelude.Maybe Prelude.Text)
getObjectTorrent_expectedBucketOwner = Lens.lens (\GetObjectTorrent' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectTorrent' {} a -> s {expectedBucketOwner = a} :: GetObjectTorrent)

-- | Undocumented member.
getObjectTorrent_requestPayer :: Lens.Lens' GetObjectTorrent (Prelude.Maybe RequestPayer)
getObjectTorrent_requestPayer = Lens.lens (\GetObjectTorrent' {requestPayer} -> requestPayer) (\s@GetObjectTorrent' {} a -> s {requestPayer = a} :: GetObjectTorrent)

-- | The name of the bucket containing the object for which to get the
-- torrent files.
getObjectTorrent_bucket :: Lens.Lens' GetObjectTorrent BucketName
getObjectTorrent_bucket = Lens.lens (\GetObjectTorrent' {bucket} -> bucket) (\s@GetObjectTorrent' {} a -> s {bucket = a} :: GetObjectTorrent)

-- | The object key for which to get the information.
getObjectTorrent_key :: Lens.Lens' GetObjectTorrent ObjectKey
getObjectTorrent_key = Lens.lens (\GetObjectTorrent' {key} -> key) (\s@GetObjectTorrent' {} a -> s {key = a} :: GetObjectTorrent)

instance Core.AWSRequest GetObjectTorrent where
  type
    AWSResponse GetObjectTorrent =
      GetObjectTorrentResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectTorrentResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetObjectTorrent where
  hashWithSalt _salt GetObjectTorrent' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObjectTorrent where
  rnf GetObjectTorrent' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetObjectTorrent where
  toHeaders GetObjectTorrent' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath GetObjectTorrent where
  toPath GetObjectTorrent' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObjectTorrent where
  toQuery = Prelude.const (Prelude.mconcat ["torrent"])

-- | /See:/ 'newGetObjectTorrentResponse' smart constructor.
data GetObjectTorrentResponse = GetObjectTorrentResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A Bencoded dictionary as defined by the BitTorrent specification
    body :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectTorrentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'getObjectTorrentResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'getObjectTorrentResponse_httpStatus' - The response's http status code.
--
-- 'body', 'getObjectTorrentResponse_body' - A Bencoded dictionary as defined by the BitTorrent specification
newGetObjectTorrentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'body'
  Data.ResponseBody ->
  GetObjectTorrentResponse
newGetObjectTorrentResponse pHttpStatus_ pBody_ =
  GetObjectTorrentResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      body = pBody_
    }

-- | Undocumented member.
getObjectTorrentResponse_requestCharged :: Lens.Lens' GetObjectTorrentResponse (Prelude.Maybe RequestCharged)
getObjectTorrentResponse_requestCharged = Lens.lens (\GetObjectTorrentResponse' {requestCharged} -> requestCharged) (\s@GetObjectTorrentResponse' {} a -> s {requestCharged = a} :: GetObjectTorrentResponse)

-- | The response's http status code.
getObjectTorrentResponse_httpStatus :: Lens.Lens' GetObjectTorrentResponse Prelude.Int
getObjectTorrentResponse_httpStatus = Lens.lens (\GetObjectTorrentResponse' {httpStatus} -> httpStatus) (\s@GetObjectTorrentResponse' {} a -> s {httpStatus = a} :: GetObjectTorrentResponse)

-- | A Bencoded dictionary as defined by the BitTorrent specification
getObjectTorrentResponse_body :: Lens.Lens' GetObjectTorrentResponse Data.ResponseBody
getObjectTorrentResponse_body = Lens.lens (\GetObjectTorrentResponse' {body} -> body) (\s@GetObjectTorrentResponse' {} a -> s {body = a} :: GetObjectTorrentResponse)
