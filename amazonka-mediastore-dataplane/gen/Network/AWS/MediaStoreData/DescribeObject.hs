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
-- Module      : Network.AWS.MediaStoreData.DescribeObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the headers for an object at the specified path.
module Network.AWS.MediaStoreData.DescribeObject
  ( -- * Creating a Request
    DescribeObject (..),
    newDescribeObject,

    -- * Request Lenses
    describeObject_path,

    -- * Destructuring the Response
    DescribeObjectResponse (..),
    newDescribeObjectResponse,

    -- * Response Lenses
    describeObjectResponse_eTag,
    describeObjectResponse_contentType,
    describeObjectResponse_contentLength,
    describeObjectResponse_lastModified,
    describeObjectResponse_cacheControl,
    describeObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeObject' smart constructor.
data DescribeObject = DescribeObject'
  { -- | The path (including the file name) where the object is stored in the
    -- container. Format: \<folder name>\/\<folder name>\/\<file name>
    path :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'describeObject_path' - The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
newDescribeObject ::
  -- | 'path'
  Core.Text ->
  DescribeObject
newDescribeObject pPath_ =
  DescribeObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
describeObject_path :: Lens.Lens' DescribeObject Core.Text
describeObject_path = Lens.lens (\DescribeObject' {path} -> path) (\s@DescribeObject' {} a -> s {path = a} :: DescribeObject)

instance Core.AWSRequest DescribeObject where
  type
    AWSResponse DescribeObject =
      DescribeObjectResponse
  request = Request.head' defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DescribeObjectResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (h Core..#? "Content-Type")
            Core.<*> (h Core..#? "Content-Length")
            Core.<*> (h Core..#? "Last-Modified")
            Core.<*> (h Core..#? "Cache-Control")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeObject

instance Core.NFData DescribeObject

instance Core.ToHeaders DescribeObject where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeObject where
  toPath DescribeObject' {..} =
    Core.mconcat ["/", Core.toBS path]

instance Core.ToQuery DescribeObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { -- | The ETag that represents a unique instance of the object.
    eTag :: Core.Maybe Core.Text,
    -- | The content type of the object.
    contentType :: Core.Maybe Core.Text,
    -- | The length of the object in bytes.
    contentLength :: Core.Maybe Core.Natural,
    -- | The date and time that the object was last modified.
    lastModified :: Core.Maybe Core.POSIX,
    -- | An optional @CacheControl@ header that allows the caller to control the
    -- object\'s cache behavior. Headers can be passed in as specified in the
    -- HTTP at
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'describeObjectResponse_eTag' - The ETag that represents a unique instance of the object.
--
-- 'contentType', 'describeObjectResponse_contentType' - The content type of the object.
--
-- 'contentLength', 'describeObjectResponse_contentLength' - The length of the object in bytes.
--
-- 'lastModified', 'describeObjectResponse_lastModified' - The date and time that the object was last modified.
--
-- 'cacheControl', 'describeObjectResponse_cacheControl' - An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
--
-- 'httpStatus', 'describeObjectResponse_httpStatus' - The response's http status code.
newDescribeObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeObjectResponse
newDescribeObjectResponse pHttpStatus_ =
  DescribeObjectResponse'
    { eTag = Core.Nothing,
      contentType = Core.Nothing,
      contentLength = Core.Nothing,
      lastModified = Core.Nothing,
      cacheControl = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ETag that represents a unique instance of the object.
describeObjectResponse_eTag :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Text)
describeObjectResponse_eTag = Lens.lens (\DescribeObjectResponse' {eTag} -> eTag) (\s@DescribeObjectResponse' {} a -> s {eTag = a} :: DescribeObjectResponse)

-- | The content type of the object.
describeObjectResponse_contentType :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Text)
describeObjectResponse_contentType = Lens.lens (\DescribeObjectResponse' {contentType} -> contentType) (\s@DescribeObjectResponse' {} a -> s {contentType = a} :: DescribeObjectResponse)

-- | The length of the object in bytes.
describeObjectResponse_contentLength :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Natural)
describeObjectResponse_contentLength = Lens.lens (\DescribeObjectResponse' {contentLength} -> contentLength) (\s@DescribeObjectResponse' {} a -> s {contentLength = a} :: DescribeObjectResponse)

-- | The date and time that the object was last modified.
describeObjectResponse_lastModified :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.UTCTime)
describeObjectResponse_lastModified = Lens.lens (\DescribeObjectResponse' {lastModified} -> lastModified) (\s@DescribeObjectResponse' {} a -> s {lastModified = a} :: DescribeObjectResponse) Core.. Lens.mapping Core._Time

-- | An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
describeObjectResponse_cacheControl :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Text)
describeObjectResponse_cacheControl = Lens.lens (\DescribeObjectResponse' {cacheControl} -> cacheControl) (\s@DescribeObjectResponse' {} a -> s {cacheControl = a} :: DescribeObjectResponse)

-- | The response's http status code.
describeObjectResponse_httpStatus :: Lens.Lens' DescribeObjectResponse Core.Int
describeObjectResponse_httpStatus = Lens.lens (\DescribeObjectResponse' {httpStatus} -> httpStatus) (\s@DescribeObjectResponse' {} a -> s {httpStatus = a} :: DescribeObjectResponse)

instance Core.NFData DescribeObjectResponse
