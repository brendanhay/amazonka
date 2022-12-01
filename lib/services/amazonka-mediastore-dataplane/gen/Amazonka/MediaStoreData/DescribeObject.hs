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
-- Module      : Amazonka.MediaStoreData.DescribeObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the headers for an object at the specified path.
module Amazonka.MediaStoreData.DescribeObject
  ( -- * Creating a Request
    DescribeObject (..),
    newDescribeObject,

    -- * Request Lenses
    describeObject_path,

    -- * Destructuring the Response
    DescribeObjectResponse (..),
    newDescribeObjectResponse,

    -- * Response Lenses
    describeObjectResponse_contentLength,
    describeObjectResponse_lastModified,
    describeObjectResponse_cacheControl,
    describeObjectResponse_eTag,
    describeObjectResponse_contentType,
    describeObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaStoreData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeObject' smart constructor.
data DescribeObject = DescribeObject'
  { -- | The path (including the file name) where the object is stored in the
    -- container. Format: \<folder name>\/\<folder name>\/\<file name>
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeObject
newDescribeObject pPath_ =
  DescribeObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
describeObject_path :: Lens.Lens' DescribeObject Prelude.Text
describeObject_path = Lens.lens (\DescribeObject' {path} -> path) (\s@DescribeObject' {} a -> s {path = a} :: DescribeObject)

instance Core.AWSRequest DescribeObject where
  type
    AWSResponse DescribeObject =
      DescribeObjectResponse
  request overrides =
    Request.head' (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DescribeObjectResponse'
            Prelude.<$> (h Core..#? "Content-Length")
            Prelude.<*> (h Core..#? "Last-Modified")
            Prelude.<*> (h Core..#? "Cache-Control")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (h Core..#? "Content-Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeObject where
  hashWithSalt _salt DescribeObject' {..} =
    _salt `Prelude.hashWithSalt` path

instance Prelude.NFData DescribeObject where
  rnf DescribeObject' {..} = Prelude.rnf path

instance Core.ToHeaders DescribeObject where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeObject where
  toPath DescribeObject' {..} =
    Prelude.mconcat ["/", Core.toBS path]

instance Core.ToQuery DescribeObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { -- | The length of the object in bytes.
    contentLength :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that the object was last modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | An optional @CacheControl@ header that allows the caller to control the
    -- object\'s cache behavior. Headers can be passed in as specified in the
    -- HTTP at
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The ETag that represents a unique instance of the object.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The content type of the object.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'eTag', 'describeObjectResponse_eTag' - The ETag that represents a unique instance of the object.
--
-- 'contentType', 'describeObjectResponse_contentType' - The content type of the object.
--
-- 'httpStatus', 'describeObjectResponse_httpStatus' - The response's http status code.
newDescribeObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeObjectResponse
newDescribeObjectResponse pHttpStatus_ =
  DescribeObjectResponse'
    { contentLength =
        Prelude.Nothing,
      lastModified = Prelude.Nothing,
      cacheControl = Prelude.Nothing,
      eTag = Prelude.Nothing,
      contentType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The length of the object in bytes.
describeObjectResponse_contentLength :: Lens.Lens' DescribeObjectResponse (Prelude.Maybe Prelude.Natural)
describeObjectResponse_contentLength = Lens.lens (\DescribeObjectResponse' {contentLength} -> contentLength) (\s@DescribeObjectResponse' {} a -> s {contentLength = a} :: DescribeObjectResponse)

-- | The date and time that the object was last modified.
describeObjectResponse_lastModified :: Lens.Lens' DescribeObjectResponse (Prelude.Maybe Prelude.UTCTime)
describeObjectResponse_lastModified = Lens.lens (\DescribeObjectResponse' {lastModified} -> lastModified) (\s@DescribeObjectResponse' {} a -> s {lastModified = a} :: DescribeObjectResponse) Prelude.. Lens.mapping Core._Time

-- | An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
describeObjectResponse_cacheControl :: Lens.Lens' DescribeObjectResponse (Prelude.Maybe Prelude.Text)
describeObjectResponse_cacheControl = Lens.lens (\DescribeObjectResponse' {cacheControl} -> cacheControl) (\s@DescribeObjectResponse' {} a -> s {cacheControl = a} :: DescribeObjectResponse)

-- | The ETag that represents a unique instance of the object.
describeObjectResponse_eTag :: Lens.Lens' DescribeObjectResponse (Prelude.Maybe Prelude.Text)
describeObjectResponse_eTag = Lens.lens (\DescribeObjectResponse' {eTag} -> eTag) (\s@DescribeObjectResponse' {} a -> s {eTag = a} :: DescribeObjectResponse)

-- | The content type of the object.
describeObjectResponse_contentType :: Lens.Lens' DescribeObjectResponse (Prelude.Maybe Prelude.Text)
describeObjectResponse_contentType = Lens.lens (\DescribeObjectResponse' {contentType} -> contentType) (\s@DescribeObjectResponse' {} a -> s {contentType = a} :: DescribeObjectResponse)

-- | The response's http status code.
describeObjectResponse_httpStatus :: Lens.Lens' DescribeObjectResponse Prelude.Int
describeObjectResponse_httpStatus = Lens.lens (\DescribeObjectResponse' {httpStatus} -> httpStatus) (\s@DescribeObjectResponse' {} a -> s {httpStatus = a} :: DescribeObjectResponse)

instance Prelude.NFData DescribeObjectResponse where
  rnf DescribeObjectResponse' {..} =
    Prelude.rnf contentLength
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf cacheControl
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf httpStatus
