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
-- Module      : Amazonka.MediaStoreData.GetObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the object at the specified path. If the object’s upload
-- availability is set to @streaming@, AWS Elemental MediaStore downloads
-- the object even if it’s still uploading the object.
module Amazonka.MediaStoreData.GetObject
  ( -- * Creating a Request
    GetObject (..),
    newGetObject,

    -- * Request Lenses
    getObject_range,
    getObject_path,

    -- * Destructuring the Response
    GetObjectResponse (..),
    newGetObjectResponse,

    -- * Response Lenses
    getObjectResponse_cacheControl,
    getObjectResponse_contentLength,
    getObjectResponse_contentRange,
    getObjectResponse_contentType,
    getObjectResponse_eTag,
    getObjectResponse_lastModified,
    getObjectResponse_statusCode,
    getObjectResponse_body,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStoreData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetObject' smart constructor.
data GetObject = GetObject'
  { -- | The range bytes of an object to retrieve. For more information about the
    -- @Range@ header, see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>. AWS
    -- Elemental MediaStore ignores this header for partially uploaded objects
    -- that have streaming upload availability.
    range :: Prelude.Maybe Prelude.Text,
    -- | The path (including the file name) where the object is stored in the
    -- container. Format: \<folder name>\/\<folder name>\/\<file name>
    --
    -- For example, to upload the file @mlaw.avi@ to the folder path
    -- @premium\\canada@ in the container @movies@, enter the path
    -- @premium\/canada\/mlaw.avi@.
    --
    -- Do not include the container name in this path.
    --
    -- If the path includes any folders that don\'t exist yet, the service
    -- creates them. For example, suppose you have an existing @premium\/usa@
    -- subfolder. If you specify @premium\/canada@, the service creates a
    -- @canada@ subfolder in the @premium@ folder. You then have two
    -- subfolders, @usa@ and @canada@, in the @premium@ folder.
    --
    -- There is no correlation between the path to the source and the path
    -- (folders) in the container in AWS Elemental MediaStore.
    --
    -- For more information about folders and how they exist in a container,
    -- see the
    -- <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide>.
    --
    -- The file name is the name that is assigned to the file that you upload.
    -- The file can have the same name inside and outside of AWS Elemental
    -- MediaStore, or it can have the same name. The file name can include or
    -- omit an extension.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'getObject_range' - The range bytes of an object to retrieve. For more information about the
-- @Range@ header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>. AWS
-- Elemental MediaStore ignores this header for partially uploaded objects
-- that have streaming upload availability.
--
-- 'path', 'getObject_path' - The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path
-- @premium\\canada@ in the container @movies@, enter the path
-- @premium\/canada\/mlaw.avi@.
--
-- Do not include the container name in this path.
--
-- If the path includes any folders that don\'t exist yet, the service
-- creates them. For example, suppose you have an existing @premium\/usa@
-- subfolder. If you specify @premium\/canada@, the service creates a
-- @canada@ subfolder in the @premium@ folder. You then have two
-- subfolders, @usa@ and @canada@, in the @premium@ folder.
--
-- There is no correlation between the path to the source and the path
-- (folders) in the container in AWS Elemental MediaStore.
--
-- For more information about folders and how they exist in a container,
-- see the
-- <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide>.
--
-- The file name is the name that is assigned to the file that you upload.
-- The file can have the same name inside and outside of AWS Elemental
-- MediaStore, or it can have the same name. The file name can include or
-- omit an extension.
newGetObject ::
  -- | 'path'
  Prelude.Text ->
  GetObject
newGetObject pPath_ =
  GetObject' {range = Prelude.Nothing, path = pPath_}

-- | The range bytes of an object to retrieve. For more information about the
-- @Range@ header, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35>. AWS
-- Elemental MediaStore ignores this header for partially uploaded objects
-- that have streaming upload availability.
getObject_range :: Lens.Lens' GetObject (Prelude.Maybe Prelude.Text)
getObject_range = Lens.lens (\GetObject' {range} -> range) (\s@GetObject' {} a -> s {range = a} :: GetObject)

-- | The path (including the file name) where the object is stored in the
-- container. Format: \<folder name>\/\<folder name>\/\<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path
-- @premium\\canada@ in the container @movies@, enter the path
-- @premium\/canada\/mlaw.avi@.
--
-- Do not include the container name in this path.
--
-- If the path includes any folders that don\'t exist yet, the service
-- creates them. For example, suppose you have an existing @premium\/usa@
-- subfolder. If you specify @premium\/canada@, the service creates a
-- @canada@ subfolder in the @premium@ folder. You then have two
-- subfolders, @usa@ and @canada@, in the @premium@ folder.
--
-- There is no correlation between the path to the source and the path
-- (folders) in the container in AWS Elemental MediaStore.
--
-- For more information about folders and how they exist in a container,
-- see the
-- <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide>.
--
-- The file name is the name that is assigned to the file that you upload.
-- The file can have the same name inside and outside of AWS Elemental
-- MediaStore, or it can have the same name. The file name can include or
-- omit an extension.
getObject_path :: Lens.Lens' GetObject Prelude.Text
getObject_path = Lens.lens (\GetObject' {path} -> path) (\s@GetObject' {} a -> s {path = a} :: GetObject)

instance Core.AWSRequest GetObject where
  type AWSResponse GetObject = GetObjectResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Prelude.<$> (h Data..#? "Cache-Control")
            Prelude.<*> (h Data..#? "Content-Length")
            Prelude.<*> (h Data..#? "Content-Range")
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (h Data..#? "Last-Modified")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetObject where
  hashWithSalt _salt GetObject' {..} =
    _salt `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` path

instance Prelude.NFData GetObject where
  rnf GetObject' {..} =
    Prelude.rnf range `Prelude.seq` Prelude.rnf path

instance Data.ToHeaders GetObject where
  toHeaders GetObject' {..} =
    Prelude.mconcat ["Range" Data.=# range]

instance Data.ToPath GetObject where
  toPath GetObject' {..} =
    Prelude.mconcat ["/", Data.toBS path]

instance Data.ToQuery GetObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { -- | An optional @CacheControl@ header that allows the caller to control the
    -- object\'s cache behavior. Headers can be passed in as specified in the
    -- HTTP spec at
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Prelude.Maybe Prelude.Text,
    -- | The length of the object in bytes.
    contentLength :: Prelude.Maybe Prelude.Natural,
    -- | The range of bytes to retrieve.
    contentRange :: Prelude.Maybe Prelude.Text,
    -- | The content type of the object.
    contentType :: Prelude.Maybe Prelude.Text,
    -- | The ETag that represents a unique instance of the object.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the object was last modified.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The HTML status code of the request. Status codes ranging from 200 to
    -- 299 indicate success. All other status codes indicate the type of error
    -- that occurred.
    statusCode :: Prelude.Int,
    -- | The bytes of the object.
    body :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheControl', 'getObjectResponse_cacheControl' - An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP spec at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
--
-- 'contentLength', 'getObjectResponse_contentLength' - The length of the object in bytes.
--
-- 'contentRange', 'getObjectResponse_contentRange' - The range of bytes to retrieve.
--
-- 'contentType', 'getObjectResponse_contentType' - The content type of the object.
--
-- 'eTag', 'getObjectResponse_eTag' - The ETag that represents a unique instance of the object.
--
-- 'lastModified', 'getObjectResponse_lastModified' - The date and time that the object was last modified.
--
-- 'statusCode', 'getObjectResponse_statusCode' - The HTML status code of the request. Status codes ranging from 200 to
-- 299 indicate success. All other status codes indicate the type of error
-- that occurred.
--
-- 'body', 'getObjectResponse_body' - The bytes of the object.
newGetObjectResponse ::
  -- | 'statusCode'
  Prelude.Int ->
  -- | 'body'
  Data.ResponseBody ->
  GetObjectResponse
newGetObjectResponse pStatusCode_ pBody_ =
  GetObjectResponse'
    { cacheControl = Prelude.Nothing,
      contentLength = Prelude.Nothing,
      contentRange = Prelude.Nothing,
      contentType = Prelude.Nothing,
      eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      statusCode = pStatusCode_,
      body = pBody_
    }

-- | An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP spec at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
getObjectResponse_cacheControl :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_cacheControl = Lens.lens (\GetObjectResponse' {cacheControl} -> cacheControl) (\s@GetObjectResponse' {} a -> s {cacheControl = a} :: GetObjectResponse)

-- | The length of the object in bytes.
getObjectResponse_contentLength :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Natural)
getObjectResponse_contentLength = Lens.lens (\GetObjectResponse' {contentLength} -> contentLength) (\s@GetObjectResponse' {} a -> s {contentLength = a} :: GetObjectResponse)

-- | The range of bytes to retrieve.
getObjectResponse_contentRange :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentRange = Lens.lens (\GetObjectResponse' {contentRange} -> contentRange) (\s@GetObjectResponse' {} a -> s {contentRange = a} :: GetObjectResponse)

-- | The content type of the object.
getObjectResponse_contentType :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_contentType = Lens.lens (\GetObjectResponse' {contentType} -> contentType) (\s@GetObjectResponse' {} a -> s {contentType = a} :: GetObjectResponse)

-- | The ETag that represents a unique instance of the object.
getObjectResponse_eTag :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.Text)
getObjectResponse_eTag = Lens.lens (\GetObjectResponse' {eTag} -> eTag) (\s@GetObjectResponse' {} a -> s {eTag = a} :: GetObjectResponse)

-- | The date and time that the object was last modified.
getObjectResponse_lastModified :: Lens.Lens' GetObjectResponse (Prelude.Maybe Prelude.UTCTime)
getObjectResponse_lastModified = Lens.lens (\GetObjectResponse' {lastModified} -> lastModified) (\s@GetObjectResponse' {} a -> s {lastModified = a} :: GetObjectResponse) Prelude.. Lens.mapping Data._Time

-- | The HTML status code of the request. Status codes ranging from 200 to
-- 299 indicate success. All other status codes indicate the type of error
-- that occurred.
getObjectResponse_statusCode :: Lens.Lens' GetObjectResponse Prelude.Int
getObjectResponse_statusCode = Lens.lens (\GetObjectResponse' {statusCode} -> statusCode) (\s@GetObjectResponse' {} a -> s {statusCode = a} :: GetObjectResponse)

-- | The bytes of the object.
getObjectResponse_body :: Lens.Lens' GetObjectResponse Data.ResponseBody
getObjectResponse_body = Lens.lens (\GetObjectResponse' {body} -> body) (\s@GetObjectResponse' {} a -> s {body = a} :: GetObjectResponse)
