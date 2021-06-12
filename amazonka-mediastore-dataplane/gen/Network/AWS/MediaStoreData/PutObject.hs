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
-- Module      : Network.AWS.MediaStoreData.PutObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an object to the specified path. Object sizes are limited to 25
-- MB for standard upload availability and 10 MB for streaming upload
-- availability.
module Network.AWS.MediaStoreData.PutObject
  ( -- * Creating a Request
    PutObject (..),
    newPutObject,

    -- * Request Lenses
    putObject_contentType,
    putObject_storageClass,
    putObject_cacheControl,
    putObject_uploadAvailability,
    putObject_path,
    putObject_body,

    -- * Destructuring the Response
    PutObjectResponse (..),
    newPutObjectResponse,

    -- * Response Lenses
    putObjectResponse_eTag,
    putObjectResponse_contentSHA256,
    putObjectResponse_storageClass,
    putObjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutObject' smart constructor.
data PutObject = PutObject'
  { -- | The content type of the object.
    contentType :: Core.Maybe Core.Text,
    -- | Indicates the storage class of a @Put@ request. Defaults to
    -- high-performance temporal storage class, and objects are persisted into
    -- durable storage shortly after being received.
    storageClass :: Core.Maybe StorageClass,
    -- | An optional @CacheControl@ header that allows the caller to control the
    -- object\'s cache behavior. Headers can be passed in as specified in the
    -- HTTP at
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Core.Maybe Core.Text,
    -- | Indicates the availability of an object while it is still uploading. If
    -- the value is set to @streaming@, the object is available for downloading
    -- after some initial buffering but before the object is uploaded
    -- completely. If the value is set to @standard@, the object is available
    -- for downloading only when it is uploaded completely. The default value
    -- for this header is @standard@.
    --
    -- To use this header, you must also set the HTTP @Transfer-Encoding@
    -- header to @chunked@.
    uploadAvailability :: Core.Maybe UploadAvailability,
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
    path :: Core.Text,
    -- | The bytes to be stored.
    body :: Core.HashedBody
  }
  deriving (Core.Show, Core.Generic)

-- |
-- Create a value of 'PutObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'putObject_contentType' - The content type of the object.
--
-- 'storageClass', 'putObject_storageClass' - Indicates the storage class of a @Put@ request. Defaults to
-- high-performance temporal storage class, and objects are persisted into
-- durable storage shortly after being received.
--
-- 'cacheControl', 'putObject_cacheControl' - An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
--
-- 'uploadAvailability', 'putObject_uploadAvailability' - Indicates the availability of an object while it is still uploading. If
-- the value is set to @streaming@, the object is available for downloading
-- after some initial buffering but before the object is uploaded
-- completely. If the value is set to @standard@, the object is available
-- for downloading only when it is uploaded completely. The default value
-- for this header is @standard@.
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@
-- header to @chunked@.
--
-- 'path', 'putObject_path' - The path (including the file name) where the object is stored in the
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
--
-- 'body', 'putObject_body' - The bytes to be stored.
newPutObject ::
  -- | 'path'
  Core.Text ->
  -- | 'body'
  Core.HashedBody ->
  PutObject
newPutObject pPath_ pBody_ =
  PutObject'
    { contentType = Core.Nothing,
      storageClass = Core.Nothing,
      cacheControl = Core.Nothing,
      uploadAvailability = Core.Nothing,
      path = pPath_,
      body = pBody_
    }

-- | The content type of the object.
putObject_contentType :: Lens.Lens' PutObject (Core.Maybe Core.Text)
putObject_contentType = Lens.lens (\PutObject' {contentType} -> contentType) (\s@PutObject' {} a -> s {contentType = a} :: PutObject)

-- | Indicates the storage class of a @Put@ request. Defaults to
-- high-performance temporal storage class, and objects are persisted into
-- durable storage shortly after being received.
putObject_storageClass :: Lens.Lens' PutObject (Core.Maybe StorageClass)
putObject_storageClass = Lens.lens (\PutObject' {storageClass} -> storageClass) (\s@PutObject' {} a -> s {storageClass = a} :: PutObject)

-- | An optional @CacheControl@ header that allows the caller to control the
-- object\'s cache behavior. Headers can be passed in as specified in the
-- HTTP at
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9>.
--
-- Headers with a custom user-defined value are also accepted.
putObject_cacheControl :: Lens.Lens' PutObject (Core.Maybe Core.Text)
putObject_cacheControl = Lens.lens (\PutObject' {cacheControl} -> cacheControl) (\s@PutObject' {} a -> s {cacheControl = a} :: PutObject)

-- | Indicates the availability of an object while it is still uploading. If
-- the value is set to @streaming@, the object is available for downloading
-- after some initial buffering but before the object is uploaded
-- completely. If the value is set to @standard@, the object is available
-- for downloading only when it is uploaded completely. The default value
-- for this header is @standard@.
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@
-- header to @chunked@.
putObject_uploadAvailability :: Lens.Lens' PutObject (Core.Maybe UploadAvailability)
putObject_uploadAvailability = Lens.lens (\PutObject' {uploadAvailability} -> uploadAvailability) (\s@PutObject' {} a -> s {uploadAvailability = a} :: PutObject)

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
putObject_path :: Lens.Lens' PutObject Core.Text
putObject_path = Lens.lens (\PutObject' {path} -> path) (\s@PutObject' {} a -> s {path = a} :: PutObject)

-- | The bytes to be stored.
putObject_body :: Lens.Lens' PutObject Core.HashedBody
putObject_body = Lens.lens (\PutObject' {body} -> body) (\s@PutObject' {} a -> s {body = a} :: PutObject)

instance Core.AWSRequest PutObject where
  type AWSResponse PutObject = PutObjectResponse
  request = Request.putBody defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutObjectResponse'
            Core.<$> (x Core..?> "ETag")
            Core.<*> (x Core..?> "ContentSHA256")
            Core.<*> (x Core..?> "StorageClass")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.ToBody PutObject where
  toBody PutObject' {..} = Core.toBody body

instance Core.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Core.mconcat
      [ "Content-Type" Core.=# contentType,
        "x-amz-storage-class" Core.=# storageClass,
        "Cache-Control" Core.=# cacheControl,
        "x-amz-upload-availability"
          Core.=# uploadAvailability
      ]

instance Core.ToPath PutObject where
  toPath PutObject' {..} =
    Core.mconcat ["/", Core.toBS path]

instance Core.ToQuery PutObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | Unique identifier of the object in the container.
    eTag :: Core.Maybe Core.Text,
    -- | The SHA256 digest of the object that is persisted.
    contentSHA256 :: Core.Maybe Core.Text,
    -- | The storage class where the object was persisted. The class should be
    -- “Temporal”.
    storageClass :: Core.Maybe StorageClass,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'putObjectResponse_eTag' - Unique identifier of the object in the container.
--
-- 'contentSHA256', 'putObjectResponse_contentSHA256' - The SHA256 digest of the object that is persisted.
--
-- 'storageClass', 'putObjectResponse_storageClass' - The storage class where the object was persisted. The class should be
-- “Temporal”.
--
-- 'httpStatus', 'putObjectResponse_httpStatus' - The response's http status code.
newPutObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutObjectResponse
newPutObjectResponse pHttpStatus_ =
  PutObjectResponse'
    { eTag = Core.Nothing,
      contentSHA256 = Core.Nothing,
      storageClass = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the object in the container.
putObjectResponse_eTag :: Lens.Lens' PutObjectResponse (Core.Maybe Core.Text)
putObjectResponse_eTag = Lens.lens (\PutObjectResponse' {eTag} -> eTag) (\s@PutObjectResponse' {} a -> s {eTag = a} :: PutObjectResponse)

-- | The SHA256 digest of the object that is persisted.
putObjectResponse_contentSHA256 :: Lens.Lens' PutObjectResponse (Core.Maybe Core.Text)
putObjectResponse_contentSHA256 = Lens.lens (\PutObjectResponse' {contentSHA256} -> contentSHA256) (\s@PutObjectResponse' {} a -> s {contentSHA256 = a} :: PutObjectResponse)

-- | The storage class where the object was persisted. The class should be
-- “Temporal”.
putObjectResponse_storageClass :: Lens.Lens' PutObjectResponse (Core.Maybe StorageClass)
putObjectResponse_storageClass = Lens.lens (\PutObjectResponse' {storageClass} -> storageClass) (\s@PutObjectResponse' {} a -> s {storageClass = a} :: PutObjectResponse)

-- | The response's http status code.
putObjectResponse_httpStatus :: Lens.Lens' PutObjectResponse Core.Int
putObjectResponse_httpStatus = Lens.lens (\PutObjectResponse' {httpStatus} -> httpStatus) (\s@PutObjectResponse' {} a -> s {httpStatus = a} :: PutObjectResponse)

instance Core.NFData PutObjectResponse
