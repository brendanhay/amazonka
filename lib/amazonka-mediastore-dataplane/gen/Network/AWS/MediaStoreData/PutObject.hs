{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.PutObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an object to the specified path. Object sizes are limited to 25 MB for standard upload availability and 10 MB for streaming upload availability.
module Network.AWS.MediaStoreData.PutObject
  ( -- * Creating a request
    PutObject (..),
    mkPutObject,

    -- ** Request lenses
    poPath,
    poBody,
    poStorageClass,
    poUploadAvailability,
    poCacheControl,
    poContentType,

    -- * Destructuring the response
    PutObjectResponse (..),
    mkPutObjectResponse,

    -- ** Response lenses
    porsETag,
    porsStorageClass,
    porsContentSHA256,
    porsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutObject' smart constructor.
data PutObject = PutObject'
  { -- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
    --
    -- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
    -- Do not include the container name in this path.
    -- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.
    -- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
    -- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
    -- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
    path :: Lude.Text,
    -- | The bytes to be stored.
    body :: Lude.HashedBody,
    -- | Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
    storageClass :: Lude.Maybe StorageClass,
    -- | Indicates the availability of an object while it is still uploading. If the value is set to @streaming@ , the object is available for downloading after some initial buffering but before the object is uploaded completely. If the value is set to @standard@ , the object is available for downloading only when it is uploaded completely. The default value for this header is @standard@ .
    --
    -- To use this header, you must also set the HTTP @Transfer-Encoding@ header to @chunked@ .
    uploadAvailability :: Lude.Maybe UploadAvailability,
    -- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Lude.Maybe Lude.Text,
    -- | The content type of the object.
    contentType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'PutObject' with the minimum fields required to make a request.
--
-- * 'path' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
-- Do not include the container name in this path.
-- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.
-- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
-- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
-- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
-- * 'body' - The bytes to be stored.
-- * 'storageClass' - Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
-- * 'uploadAvailability' - Indicates the availability of an object while it is still uploading. If the value is set to @streaming@ , the object is available for downloading after some initial buffering but before the object is uploaded completely. If the value is set to @standard@ , the object is available for downloading only when it is uploaded completely. The default value for this header is @standard@ .
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@ header to @chunked@ .
-- * 'cacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
-- * 'contentType' - The content type of the object.
mkPutObject ::
  -- | 'path'
  Lude.Text ->
  -- | 'body'
  Lude.HashedBody ->
  PutObject
mkPutObject pPath_ pBody_ =
  PutObject'
    { path = pPath_,
      body = pBody_,
      storageClass = Lude.Nothing,
      uploadAvailability = Lude.Nothing,
      cacheControl = Lude.Nothing,
      contentType = Lude.Nothing
    }

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
-- Do not include the container name in this path.
-- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.
-- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
-- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
-- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poPath :: Lens.Lens' PutObject Lude.Text
poPath = Lens.lens (path :: PutObject -> Lude.Text) (\s a -> s {path = a} :: PutObject)
{-# DEPRECATED poPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The bytes to be stored.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBody :: Lens.Lens' PutObject Lude.HashedBody
poBody = Lens.lens (body :: PutObject -> Lude.HashedBody) (\s a -> s {body = a} :: PutObject)
{-# DEPRECATED poBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStorageClass :: Lens.Lens' PutObject (Lude.Maybe StorageClass)
poStorageClass = Lens.lens (storageClass :: PutObject -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: PutObject)
{-# DEPRECATED poStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Indicates the availability of an object while it is still uploading. If the value is set to @streaming@ , the object is available for downloading after some initial buffering but before the object is uploaded completely. If the value is set to @standard@ , the object is available for downloading only when it is uploaded completely. The default value for this header is @standard@ .
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@ header to @chunked@ .
--
-- /Note:/ Consider using 'uploadAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poUploadAvailability :: Lens.Lens' PutObject (Lude.Maybe UploadAvailability)
poUploadAvailability = Lens.lens (uploadAvailability :: PutObject -> Lude.Maybe UploadAvailability) (\s a -> s {uploadAvailability = a} :: PutObject)
{-# DEPRECATED poUploadAvailability "Use generic-lens or generic-optics with 'uploadAvailability' instead." #-}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCacheControl :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poCacheControl = Lens.lens (cacheControl :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: PutObject)
{-# DEPRECATED poCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentType :: Lens.Lens' PutObject (Lude.Maybe Lude.Text)
poContentType = Lens.lens (contentType :: PutObject -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: PutObject)
{-# DEPRECATED poContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.AWSRequest PutObject where
  type Rs PutObject = PutObjectResponse
  request = Req.putBody mediaStoreDataService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutObjectResponse'
            Lude.<$> (x Lude..?> "ETag")
            Lude.<*> (x Lude..?> "StorageClass")
            Lude.<*> (x Lude..?> "ContentSHA256")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody PutObject where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders PutObject where
  toHeaders PutObject' {..} =
    Lude.mconcat
      [ "x-amz-storage-class" Lude.=# storageClass,
        "x-amz-upload-availability" Lude.=# uploadAvailability,
        "Cache-Control" Lude.=# cacheControl,
        "Content-Type" Lude.=# contentType
      ]

instance Lude.ToPath PutObject where
  toPath PutObject' {..} = Lude.mconcat ["/", Lude.toBS path]

instance Lude.ToQuery PutObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { -- | Unique identifier of the object in the container.
    eTag :: Lude.Maybe Lude.Text,
    -- | The storage class where the object was persisted. The class should be “Temporal”.
    storageClass :: Lude.Maybe StorageClass,
    -- | The SHA256 digest of the object that is persisted.
    contentSHA256 :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutObjectResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - Unique identifier of the object in the container.
-- * 'storageClass' - The storage class where the object was persisted. The class should be “Temporal”.
-- * 'contentSHA256' - The SHA256 digest of the object that is persisted.
-- * 'responseStatus' - The response status code.
mkPutObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutObjectResponse
mkPutObjectResponse pResponseStatus_ =
  PutObjectResponse'
    { eTag = Lude.Nothing,
      storageClass = Lude.Nothing,
      contentSHA256 = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique identifier of the object in the container.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsETag :: Lens.Lens' PutObjectResponse (Lude.Maybe Lude.Text)
porsETag = Lens.lens (eTag :: PutObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: PutObjectResponse)
{-# DEPRECATED porsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The storage class where the object was persisted. The class should be “Temporal”.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsStorageClass :: Lens.Lens' PutObjectResponse (Lude.Maybe StorageClass)
porsStorageClass = Lens.lens (storageClass :: PutObjectResponse -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: PutObjectResponse)
{-# DEPRECATED porsStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The SHA256 digest of the object that is persisted.
--
-- /Note:/ Consider using 'contentSHA256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsContentSHA256 :: Lens.Lens' PutObjectResponse (Lude.Maybe Lude.Text)
porsContentSHA256 = Lens.lens (contentSHA256 :: PutObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentSHA256 = a} :: PutObjectResponse)
{-# DEPRECATED porsContentSHA256 "Use generic-lens or generic-optics with 'contentSHA256' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porsResponseStatus :: Lens.Lens' PutObjectResponse Lude.Int
porsResponseStatus = Lens.lens (responseStatus :: PutObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutObjectResponse)
{-# DEPRECATED porsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
