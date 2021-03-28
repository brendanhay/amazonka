{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutObject (..)
    , mkPutObject
    -- ** Request lenses
    , poBody
    , poPath
    , poCacheControl
    , poContentType
    , poStorageClass
    , poUploadAvailability

    -- * Destructuring the response
    , PutObjectResponse (..)
    , mkPutObjectResponse
    -- ** Response lenses
    , porrsContentSHA256
    , porrsETag
    , porrsStorageClass
    , porrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutObject' smart constructor.
data PutObject = PutObject'
  { body :: Core.HashedBody
    -- ^ The bytes to be stored. 
  , path :: Types.PathNaming
    -- ^ The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
-- Do not include the container name in this path.
-- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder. 
-- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
-- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
-- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension. 
  , cacheControl :: Core.Maybe Types.StringPrimitive
    -- ^ An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
  , contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the object.
  , storageClass :: Core.Maybe Types.StorageClass
    -- ^ Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
  , uploadAvailability :: Core.Maybe Types.UploadAvailability
    -- ^ Indicates the availability of an object while it is still uploading. If the value is set to @streaming@ , the object is available for downloading after some initial buffering but before the object is uploaded completely. If the value is set to @standard@ , the object is available for downloading only when it is uploaded completely. The default value for this header is @standard@ .
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@ header to @chunked@ .
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'PutObject' value with any optional fields omitted.
mkPutObject
    :: Core.HashedBody -- ^ 'body'
    -> Types.PathNaming -- ^ 'path'
    -> PutObject
mkPutObject body path
  = PutObject'{body, path, cacheControl = Core.Nothing,
               contentType = Core.Nothing, storageClass = Core.Nothing,
               uploadAvailability = Core.Nothing}

-- | The bytes to be stored. 
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poBody :: Lens.Lens' PutObject Core.HashedBody
poBody = Lens.field @"body"
{-# INLINEABLE poBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

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
poPath :: Lens.Lens' PutObject Types.PathNaming
poPath = Lens.field @"path"
{-# INLINEABLE poPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poCacheControl :: Lens.Lens' PutObject (Core.Maybe Types.StringPrimitive)
poCacheControl = Lens.field @"cacheControl"
{-# INLINEABLE poCacheControl #-}
{-# DEPRECATED cacheControl "Use generic-lens or generic-optics with 'cacheControl' instead"  #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poContentType :: Lens.Lens' PutObject (Core.Maybe Types.ContentType)
poContentType = Lens.field @"contentType"
{-# INLINEABLE poContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | Indicates the storage class of a @Put@ request. Defaults to high-performance temporal storage class, and objects are persisted into durable storage shortly after being received.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poStorageClass :: Lens.Lens' PutObject (Core.Maybe Types.StorageClass)
poStorageClass = Lens.field @"storageClass"
{-# INLINEABLE poStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

-- | Indicates the availability of an object while it is still uploading. If the value is set to @streaming@ , the object is available for downloading after some initial buffering but before the object is uploaded completely. If the value is set to @standard@ , the object is available for downloading only when it is uploaded completely. The default value for this header is @standard@ .
--
-- To use this header, you must also set the HTTP @Transfer-Encoding@ header to @chunked@ .
--
-- /Note:/ Consider using 'uploadAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poUploadAvailability :: Lens.Lens' PutObject (Core.Maybe Types.UploadAvailability)
poUploadAvailability = Lens.field @"uploadAvailability"
{-# INLINEABLE poUploadAvailability #-}
{-# DEPRECATED uploadAvailability "Use generic-lens or generic-optics with 'uploadAvailability' instead"  #-}

instance Core.ToQuery PutObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutObject where
        toHeaders PutObject{..}
          = Core.toHeaders "Cache-Control" cacheControl Core.<>
              Core.toHeaders "Content-Type" contentType
              Core.<> Core.toHeaders "x-amz-storage-class" storageClass
              Core.<>
              Core.toHeaders "x-amz-upload-availability" uploadAvailability

instance Core.AWSRequest PutObject where
        type Rs PutObject = PutObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText path,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toBody body}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutObjectResponse' Core.<$>
                   (x Core..:? "ContentSHA256") Core.<*> x Core..:? "ETag" Core.<*>
                     x Core..:? "StorageClass"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutObjectResponse' smart constructor.
data PutObjectResponse = PutObjectResponse'
  { contentSHA256 :: Core.Maybe Types.ContentSHA256
    -- ^ The SHA256 digest of the object that is persisted.
  , eTag :: Core.Maybe Types.ETag
    -- ^ Unique identifier of the object in the container.
  , storageClass :: Core.Maybe Types.StorageClass
    -- ^ The storage class where the object was persisted. The class should be “Temporal”.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectResponse' value with any optional fields omitted.
mkPutObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutObjectResponse
mkPutObjectResponse responseStatus
  = PutObjectResponse'{contentSHA256 = Core.Nothing,
                       eTag = Core.Nothing, storageClass = Core.Nothing, responseStatus}

-- | The SHA256 digest of the object that is persisted.
--
-- /Note:/ Consider using 'contentSHA256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsContentSHA256 :: Lens.Lens' PutObjectResponse (Core.Maybe Types.ContentSHA256)
porrsContentSHA256 = Lens.field @"contentSHA256"
{-# INLINEABLE porrsContentSHA256 #-}
{-# DEPRECATED contentSHA256 "Use generic-lens or generic-optics with 'contentSHA256' instead"  #-}

-- | Unique identifier of the object in the container.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsETag :: Lens.Lens' PutObjectResponse (Core.Maybe Types.ETag)
porrsETag = Lens.field @"eTag"
{-# INLINEABLE porrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The storage class where the object was persisted. The class should be “Temporal”.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsStorageClass :: Lens.Lens' PutObjectResponse (Core.Maybe Types.StorageClass)
porrsStorageClass = Lens.field @"storageClass"
{-# INLINEABLE porrsStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
porrsResponseStatus :: Lens.Lens' PutObjectResponse Core.Int
porrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE porrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
