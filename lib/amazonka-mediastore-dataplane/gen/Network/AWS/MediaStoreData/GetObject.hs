{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.GetObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the object at the specified path. If the object’s upload availability is set to @streaming@ , AWS Elemental MediaStore downloads the object even if it’s still uploading the object.
module Network.AWS.MediaStoreData.GetObject
  ( -- * Creating a request
    GetObject (..),
    mkGetObject,

    -- ** Request lenses
    goPath,
    goRange,

    -- * Destructuring the response
    GetObjectResponse (..),
    mkGetObjectResponse,

    -- ** Response lenses
    gorrsStatusCode,
    gorrsBody,
    gorrsCacheControl,
    gorrsContentLength,
    gorrsContentRange,
    gorrsContentType,
    gorrsETag,
    gorrsLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetObject' smart constructor.
data GetObject = GetObject'
  { -- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
    --
    -- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
    -- Do not include the container name in this path.
    -- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.
    -- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
    -- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
    -- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
    path :: Types.PathNaming,
    -- | The range bytes of an object to retrieve. For more information about the @Range@ header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> . AWS Elemental MediaStore ignores this header for partially uploaded objects that have streaming upload availability.
    range :: Core.Maybe Types.RangePattern
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObject' value with any optional fields omitted.
mkGetObject ::
  -- | 'path'
  Types.PathNaming ->
  GetObject
mkGetObject path = GetObject' {path, range = Core.Nothing}

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
goPath :: Lens.Lens' GetObject Types.PathNaming
goPath = Lens.field @"path"
{-# DEPRECATED goPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The range bytes of an object to retrieve. For more information about the @Range@ header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> . AWS Elemental MediaStore ignores this header for partially uploaded objects that have streaming upload availability.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRange :: Lens.Lens' GetObject (Core.Maybe Types.RangePattern)
goRange = Lens.field @"range"
{-# DEPRECATED goRange "Use generic-lens or generic-optics with 'range' instead." #-}

instance Core.AWSRequest GetObject where
  type Rs GetObject = GetObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText path)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "Range" range,
        Core._rqBody = ""
      }
  response =
    Response.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.pure x)
            Core.<*> (Core.parseHeaderMaybe "Cache-Control" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Length" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Range" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Type" h)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Last-Modified" h)
      )

-- | /See:/ 'mkGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { -- | The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
    statusCode :: Core.Int,
    -- | The bytes of the object.
    body :: Core.RsBody,
    -- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Core.Maybe Types.StringPrimitive,
    -- | The length of the object in bytes.
    contentLength :: Core.Maybe Core.Natural,
    -- | The range of bytes to retrieve.
    contentRange :: Core.Maybe Types.ContentRange,
    -- | The content type of the object.
    contentType :: Core.Maybe Types.ContentType,
    -- | The ETag that represents a unique instance of the object.
    eTag :: Core.Maybe Types.ETag,
    -- | The date and time that the object was last modified.
    lastModified :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Show, Core.Generic)

-- | Creates a 'GetObjectResponse' value with any optional fields omitted.
mkGetObjectResponse ::
  -- | 'statusCode'
  Core.Int ->
  -- | 'body'
  Core.RsBody ->
  GetObjectResponse
mkGetObjectResponse statusCode body =
  GetObjectResponse'
    { statusCode,
      body,
      cacheControl = Core.Nothing,
      contentLength = Core.Nothing,
      contentRange = Core.Nothing,
      contentType = Core.Nothing,
      eTag = Core.Nothing,
      lastModified = Core.Nothing
    }

-- | The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsStatusCode :: Lens.Lens' GetObjectResponse Core.Int
gorrsStatusCode = Lens.field @"statusCode"
{-# DEPRECATED gorrsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The bytes of the object.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsBody :: Lens.Lens' GetObjectResponse Core.RsBody
gorrsBody = Lens.field @"body"
{-# DEPRECATED gorrsBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsCacheControl :: Lens.Lens' GetObjectResponse (Core.Maybe Types.StringPrimitive)
gorrsCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED gorrsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The length of the object in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentLength :: Lens.Lens' GetObjectResponse (Core.Maybe Core.Natural)
gorrsContentLength = Lens.field @"contentLength"
{-# DEPRECATED gorrsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The range of bytes to retrieve.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentRange :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentRange)
gorrsContentRange = Lens.field @"contentRange"
{-# DEPRECATED gorrsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsContentType :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ContentType)
gorrsContentType = Lens.field @"contentType"
{-# DEPRECATED gorrsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The ETag that represents a unique instance of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsETag :: Lens.Lens' GetObjectResponse (Core.Maybe Types.ETag)
gorrsETag = Lens.field @"eTag"
{-# DEPRECATED gorrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The date and time that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorrsLastModified :: Lens.Lens' GetObjectResponse (Core.Maybe Core.NominalDiffTime)
gorrsLastModified = Lens.field @"lastModified"
{-# DEPRECATED gorrsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}
