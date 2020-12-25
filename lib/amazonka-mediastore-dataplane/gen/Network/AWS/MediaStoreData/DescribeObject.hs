{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.DescribeObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the headers for an object at the specified path.
module Network.AWS.MediaStoreData.DescribeObject
  ( -- * Creating a request
    DescribeObject (..),
    mkDescribeObject,

    -- ** Request lenses
    dPath,

    -- * Destructuring the response
    DescribeObjectResponse (..),
    mkDescribeObjectResponse,

    -- ** Response lenses
    drsCacheControl,
    drsContentLength,
    drsContentType,
    drsETag,
    drsLastModified,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeObject' smart constructor.
newtype DescribeObject = DescribeObject'
  { -- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
    path :: Types.PathNaming
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObject' value with any optional fields omitted.
mkDescribeObject ::
  -- | 'path'
  Types.PathNaming ->
  DescribeObject
mkDescribeObject path = DescribeObject' {path}

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPath :: Lens.Lens' DescribeObject Types.PathNaming
dPath = Lens.field @"path"
{-# DEPRECATED dPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.AWSRequest DescribeObject where
  type Rs DescribeObject = DescribeObjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.HEAD,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText path)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DescribeObjectResponse'
            Core.<$> (Core.parseHeaderMaybe "Cache-Control" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Length" h)
            Core.<*> (Core.parseHeaderMaybe "Content-Type" h)
            Core.<*> (Core.parseHeaderMaybe "ETag" h)
            Core.<*> (Core.parseHeaderMaybe "Last-Modified" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { -- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
    --
    -- Headers with a custom user-defined value are also accepted.
    cacheControl :: Core.Maybe Types.StringPrimitive,
    -- | The length of the object in bytes.
    contentLength :: Core.Maybe Core.Natural,
    -- | The content type of the object.
    contentType :: Core.Maybe Types.ContentType,
    -- | The ETag that represents a unique instance of the object.
    eTag :: Core.Maybe Types.ETag,
    -- | The date and time that the object was last modified.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeObjectResponse' value with any optional fields omitted.
mkDescribeObjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeObjectResponse
mkDescribeObjectResponse responseStatus =
  DescribeObjectResponse'
    { cacheControl = Core.Nothing,
      contentLength = Core.Nothing,
      contentType = Core.Nothing,
      eTag = Core.Nothing,
      lastModified = Core.Nothing,
      responseStatus
    }

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCacheControl :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.StringPrimitive)
drsCacheControl = Lens.field @"cacheControl"
{-# DEPRECATED drsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The length of the object in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentLength :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Natural)
drsContentLength = Lens.field @"contentLength"
{-# DEPRECATED drsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentType :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.ContentType)
drsContentType = Lens.field @"contentType"
{-# DEPRECATED drsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The ETag that represents a unique instance of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsETag :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.ETag)
drsETag = Lens.field @"eTag"
{-# DEPRECATED drsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The date and time that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastModified :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.NominalDiffTime)
drsLastModified = Lens.field @"lastModified"
{-# DEPRECATED drsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeObjectResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
