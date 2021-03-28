{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeObject (..)
    , mkDescribeObject
    -- ** Request lenses
    , dPath

    -- * Destructuring the response
    , DescribeObjectResponse (..)
    , mkDescribeObjectResponse
    -- ** Response lenses
    , drsCacheControl
    , drsContentLength
    , drsContentType
    , drsETag
    , drsLastModified
    , drsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStoreData.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeObject' smart constructor.
newtype DescribeObject = DescribeObject'
  { path :: Types.PathNaming
    -- ^ The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeObject' value with any optional fields omitted.
mkDescribeObject
    :: Types.PathNaming -- ^ 'path'
    -> DescribeObject
mkDescribeObject path = DescribeObject'{path}

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPath :: Lens.Lens' DescribeObject Types.PathNaming
dPath = Lens.field @"path"
{-# INLINEABLE dPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery DescribeObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeObject where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeObject where
        type Rs DescribeObject = DescribeObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.HEAD,
                         Core._rqPath = "/" Core.<> Core.toText path,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DescribeObjectResponse' Core.<$>
                   (Core.parseHeaderMaybe "Cache-Control" h) Core.<*>
                     Core.parseHeaderMaybe "Content-Length" h
                     Core.<*> Core.parseHeaderMaybe "Content-Type" h
                     Core.<*> Core.parseHeaderMaybe "ETag" h
                     Core.<*> Core.parseHeaderMaybe "Last-Modified" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { cacheControl :: Core.Maybe Types.StringPrimitive
    -- ^ An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
  , contentLength :: Core.Maybe Core.Natural
    -- ^ The length of the object in bytes.
  , contentType :: Core.Maybe Types.ContentType
    -- ^ The content type of the object.
  , eTag :: Core.Maybe Types.ETag
    -- ^ The ETag that represents a unique instance of the object.
  , lastModified :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the object was last modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeObjectResponse' value with any optional fields omitted.
mkDescribeObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeObjectResponse
mkDescribeObjectResponse responseStatus
  = DescribeObjectResponse'{cacheControl = Core.Nothing,
                            contentLength = Core.Nothing, contentType = Core.Nothing,
                            eTag = Core.Nothing, lastModified = Core.Nothing, responseStatus}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCacheControl :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.StringPrimitive)
drsCacheControl = Lens.field @"cacheControl"
{-# INLINEABLE drsCacheControl #-}
{-# DEPRECATED cacheControl "Use generic-lens or generic-optics with 'cacheControl' instead"  #-}

-- | The length of the object in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentLength :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.Natural)
drsContentLength = Lens.field @"contentLength"
{-# INLINEABLE drsContentLength #-}
{-# DEPRECATED contentLength "Use generic-lens or generic-optics with 'contentLength' instead"  #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentType :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.ContentType)
drsContentType = Lens.field @"contentType"
{-# INLINEABLE drsContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The ETag that represents a unique instance of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsETag :: Lens.Lens' DescribeObjectResponse (Core.Maybe Types.ETag)
drsETag = Lens.field @"eTag"
{-# INLINEABLE drsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The date and time that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastModified :: Lens.Lens' DescribeObjectResponse (Core.Maybe Core.NominalDiffTime)
drsLastModified = Lens.field @"lastModified"
{-# INLINEABLE drsLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeObjectResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
