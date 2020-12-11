{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    drsETag,
    drsContentLength,
    drsCacheControl,
    drsLastModified,
    drsContentType,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeObject' smart constructor.
newtype DescribeObject = DescribeObject' {path :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeObject' with the minimum fields required to make a request.
--
-- * 'path' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
mkDescribeObject ::
  -- | 'path'
  Lude.Text ->
  DescribeObject
mkDescribeObject pPath_ = DescribeObject' {path = pPath_}

-- | The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPath :: Lens.Lens' DescribeObject Lude.Text
dPath = Lens.lens (path :: DescribeObject -> Lude.Text) (\s a -> s {path = a} :: DescribeObject)
{-# DEPRECATED dPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.AWSRequest DescribeObject where
  type Rs DescribeObject = DescribeObjectResponse
  request = Req.head' mediaStoreDataService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DescribeObjectResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Content-Length")
            Lude.<*> (h Lude..#? "Cache-Control")
            Lude.<*> (h Lude..#? "Last-Modified")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeObject where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeObject where
  toPath DescribeObject' {..} = Lude.mconcat ["/", Lude.toBS path]

instance Lude.ToQuery DescribeObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeObjectResponse' smart constructor.
data DescribeObjectResponse = DescribeObjectResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    contentLength :: Lude.Maybe Lude.Natural,
    cacheControl :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.Timestamp,
    contentType :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeObjectResponse' with the minimum fields required to make a request.
--
-- * 'cacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
-- * 'contentLength' - The length of the object in bytes.
-- * 'contentType' - The content type of the object.
-- * 'eTag' - The ETag that represents a unique instance of the object.
-- * 'lastModified' - The date and time that the object was last modified.
-- * 'responseStatus' - The response status code.
mkDescribeObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeObjectResponse
mkDescribeObjectResponse pResponseStatus_ =
  DescribeObjectResponse'
    { eTag = Lude.Nothing,
      contentLength = Lude.Nothing,
      cacheControl = Lude.Nothing,
      lastModified = Lude.Nothing,
      contentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ETag that represents a unique instance of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsETag :: Lens.Lens' DescribeObjectResponse (Lude.Maybe Lude.Text)
drsETag = Lens.lens (eTag :: DescribeObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: DescribeObjectResponse)
{-# DEPRECATED drsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The length of the object in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentLength :: Lens.Lens' DescribeObjectResponse (Lude.Maybe Lude.Natural)
drsContentLength = Lens.lens (contentLength :: DescribeObjectResponse -> Lude.Maybe Lude.Natural) (\s a -> s {contentLength = a} :: DescribeObjectResponse)
{-# DEPRECATED drsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCacheControl :: Lens.Lens' DescribeObjectResponse (Lude.Maybe Lude.Text)
drsCacheControl = Lens.lens (cacheControl :: DescribeObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: DescribeObjectResponse)
{-# DEPRECATED drsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The date and time that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastModified :: Lens.Lens' DescribeObjectResponse (Lude.Maybe Lude.Timestamp)
drsLastModified = Lens.lens (lastModified :: DescribeObjectResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: DescribeObjectResponse)
{-# DEPRECATED drsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsContentType :: Lens.Lens' DescribeObjectResponse (Lude.Maybe Lude.Text)
drsContentType = Lens.lens (contentType :: DescribeObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: DescribeObjectResponse)
{-# DEPRECATED drsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeObjectResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeObjectResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
