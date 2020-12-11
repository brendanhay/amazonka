{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    goRange,
    goPath,

    -- * Destructuring the response
    GetObjectResponse (..),
    mkGetObjectResponse,

    -- ** Response lenses
    gorsETag,
    gorsContentLength,
    gorsCacheControl,
    gorsLastModified,
    gorsContentRange,
    gorsContentType,
    gorsStatusCode,
    gorsBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStoreData.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetObject' smart constructor.
data GetObject = GetObject'
  { range :: Lude.Maybe Lude.Text,
    path :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObject' with the minimum fields required to make a request.
--
-- * 'path' - The path (including the file name) where the object is stored in the container. Format: <folder name>/<folder name>/<file name>
--
-- For example, to upload the file @mlaw.avi@ to the folder path @premium\canada@ in the container @movies@ , enter the path @premium/canada/mlaw.avi@ .
-- Do not include the container name in this path.
-- If the path includes any folders that don't exist yet, the service creates them. For example, suppose you have an existing @premium/usa@ subfolder. If you specify @premium/canada@ , the service creates a @canada@ subfolder in the @premium@ folder. You then have two subfolders, @usa@ and @canada@ , in the @premium@ folder.
-- There is no correlation between the path to the source and the path (folders) in the container in AWS Elemental MediaStore.
-- For more information about folders and how they exist in a container, see the <http://docs.aws.amazon.com/mediastore/latest/ug/ AWS Elemental MediaStore User Guide> .
-- The file name is the name that is assigned to the file that you upload. The file can have the same name inside and outside of AWS Elemental MediaStore, or it can have the same name. The file name can include or omit an extension.
-- * 'range' - The range bytes of an object to retrieve. For more information about the @Range@ header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> . AWS Elemental MediaStore ignores this header for partially uploaded objects that have streaming upload availability.
mkGetObject ::
  -- | 'path'
  Lude.Text ->
  GetObject
mkGetObject pPath_ =
  GetObject' {range = Lude.Nothing, path = pPath_}

-- | The range bytes of an object to retrieve. For more information about the @Range@ header, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35 http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35> . AWS Elemental MediaStore ignores this header for partially uploaded objects that have streaming upload availability.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goRange :: Lens.Lens' GetObject (Lude.Maybe Lude.Text)
goRange = Lens.lens (range :: GetObject -> Lude.Maybe Lude.Text) (\s a -> s {range = a} :: GetObject)
{-# DEPRECATED goRange "Use generic-lens or generic-optics with 'range' instead." #-}

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
goPath :: Lens.Lens' GetObject Lude.Text
goPath = Lens.lens (path :: GetObject -> Lude.Text) (\s a -> s {path = a} :: GetObject)
{-# DEPRECATED goPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.AWSRequest GetObject where
  type Rs GetObject = GetObjectResponse
  request = Req.get mediaStoreDataService
  response =
    Res.receiveBody
      ( \s h x ->
          GetObjectResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Content-Length")
            Lude.<*> (h Lude..#? "Cache-Control")
            Lude.<*> (h Lude..#? "Last-Modified")
            Lude.<*> (h Lude..#? "Content-Range")
            Lude.<*> (h Lude..#? "Content-Type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (Lude.pure x)
      )

instance Lude.ToHeaders GetObject where
  toHeaders GetObject' {..} = Lude.mconcat ["Range" Lude.=# range]

instance Lude.ToPath GetObject where
  toPath GetObject' {..} = Lude.mconcat ["/", Lude.toBS path]

instance Lude.ToQuery GetObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetObjectResponse' smart constructor.
data GetObjectResponse = GetObjectResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    contentLength :: Lude.Maybe Lude.Natural,
    cacheControl :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.Timestamp,
    contentRange :: Lude.Maybe Lude.Text,
    contentType :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Int,
    body :: Lude.RsBody
  }
  deriving stock (Lude.Show, Lude.Generic)

-- | Creates a value of 'GetObjectResponse' with the minimum fields required to make a request.
--
-- * 'body' - The bytes of the object.
-- * 'cacheControl' - An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
-- * 'contentLength' - The length of the object in bytes.
-- * 'contentRange' - The range of bytes to retrieve.
-- * 'contentType' - The content type of the object.
-- * 'eTag' - The ETag that represents a unique instance of the object.
-- * 'lastModified' - The date and time that the object was last modified.
-- * 'statusCode' - The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
mkGetObjectResponse ::
  -- | 'statusCode'
  Lude.Int ->
  -- | 'body'
  Lude.RsBody ->
  GetObjectResponse
mkGetObjectResponse pStatusCode_ pBody_ =
  GetObjectResponse'
    { eTag = Lude.Nothing,
      contentLength = Lude.Nothing,
      cacheControl = Lude.Nothing,
      lastModified = Lude.Nothing,
      contentRange = Lude.Nothing,
      contentType = Lude.Nothing,
      statusCode = pStatusCode_,
      body = pBody_
    }

-- | The ETag that represents a unique instance of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsETag :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsETag = Lens.lens (eTag :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: GetObjectResponse)
{-# DEPRECATED gorsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The length of the object in bytes.
--
-- /Note:/ Consider using 'contentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentLength :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Natural)
gorsContentLength = Lens.lens (contentLength :: GetObjectResponse -> Lude.Maybe Lude.Natural) (\s a -> s {contentLength = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentLength "Use generic-lens or generic-optics with 'contentLength' instead." #-}

-- | An optional @CacheControl@ header that allows the caller to control the object's cache behavior. Headers can be passed in as specified in the HTTP spec at <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9 https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9> .
--
-- Headers with a custom user-defined value are also accepted.
--
-- /Note:/ Consider using 'cacheControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsCacheControl :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsCacheControl = Lens.lens (cacheControl :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {cacheControl = a} :: GetObjectResponse)
{-# DEPRECATED gorsCacheControl "Use generic-lens or generic-optics with 'cacheControl' instead." #-}

-- | The date and time that the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsLastModified :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Timestamp)
gorsLastModified = Lens.lens (lastModified :: GetObjectResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: GetObjectResponse)
{-# DEPRECATED gorsLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The range of bytes to retrieve.
--
-- /Note:/ Consider using 'contentRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentRange :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentRange = Lens.lens (contentRange :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentRange = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentRange "Use generic-lens or generic-optics with 'contentRange' instead." #-}

-- | The content type of the object.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsContentType :: Lens.Lens' GetObjectResponse (Lude.Maybe Lude.Text)
gorsContentType = Lens.lens (contentType :: GetObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: GetObjectResponse)
{-# DEPRECATED gorsContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The HTML status code of the request. Status codes ranging from 200 to 299 indicate success. All other status codes indicate the type of error that occurred.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsStatusCode :: Lens.Lens' GetObjectResponse Lude.Int
gorsStatusCode = Lens.lens (statusCode :: GetObjectResponse -> Lude.Int) (\s a -> s {statusCode = a} :: GetObjectResponse)
{-# DEPRECATED gorsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The bytes of the object.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gorsBody :: Lens.Lens' GetObjectResponse Lude.RsBody
gorsBody = Lens.lens (body :: GetObjectResponse -> Lude.RsBody) (\s a -> s {body = a} :: GetObjectResponse)
{-# DEPRECATED gorsBody "Use generic-lens or generic-optics with 'body' instead." #-}
