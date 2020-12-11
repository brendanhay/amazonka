{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.GetCelebrityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the name and additional information about a celebrity based on his or her Amazon Rekognition ID. The additional information is returned as an array of URLs. If there is no additional information about the celebrity, this list is empty.
--
-- For more information, see Recognizing Celebrities in an Image in the Amazon Rekognition Developer Guide.
-- This operation requires permissions to perform the @rekognition:GetCelebrityInfo@ action.
module Network.AWS.Rekognition.GetCelebrityInfo
  ( -- * Creating a request
    GetCelebrityInfo (..),
    mkGetCelebrityInfo,

    -- ** Request lenses
    gciId,

    -- * Destructuring the response
    GetCelebrityInfoResponse (..),
    mkGetCelebrityInfoResponse,

    -- ** Response lenses
    gcirsURLs,
    gcirsName,
    gcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCelebrityInfo' smart constructor.
newtype GetCelebrityInfo = GetCelebrityInfo' {id :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCelebrityInfo' with the minimum fields required to make a request.
--
-- * 'id' - The ID for the celebrity. You get the celebrity ID from a call to the 'RecognizeCelebrities' operation, which recognizes celebrities in an image.
mkGetCelebrityInfo ::
  -- | 'id'
  Lude.Text ->
  GetCelebrityInfo
mkGetCelebrityInfo pId_ = GetCelebrityInfo' {id = pId_}

-- | The ID for the celebrity. You get the celebrity ID from a call to the 'RecognizeCelebrities' operation, which recognizes celebrities in an image.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gciId :: Lens.Lens' GetCelebrityInfo Lude.Text
gciId = Lens.lens (id :: GetCelebrityInfo -> Lude.Text) (\s a -> s {id = a} :: GetCelebrityInfo)
{-# DEPRECATED gciId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetCelebrityInfo where
  type Rs GetCelebrityInfo = GetCelebrityInfoResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetCelebrityInfoResponse'
            Lude.<$> (x Lude..?> "Urls" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCelebrityInfo where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.GetCelebrityInfo" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetCelebrityInfo where
  toJSON GetCelebrityInfo' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath GetCelebrityInfo where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCelebrityInfo where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetCelebrityInfoResponse' smart constructor.
data GetCelebrityInfoResponse = GetCelebrityInfoResponse'
  { urls ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCelebrityInfoResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the celebrity.
-- * 'responseStatus' - The response status code.
-- * 'urls' - An array of URLs pointing to additional celebrity information.
mkGetCelebrityInfoResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCelebrityInfoResponse
mkGetCelebrityInfoResponse pResponseStatus_ =
  GetCelebrityInfoResponse'
    { urls = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of URLs pointing to additional celebrity information.
--
-- /Note:/ Consider using 'urls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsURLs :: Lens.Lens' GetCelebrityInfoResponse (Lude.Maybe [Lude.Text])
gcirsURLs = Lens.lens (urls :: GetCelebrityInfoResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {urls = a} :: GetCelebrityInfoResponse)
{-# DEPRECATED gcirsURLs "Use generic-lens or generic-optics with 'urls' instead." #-}

-- | The name of the celebrity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsName :: Lens.Lens' GetCelebrityInfoResponse (Lude.Maybe Lude.Text)
gcirsName = Lens.lens (name :: GetCelebrityInfoResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetCelebrityInfoResponse)
{-# DEPRECATED gcirsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseStatus :: Lens.Lens' GetCelebrityInfoResponse Lude.Int
gcirsResponseStatus = Lens.lens (responseStatus :: GetCelebrityInfoResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCelebrityInfoResponse)
{-# DEPRECATED gcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
