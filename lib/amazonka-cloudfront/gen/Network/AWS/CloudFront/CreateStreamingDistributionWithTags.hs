{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateStreamingDistributionWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new streaming distribution with tags.
module Network.AWS.CloudFront.CreateStreamingDistributionWithTags
  ( -- * Creating a request
    CreateStreamingDistributionWithTags (..),
    mkCreateStreamingDistributionWithTags,

    -- ** Request lenses
    csdwtStreamingDistributionConfigWithTags,

    -- * Destructuring the response
    CreateStreamingDistributionWithTagsResponse (..),
    mkCreateStreamingDistributionWithTagsResponse,

    -- ** Response lenses
    csdwtrsETag,
    csdwtrsLocation,
    csdwtrsStreamingDistribution,
    csdwtrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create a new streaming distribution with tags.
--
-- /See:/ 'mkCreateStreamingDistributionWithTags' smart constructor.
newtype CreateStreamingDistributionWithTags = CreateStreamingDistributionWithTags'
  { -- | The streaming distribution's configuration information.
    streamingDistributionConfigWithTags :: StreamingDistributionConfigWithTags
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingDistributionWithTags' with the minimum fields required to make a request.
--
-- * 'streamingDistributionConfigWithTags' - The streaming distribution's configuration information.
mkCreateStreamingDistributionWithTags ::
  -- | 'streamingDistributionConfigWithTags'
  StreamingDistributionConfigWithTags ->
  CreateStreamingDistributionWithTags
mkCreateStreamingDistributionWithTags
  pStreamingDistributionConfigWithTags_ =
    CreateStreamingDistributionWithTags'
      { streamingDistributionConfigWithTags =
          pStreamingDistributionConfigWithTags_
      }

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfigWithTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtStreamingDistributionConfigWithTags :: Lens.Lens' CreateStreamingDistributionWithTags StreamingDistributionConfigWithTags
csdwtStreamingDistributionConfigWithTags = Lens.lens (streamingDistributionConfigWithTags :: CreateStreamingDistributionWithTags -> StreamingDistributionConfigWithTags) (\s a -> s {streamingDistributionConfigWithTags = a} :: CreateStreamingDistributionWithTags)
{-# DEPRECATED csdwtStreamingDistributionConfigWithTags "Use generic-lens or generic-optics with 'streamingDistributionConfigWithTags' instead." #-}

instance Lude.AWSRequest CreateStreamingDistributionWithTags where
  type
    Rs CreateStreamingDistributionWithTags =
      CreateStreamingDistributionWithTagsResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateStreamingDistributionWithTagsResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateStreamingDistributionWithTags where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfigWithTags"
      Lude.. streamingDistributionConfigWithTags

instance Lude.ToHeaders CreateStreamingDistributionWithTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStreamingDistributionWithTags where
  toPath = Lude.const "/2020-05-31/streaming-distribution"

instance Lude.ToQuery CreateStreamingDistributionWithTags where
  toQuery = Lude.const (Lude.mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateStreamingDistributionWithTagsResponse' smart constructor.
data CreateStreamingDistributionWithTagsResponse = CreateStreamingDistributionWithTagsResponse'
  { -- | The current version of the distribution created.
    eTag :: Lude.Maybe Lude.Text,
    -- | The fully qualified URI of the new streaming distribution resource just created.
    location :: Lude.Maybe Lude.Text,
    -- | The streaming distribution's information.
    streamingDistribution :: Lude.Maybe StreamingDistribution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingDistributionWithTagsResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the distribution created.
-- * 'location' - The fully qualified URI of the new streaming distribution resource just created.
-- * 'streamingDistribution' - The streaming distribution's information.
-- * 'responseStatus' - The response status code.
mkCreateStreamingDistributionWithTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamingDistributionWithTagsResponse
mkCreateStreamingDistributionWithTagsResponse pResponseStatus_ =
  CreateStreamingDistributionWithTagsResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      streamingDistribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrsETag :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Lude.Maybe Lude.Text)
csdwtrsETag = Lens.lens (eTag :: CreateStreamingDistributionWithTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateStreamingDistributionWithTagsResponse)
{-# DEPRECATED csdwtrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new streaming distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrsLocation :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Lude.Maybe Lude.Text)
csdwtrsLocation = Lens.lens (location :: CreateStreamingDistributionWithTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateStreamingDistributionWithTagsResponse)
{-# DEPRECATED csdwtrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrsStreamingDistribution :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Lude.Maybe StreamingDistribution)
csdwtrsStreamingDistribution = Lens.lens (streamingDistribution :: CreateStreamingDistributionWithTagsResponse -> Lude.Maybe StreamingDistribution) (\s a -> s {streamingDistribution = a} :: CreateStreamingDistributionWithTagsResponse)
{-# DEPRECATED csdwtrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrsResponseStatus :: Lens.Lens' CreateStreamingDistributionWithTagsResponse Lude.Int
csdwtrsResponseStatus = Lens.lens (responseStatus :: CreateStreamingDistributionWithTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamingDistributionWithTagsResponse)
{-# DEPRECATED csdwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
