{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new RTMP distribution. An RTMP distribution is similar to a web distribution, but an RTMP distribution streams media files using the Adobe Real-Time Messaging Protocol (RTMP) instead of serving files using HTTP.
--
-- To create a new distribution, submit a @POST@ request to the /CloudFront API version/ /distribution resource. The request body must include a document with a /StreamingDistributionConfig/ element. The response echoes the @StreamingDistributionConfig@ element and returns other information about the RTMP distribution.
-- To get the status of your request, use the /GET StreamingDistribution/ API action. When the value of @Enabled@ is @true@ and the value of @Status@ is @Deployed@ , your distribution is ready. A distribution usually deploys in less than 15 minutes.
-- For more information about web distributions, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-rtmp.html Working with RTMP Distributions> in the /Amazon CloudFront Developer Guide/ .
-- /Important:/ Beginning with the 2012-05-05 version of the CloudFront API, we made substantial changes to the format of the XML document that you include in the request body when you create or update a web distribution or an RTMP distribution, and when you invalidate objects. With previous versions of the API, we discovered that it was too easy to accidentally delete one or more values for an element that accepts multiple values, for example, CNAMEs and trusted signers. Our changes for the 2012-05-05 release are intended to prevent these accidental deletions and to notify you when there's a mismatch between the number of values you say you're specifying in the @Quantity@ element and the number of values specified.
module Network.AWS.CloudFront.CreateStreamingDistribution
  ( -- * Creating a request
    CreateStreamingDistribution (..),
    mkCreateStreamingDistribution,

    -- ** Request lenses
    csdStreamingDistributionConfig,

    -- * Destructuring the response
    CreateStreamingDistributionResponse (..),
    mkCreateStreamingDistributionResponse,

    -- ** Response lenses
    csdrsETag,
    csdrsLocation,
    csdrsStreamingDistribution,
    csdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create a new streaming distribution.
--
-- /See:/ 'mkCreateStreamingDistribution' smart constructor.
newtype CreateStreamingDistribution = CreateStreamingDistribution'
  { streamingDistributionConfig ::
      StreamingDistributionConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingDistribution' with the minimum fields required to make a request.
--
-- * 'streamingDistributionConfig' - The streaming distribution's configuration information.
mkCreateStreamingDistribution ::
  -- | 'streamingDistributionConfig'
  StreamingDistributionConfig ->
  CreateStreamingDistribution
mkCreateStreamingDistribution pStreamingDistributionConfig_ =
  CreateStreamingDistribution'
    { streamingDistributionConfig =
        pStreamingDistributionConfig_
    }

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdStreamingDistributionConfig :: Lens.Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig = Lens.lens (streamingDistributionConfig :: CreateStreamingDistribution -> StreamingDistributionConfig) (\s a -> s {streamingDistributionConfig = a} :: CreateStreamingDistribution)
{-# DEPRECATED csdStreamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead." #-}

instance Lude.AWSRequest CreateStreamingDistribution where
  type
    Rs CreateStreamingDistribution =
      CreateStreamingDistributionResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateStreamingDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateStreamingDistribution where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}StreamingDistributionConfig"
      Lude.. streamingDistributionConfig

instance Lude.ToHeaders CreateStreamingDistribution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateStreamingDistribution where
  toPath = Lude.const "/2020-05-31/streaming-distribution"

instance Lude.ToQuery CreateStreamingDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateStreamingDistributionResponse' smart constructor.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
  { eTag ::
      Lude.Maybe
        Lude.Text,
    location ::
      Lude.Maybe
        Lude.Text,
    streamingDistribution ::
      Lude.Maybe
        StreamingDistribution,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStreamingDistributionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the streaming distribution created.
-- * 'location' - The fully qualified URI of the new streaming distribution resource just created.
-- * 'responseStatus' - The response status code.
-- * 'streamingDistribution' - The streaming distribution's information.
mkCreateStreamingDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStreamingDistributionResponse
mkCreateStreamingDistributionResponse pResponseStatus_ =
  CreateStreamingDistributionResponse'
    { eTag = Lude.Nothing,
      location = Lude.Nothing,
      streamingDistribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the streaming distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsETag :: Lens.Lens' CreateStreamingDistributionResponse (Lude.Maybe Lude.Text)
csdrsETag = Lens.lens (eTag :: CreateStreamingDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateStreamingDistributionResponse)
{-# DEPRECATED csdrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The fully qualified URI of the new streaming distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsLocation :: Lens.Lens' CreateStreamingDistributionResponse (Lude.Maybe Lude.Text)
csdrsLocation = Lens.lens (location :: CreateStreamingDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateStreamingDistributionResponse)
{-# DEPRECATED csdrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsStreamingDistribution :: Lens.Lens' CreateStreamingDistributionResponse (Lude.Maybe StreamingDistribution)
csdrsStreamingDistribution = Lens.lens (streamingDistribution :: CreateStreamingDistributionResponse -> Lude.Maybe StreamingDistribution) (\s a -> s {streamingDistribution = a} :: CreateStreamingDistributionResponse)
{-# DEPRECATED csdrsStreamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrsResponseStatus :: Lens.Lens' CreateStreamingDistributionResponse Lude.Int
csdrsResponseStatus = Lens.lens (responseStatus :: CreateStreamingDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStreamingDistributionResponse)
{-# DEPRECATED csdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
