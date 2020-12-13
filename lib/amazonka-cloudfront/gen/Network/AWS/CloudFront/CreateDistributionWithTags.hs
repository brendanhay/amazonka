{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateDistributionWithTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new distribution with tags.
module Network.AWS.CloudFront.CreateDistributionWithTags
  ( -- * Creating a request
    CreateDistributionWithTags (..),
    mkCreateDistributionWithTags,

    -- ** Request lenses
    cdwtDistributionConfigWithTags,

    -- * Destructuring the response
    CreateDistributionWithTagsResponse (..),
    mkCreateDistributionWithTagsResponse,

    -- ** Response lenses
    cdwtrsETag,
    cdwtrsDistribution,
    cdwtrsLocation,
    cdwtrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create a new distribution with tags.
--
-- /See:/ 'mkCreateDistributionWithTags' smart constructor.
newtype CreateDistributionWithTags = CreateDistributionWithTags'
  { -- | The distribution's configuration information.
    distributionConfigWithTags :: DistributionConfigWithTags
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDistributionWithTags' with the minimum fields required to make a request.
--
-- * 'distributionConfigWithTags' - The distribution's configuration information.
mkCreateDistributionWithTags ::
  -- | 'distributionConfigWithTags'
  DistributionConfigWithTags ->
  CreateDistributionWithTags
mkCreateDistributionWithTags pDistributionConfigWithTags_ =
  CreateDistributionWithTags'
    { distributionConfigWithTags =
        pDistributionConfigWithTags_
    }

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfigWithTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtDistributionConfigWithTags :: Lens.Lens' CreateDistributionWithTags DistributionConfigWithTags
cdwtDistributionConfigWithTags = Lens.lens (distributionConfigWithTags :: CreateDistributionWithTags -> DistributionConfigWithTags) (\s a -> s {distributionConfigWithTags = a} :: CreateDistributionWithTags)
{-# DEPRECATED cdwtDistributionConfigWithTags "Use generic-lens or generic-optics with 'distributionConfigWithTags' instead." #-}

instance Lude.AWSRequest CreateDistributionWithTags where
  type
    Rs CreateDistributionWithTags =
      CreateDistributionWithTagsResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateDistributionWithTagsResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateDistributionWithTags where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfigWithTags"
      Lude.. distributionConfigWithTags

instance Lude.ToHeaders CreateDistributionWithTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDistributionWithTags where
  toPath = Lude.const "/2020-05-31/distribution"

instance Lude.ToQuery CreateDistributionWithTags where
  toQuery = Lude.const (Lude.mconcat ["WithTags"])

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { -- | The current version of the distribution created.
    eTag :: Lude.Maybe Lude.Text,
    -- | The distribution's information.
    distribution :: Lude.Maybe Distribution,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDistributionWithTagsResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the distribution created.
-- * 'distribution' - The distribution's information.
-- * 'location' - The fully qualified URI of the new distribution resource just created.
-- * 'responseStatus' - The response status code.
mkCreateDistributionWithTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDistributionWithTagsResponse
mkCreateDistributionWithTagsResponse pResponseStatus_ =
  CreateDistributionWithTagsResponse'
    { eTag = Lude.Nothing,
      distribution = Lude.Nothing,
      location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrsETag :: Lens.Lens' CreateDistributionWithTagsResponse (Lude.Maybe Lude.Text)
cdwtrsETag = Lens.lens (eTag :: CreateDistributionWithTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateDistributionWithTagsResponse)
{-# DEPRECATED cdwtrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrsDistribution :: Lens.Lens' CreateDistributionWithTagsResponse (Lude.Maybe Distribution)
cdwtrsDistribution = Lens.lens (distribution :: CreateDistributionWithTagsResponse -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: CreateDistributionWithTagsResponse)
{-# DEPRECATED cdwtrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The fully qualified URI of the new distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrsLocation :: Lens.Lens' CreateDistributionWithTagsResponse (Lude.Maybe Lude.Text)
cdwtrsLocation = Lens.lens (location :: CreateDistributionWithTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateDistributionWithTagsResponse)
{-# DEPRECATED cdwtrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrsResponseStatus :: Lens.Lens' CreateDistributionWithTagsResponse Lude.Int
cdwtrsResponseStatus = Lens.lens (responseStatus :: CreateDistributionWithTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDistributionWithTagsResponse)
{-# DEPRECATED cdwtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
