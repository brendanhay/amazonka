{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new web distribution. You create a CloudFront distribution to tell CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery. Send a @POST@ request to the @//CloudFront API version/ /distribution@ /@distribution ID@ resource.
--
-- /Important:/ When you update a distribution, there are more required fields than when you create a distribution. When you update your distribution by using <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_UpdateDistribution.html UpdateDistribution> , follow the steps included in the documentation to get the current configuration and then make your updates. This helps to make sure that you include all of the required fields. To view a summary, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateDistribution
  ( -- * Creating a request
    CreateDistribution (..),
    mkCreateDistribution,

    -- ** Request lenses
    cdDistributionConfig,

    -- * Destructuring the response
    CreateDistributionResponse (..),
    mkCreateDistributionResponse,

    -- ** Response lenses
    cdrsETag,
    cdrsDistribution,
    cdrsLocation,
    cdrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to create a new distribution.
--
-- /See:/ 'mkCreateDistribution' smart constructor.
newtype CreateDistribution = CreateDistribution'
  { distributionConfig ::
      DistributionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDistribution' with the minimum fields required to make a request.
--
-- * 'distributionConfig' - The distribution's configuration information.
mkCreateDistribution ::
  -- | 'distributionConfig'
  DistributionConfig ->
  CreateDistribution
mkCreateDistribution pDistributionConfig_ =
  CreateDistribution' {distributionConfig = pDistributionConfig_}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDistributionConfig :: Lens.Lens' CreateDistribution DistributionConfig
cdDistributionConfig = Lens.lens (distributionConfig :: CreateDistribution -> DistributionConfig) (\s a -> s {distributionConfig = a} :: CreateDistribution)
{-# DEPRECATED cdDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

instance Lude.AWSRequest CreateDistribution where
  type Rs CreateDistribution = CreateDistributionResponse
  request = Req.postXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          CreateDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateDistribution where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      Lude.. distributionConfig

instance Lude.ToHeaders CreateDistribution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDistribution where
  toPath = Lude.const "/2020-05-31/distribution"

instance Lude.ToQuery CreateDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { eTag ::
      Lude.Maybe Lude.Text,
    distribution ::
      Lude.Maybe Distribution,
    location :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDistributionResponse' with the minimum fields required to make a request.
--
-- * 'distribution' - The distribution's information.
-- * 'eTag' - The current version of the distribution created.
-- * 'location' - The fully qualified URI of the new distribution resource just created.
-- * 'responseStatus' - The response status code.
mkCreateDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDistributionResponse
mkCreateDistributionResponse pResponseStatus_ =
  CreateDistributionResponse'
    { eTag = Lude.Nothing,
      distribution = Lude.Nothing,
      location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsETag :: Lens.Lens' CreateDistributionResponse (Lude.Maybe Lude.Text)
cdrsETag = Lens.lens (eTag :: CreateDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: CreateDistributionResponse)
{-# DEPRECATED cdrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsDistribution :: Lens.Lens' CreateDistributionResponse (Lude.Maybe Distribution)
cdrsDistribution = Lens.lens (distribution :: CreateDistributionResponse -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: CreateDistributionResponse)
{-# DEPRECATED cdrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The fully qualified URI of the new distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsLocation :: Lens.Lens' CreateDistributionResponse (Lude.Maybe Lude.Text)
cdrsLocation = Lens.lens (location :: CreateDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: CreateDistributionResponse)
{-# DEPRECATED cdrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDistributionResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDistributionResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
