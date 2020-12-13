{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a web distribution.
--
-- /Important:/ When you update a distribution, there are more required fields than when you create a distribution. When you update your distribution by using this API action, follow the steps here to get the current configuration and then make your updates, to make sure that you include all of the required fields. To view a summary, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution> in the /Amazon CloudFront Developer Guide/ .
-- The update process includes getting the current distribution configuration, updating the XML document that is returned to make your changes, and then submitting an @UpdateDistribution@ request to make the updates.
-- For information about updating a distribution using the CloudFront console instead, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-creating-console.html Creating a Distribution> in the /Amazon CloudFront Developer Guide/ .
-- __To update a web distribution using the CloudFront API__
--
--     * Submit a <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistributionConfig.html GetDistributionConfig> request to get the current configuration and an @Etag@ header for the distribution.
--
--
--     * Update the XML document that was returned in the response to your @GetDistributionConfig@ request to include your changes.
-- /Important:/ When you edit the XML file, be aware of the following:
--
--     * You must strip out the ETag parameter that is returned.
--
--
--     * Additional fields are required when you update a distribution. There may be fields included in the XML file for features that you haven't configured for your distribution. This is expected and required to successfully update the distribution.
--
--
--     * You can't change the value of @CallerReference@ . If you try to change this value, CloudFront returns an @IllegalUpdate@ error.
--
--
--     * The new configuration replaces the existing configuration; the values that you specify in an @UpdateDistribution@ request are not merged into your existing configuration. When you add, delete, or replace values in an element that allows multiple values (for example, @CNAME@ ), you must specify all of the values that you want to appear in the updated distribution. In addition, you must update the corresponding @Quantity@ element.
--
--
--
--
--     * Submit an @UpdateDistribution@ request to update the configuration for your distribution:
--
--     * In the request body, include the XML document that you updated in Step 2. The request body must include an XML document with a @DistributionConfig@ element.
--
--
--     * Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GetDistributionConfig@ request in Step 1.
--
--
--
--
--     * Review the response to the @UpdateDistribution@ request to confirm that the configuration was successfully updated.
--
--
--     * Optional: Submit a <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistribution.html GetDistribution> request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
module Network.AWS.CloudFront.UpdateDistribution
  ( -- * Creating a request
    UpdateDistribution (..),
    mkUpdateDistribution,

    -- ** Request lenses
    udIfMatch,
    udDistributionConfig,
    udId,

    -- * Destructuring the response
    UpdateDistributionResponse (..),
    mkUpdateDistributionResponse,

    -- ** Response lenses
    udrsETag,
    udrsDistribution,
    udrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to update a distribution.
--
-- /See:/ 'mkUpdateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { -- | The value of the @ETag@ header that you received when retrieving the distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The distribution's configuration information.
    distributionConfig :: DistributionConfig,
    -- | The distribution's id.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDistribution' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The value of the @ETag@ header that you received when retrieving the distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'distributionConfig' - The distribution's configuration information.
-- * 'id' - The distribution's id.
mkUpdateDistribution ::
  -- | 'distributionConfig'
  DistributionConfig ->
  -- | 'id'
  Lude.Text ->
  UpdateDistribution
mkUpdateDistribution pDistributionConfig_ pId_ =
  UpdateDistribution'
    { ifMatch = Lude.Nothing,
      distributionConfig = pDistributionConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the distribution's configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udIfMatch :: Lens.Lens' UpdateDistribution (Lude.Maybe Lude.Text)
udIfMatch = Lens.lens (ifMatch :: UpdateDistribution -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: UpdateDistribution)
{-# DEPRECATED udIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The distribution's configuration information.
--
-- /Note:/ Consider using 'distributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDistributionConfig :: Lens.Lens' UpdateDistribution DistributionConfig
udDistributionConfig = Lens.lens (distributionConfig :: UpdateDistribution -> DistributionConfig) (\s a -> s {distributionConfig = a} :: UpdateDistribution)
{-# DEPRECATED udDistributionConfig "Use generic-lens or generic-optics with 'distributionConfig' instead." #-}

-- | The distribution's id.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udId :: Lens.Lens' UpdateDistribution Lude.Text
udId = Lens.lens (id :: UpdateDistribution -> Lude.Text) (\s a -> s {id = a} :: UpdateDistribution)
{-# DEPRECATED udId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest UpdateDistribution where
  type Rs UpdateDistribution = UpdateDistributionResponse
  request = Req.putXML cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateDistributionResponse'
            Lude.<$> (h Lude..#? "ETag")
            Lude.<*> (Lude.parseXML x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement UpdateDistribution where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      Lude.. distributionConfig

instance Lude.ToHeaders UpdateDistribution where
  toHeaders UpdateDistribution' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath UpdateDistribution where
  toPath UpdateDistribution' {..} =
    Lude.mconcat
      ["/2020-05-31/distribution/", Lude.toBS id, "/config"]

instance Lude.ToQuery UpdateDistribution where
  toQuery = Lude.const Lude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
    eTag :: Lude.Maybe Lude.Text,
    -- | The distribution's information.
    distribution :: Lude.Maybe Distribution,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDistributionResponse' with the minimum fields required to make a request.
--
-- * 'eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
-- * 'distribution' - The distribution's information.
-- * 'responseStatus' - The response status code.
mkUpdateDistributionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDistributionResponse
mkUpdateDistributionResponse pResponseStatus_ =
  UpdateDistributionResponse'
    { eTag = Lude.Nothing,
      distribution = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsETag :: Lens.Lens' UpdateDistributionResponse (Lude.Maybe Lude.Text)
udrsETag = Lens.lens (eTag :: UpdateDistributionResponse -> Lude.Maybe Lude.Text) (\s a -> s {eTag = a} :: UpdateDistributionResponse)
{-# DEPRECATED udrsETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The distribution's information.
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsDistribution :: Lens.Lens' UpdateDistributionResponse (Lude.Maybe Distribution)
udrsDistribution = Lens.lens (distribution :: UpdateDistributionResponse -> Lude.Maybe Distribution) (\s a -> s {distribution = a} :: UpdateDistributionResponse)
{-# DEPRECATED udrsDistribution "Use generic-lens or generic-optics with 'distribution' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDistributionResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDistributionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDistributionResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
