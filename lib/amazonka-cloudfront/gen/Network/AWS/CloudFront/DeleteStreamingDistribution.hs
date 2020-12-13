{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a streaming distribution. To delete an RTMP distribution using the CloudFront API, perform the following steps.
--
-- __To delete an RTMP distribution using the CloudFront API__ :
--
--     * Disable the RTMP distribution.
--
--
--     * Submit a @GET Streaming Distribution Config@ request to get the current configuration and the @Etag@ header for the distribution.
--
--
--     * Update the XML document that was returned in the response to your @GET Streaming Distribution Config@ request to change the value of @Enabled@ to @false@ .
--
--
--     * Submit a @PUT Streaming Distribution Config@ request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Then set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Streaming Distribution Config@ request in Step 2.
--
--
--     * Review the response to the @PUT Streaming Distribution Config@ request to confirm that the distribution was successfully disabled.
--
--
--     * Submit a @GET Streaming Distribution Config@ request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
--
--
--     * Submit a @DELETE Streaming Distribution@ request. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Streaming Distribution Config@ request in Step 2.
--
--
--     * Review the response to your @DELETE Streaming Distribution@ request to confirm that the distribution was successfully deleted.
--
--
-- For information about deleting a distribution using the CloudFront console, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.DeleteStreamingDistribution
  ( -- * Creating a request
    DeleteStreamingDistribution (..),
    mkDeleteStreamingDistribution,

    -- ** Request lenses
    dsdIfMatch,
    dsdId,

    -- * Destructuring the response
    DeleteStreamingDistributionResponse (..),
    mkDeleteStreamingDistributionResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to delete a streaming distribution.
--
-- /See:/ 'mkDeleteStreamingDistribution' smart constructor.
data DeleteStreamingDistribution = DeleteStreamingDistribution'
  { -- | The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The distribution ID.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStreamingDistribution' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
-- * 'id' - The distribution ID.
mkDeleteStreamingDistribution ::
  -- | 'id'
  Lude.Text ->
  DeleteStreamingDistribution
mkDeleteStreamingDistribution pId_ =
  DeleteStreamingDistribution' {ifMatch = Lude.Nothing, id = pId_}

-- | The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdIfMatch :: Lens.Lens' DeleteStreamingDistribution (Lude.Maybe Lude.Text)
dsdIfMatch = Lens.lens (ifMatch :: DeleteStreamingDistribution -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteStreamingDistribution)
{-# DEPRECATED dsdIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The distribution ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdId :: Lens.Lens' DeleteStreamingDistribution Lude.Text
dsdId = Lens.lens (id :: DeleteStreamingDistribution -> Lude.Text) (\s a -> s {id = a} :: DeleteStreamingDistribution)
{-# DEPRECATED dsdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteStreamingDistribution where
  type
    Rs DeleteStreamingDistribution =
      DeleteStreamingDistributionResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeleteStreamingDistributionResponse'

instance Lude.ToHeaders DeleteStreamingDistribution where
  toHeaders DeleteStreamingDistribution' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteStreamingDistribution where
  toPath DeleteStreamingDistribution' {..} =
    Lude.mconcat
      ["/2020-05-31/streaming-distribution/", Lude.toBS id]

instance Lude.ToQuery DeleteStreamingDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStreamingDistributionResponse' with the minimum fields required to make a request.
mkDeleteStreamingDistributionResponse ::
  DeleteStreamingDistributionResponse
mkDeleteStreamingDistributionResponse =
  DeleteStreamingDistributionResponse'
