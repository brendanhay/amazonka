{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteDistribution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a distribution.
module Network.AWS.CloudFront.DeleteDistribution
  ( -- * Creating a request
    DeleteDistribution (..),
    mkDeleteDistribution,

    -- ** Request lenses
    ddIfMatch,
    ddId,

    -- * Destructuring the response
    DeleteDistributionResponse (..),
    mkDeleteDistributionResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This action deletes a web distribution. To delete a web distribution using the CloudFront API, perform the following steps.
--
-- __To delete a web distribution using the CloudFront API:__
--
--     * Disable the web distribution
--
--
--     * Submit a @GET Distribution Config@ request to get the current configuration and the @Etag@ header for the distribution.
--
--
--     * Update the XML document that was returned in the response to your @GET Distribution Config@ request to change the value of @Enabled@ to @false@ .
--
--
--     * Submit a @PUT Distribution Config@ request to update the configuration for your distribution. In the request body, include the XML document that you updated in Step 3. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Distribution Config@ request in Step 2.
--
--
--     * Review the response to the @PUT Distribution Config@ request to confirm that the distribution was successfully disabled.
--
--
--     * Submit a @GET Distribution@ request to confirm that your changes have propagated. When propagation is complete, the value of @Status@ is @Deployed@ .
--
--
--     * Submit a @DELETE Distribution@ request. Set the value of the HTTP @If-Match@ header to the value of the @ETag@ header that CloudFront returned when you submitted the @GET Distribution Config@ request in Step 6.
--
--
--     * Review the response to your @DELETE Distribution@ request to confirm that the distribution was successfully deleted.
--
--
-- For information about deleting a distribution using the CloudFront console, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/HowToDeleteDistribution.html Deleting a Distribution> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkDeleteDistribution' smart constructor.
data DeleteDistribution = DeleteDistribution'
  { -- | The value of the @ETag@ header that you received when you disabled the distribution. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Lude.Maybe Lude.Text,
    -- | The distribution ID.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDistribution' with the minimum fields required to make a request.
--
-- * 'ifMatch' - The value of the @ETag@ header that you received when you disabled the distribution. For example: @E2QWRUHAPOMQZL@ .
-- * 'id' - The distribution ID.
mkDeleteDistribution ::
  -- | 'id'
  Lude.Text ->
  DeleteDistribution
mkDeleteDistribution pId_ =
  DeleteDistribution' {ifMatch = Lude.Nothing, id = pId_}

-- | The value of the @ETag@ header that you received when you disabled the distribution. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddIfMatch :: Lens.Lens' DeleteDistribution (Lude.Maybe Lude.Text)
ddIfMatch = Lens.lens (ifMatch :: DeleteDistribution -> Lude.Maybe Lude.Text) (\s a -> s {ifMatch = a} :: DeleteDistribution)
{-# DEPRECATED ddIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

-- | The distribution ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddId :: Lens.Lens' DeleteDistribution Lude.Text
ddId = Lens.lens (id :: DeleteDistribution -> Lude.Text) (\s a -> s {id = a} :: DeleteDistribution)
{-# DEPRECATED ddId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteDistribution where
  type Rs DeleteDistribution = DeleteDistributionResponse
  request = Req.delete cloudFrontService
  response = Res.receiveNull DeleteDistributionResponse'

instance Lude.ToHeaders DeleteDistribution where
  toHeaders DeleteDistribution' {..} =
    Lude.mconcat ["If-Match" Lude.=# ifMatch]

instance Lude.ToPath DeleteDistribution where
  toPath DeleteDistribution' {..} =
    Lude.mconcat ["/2020-05-31/distribution/", Lude.toBS id]

instance Lude.ToQuery DeleteDistribution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDistributionResponse' smart constructor.
data DeleteDistributionResponse = DeleteDistributionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDistributionResponse' with the minimum fields required to make a request.
mkDeleteDistributionResponse ::
  DeleteDistributionResponse
mkDeleteDistributionResponse = DeleteDistributionResponse'
