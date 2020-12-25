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
    dsdId,
    dsdIfMatch,

    -- * Destructuring the response
    DeleteStreamingDistributionResponse (..),
    mkDeleteStreamingDistributionResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to delete a streaming distribution.
--
-- /See:/ 'mkDeleteStreamingDistribution' smart constructor.
data DeleteStreamingDistribution = DeleteStreamingDistribution'
  { -- | The distribution ID.
    id :: Types.String,
    -- | The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamingDistribution' value with any optional fields omitted.
mkDeleteStreamingDistribution ::
  -- | 'id'
  Types.String ->
  DeleteStreamingDistribution
mkDeleteStreamingDistribution id =
  DeleteStreamingDistribution' {id, ifMatch = Core.Nothing}

-- | The distribution ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdId :: Lens.Lens' DeleteStreamingDistribution Types.String
dsdId = Lens.field @"id"
{-# DEPRECATED dsdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value of the @ETag@ header that you received when you disabled the streaming distribution. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdIfMatch :: Lens.Lens' DeleteStreamingDistribution (Core.Maybe Types.String)
dsdIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED dsdIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest DeleteStreamingDistribution where
  type
    Rs DeleteStreamingDistribution =
      DeleteStreamingDistributionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/2020-05-31/streaming-distribution/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = ""
      }
  response =
    Response.receiveNull DeleteStreamingDistributionResponse'

-- | /See:/ 'mkDeleteStreamingDistributionResponse' smart constructor.
data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteStreamingDistributionResponse' value with any optional fields omitted.
mkDeleteStreamingDistributionResponse ::
  DeleteStreamingDistributionResponse
mkDeleteStreamingDistributionResponse =
  DeleteStreamingDistributionResponse'
