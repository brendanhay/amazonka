{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStreamingDistribution (..)
    , mkCreateStreamingDistribution
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Destructuring the response
    , CreateStreamingDistributionResponse (..)
    , mkCreateStreamingDistributionResponse
    -- ** Response lenses
    , csdrrsETag
    , csdrrsLocation
    , csdrrsStreamingDistribution
    , csdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new streaming distribution.
--
-- /See:/ 'mkCreateStreamingDistribution' smart constructor.
newtype CreateStreamingDistribution = CreateStreamingDistribution'
  { streamingDistributionConfig :: Types.StreamingDistributionConfig
    -- ^ The streaming distribution's configuration information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamingDistribution' value with any optional fields omitted.
mkCreateStreamingDistribution
    :: Types.StreamingDistributionConfig -- ^ 'streamingDistributionConfig'
    -> CreateStreamingDistribution
mkCreateStreamingDistribution streamingDistributionConfig
  = CreateStreamingDistribution'{streamingDistributionConfig}

-- | The streaming distribution's configuration information.
--
-- /Note:/ Consider using 'streamingDistributionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdStreamingDistributionConfig :: Lens.Lens' CreateStreamingDistribution Types.StreamingDistributionConfig
csdStreamingDistributionConfig = Lens.field @"streamingDistributionConfig"
{-# INLINEABLE csdStreamingDistributionConfig #-}
{-# DEPRECATED streamingDistributionConfig "Use generic-lens or generic-optics with 'streamingDistributionConfig' instead"  #-}

instance Core.ToQuery CreateStreamingDistribution where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStreamingDistribution where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateStreamingDistribution where
        type Rs CreateStreamingDistribution =
             CreateStreamingDistributionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-05-31/streaming-distribution",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateStreamingDistributionResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*>
                     Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.parseXML x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkCreateStreamingDistributionResponse' smart constructor.
data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the streaming distribution created.
  , location :: Core.Maybe Core.Text
    -- ^ The fully qualified URI of the new streaming distribution resource just created.
  , streamingDistribution :: Core.Maybe Types.StreamingDistribution
    -- ^ The streaming distribution's information.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateStreamingDistributionResponse' value with any optional fields omitted.
mkCreateStreamingDistributionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStreamingDistributionResponse
mkCreateStreamingDistributionResponse responseStatus
  = CreateStreamingDistributionResponse'{eTag = Core.Nothing,
                                         location = Core.Nothing,
                                         streamingDistribution = Core.Nothing, responseStatus}

-- | The current version of the streaming distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsETag :: Lens.Lens' CreateStreamingDistributionResponse (Core.Maybe Core.Text)
csdrrsETag = Lens.field @"eTag"
{-# INLINEABLE csdrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The fully qualified URI of the new streaming distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsLocation :: Lens.Lens' CreateStreamingDistributionResponse (Core.Maybe Core.Text)
csdrrsLocation = Lens.field @"location"
{-# INLINEABLE csdrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The streaming distribution's information.
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsStreamingDistribution :: Lens.Lens' CreateStreamingDistributionResponse (Core.Maybe Types.StreamingDistribution)
csdrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# INLINEABLE csdrrsStreamingDistribution #-}
{-# DEPRECATED streamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsResponseStatus :: Lens.Lens' CreateStreamingDistributionResponse Core.Int
csdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
