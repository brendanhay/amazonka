{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStreamingDistributionWithTags (..)
    , mkCreateStreamingDistributionWithTags
    -- ** Request lenses
    , csdwtStreamingDistributionConfigWithTags

    -- * Destructuring the response
    , CreateStreamingDistributionWithTagsResponse (..)
    , mkCreateStreamingDistributionWithTagsResponse
    -- ** Response lenses
    , csdwtrrsETag
    , csdwtrrsLocation
    , csdwtrrsStreamingDistribution
    , csdwtrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new streaming distribution with tags.
--
-- /See:/ 'mkCreateStreamingDistributionWithTags' smart constructor.
newtype CreateStreamingDistributionWithTags = CreateStreamingDistributionWithTags'
  { streamingDistributionConfigWithTags :: Types.StreamingDistributionConfigWithTags
    -- ^ The streaming distribution's configuration information. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStreamingDistributionWithTags' value with any optional fields omitted.
mkCreateStreamingDistributionWithTags
    :: Types.StreamingDistributionConfigWithTags -- ^ 'streamingDistributionConfigWithTags'
    -> CreateStreamingDistributionWithTags
mkCreateStreamingDistributionWithTags
  streamingDistributionConfigWithTags
  = CreateStreamingDistributionWithTags'{streamingDistributionConfigWithTags}

-- | The streaming distribution's configuration information. 
--
-- /Note:/ Consider using 'streamingDistributionConfigWithTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtStreamingDistributionConfigWithTags :: Lens.Lens' CreateStreamingDistributionWithTags Types.StreamingDistributionConfigWithTags
csdwtStreamingDistributionConfigWithTags = Lens.field @"streamingDistributionConfigWithTags"
{-# INLINEABLE csdwtStreamingDistributionConfigWithTags #-}
{-# DEPRECATED streamingDistributionConfigWithTags "Use generic-lens or generic-optics with 'streamingDistributionConfigWithTags' instead"  #-}

instance Core.ToQuery CreateStreamingDistributionWithTags where
        toQuery CreateStreamingDistributionWithTags{..}
          = Core.toQueryPair "WithTags" ("" :: Core.Text)

instance Core.ToHeaders CreateStreamingDistributionWithTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateStreamingDistributionWithTags where
        type Rs CreateStreamingDistributionWithTags =
             CreateStreamingDistributionWithTagsResponse
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
                 CreateStreamingDistributionWithTagsResponse' Core.<$>
                   (Core.parseHeaderMaybe "ETag" h) Core.<*>
                     Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.parseXML x
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request. 
--
-- /See:/ 'mkCreateStreamingDistributionWithTagsResponse' smart constructor.
data CreateStreamingDistributionWithTagsResponse = CreateStreamingDistributionWithTagsResponse'
  { eTag :: Core.Maybe Core.Text
    -- ^ The current version of the distribution created.
  , location :: Core.Maybe Core.Text
    -- ^ The fully qualified URI of the new streaming distribution resource just created.
  , streamingDistribution :: Core.Maybe Types.StreamingDistribution
    -- ^ The streaming distribution's information. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateStreamingDistributionWithTagsResponse' value with any optional fields omitted.
mkCreateStreamingDistributionWithTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStreamingDistributionWithTagsResponse
mkCreateStreamingDistributionWithTagsResponse responseStatus
  = CreateStreamingDistributionWithTagsResponse'{eTag = Core.Nothing,
                                                 location = Core.Nothing,
                                                 streamingDistribution = Core.Nothing,
                                                 responseStatus}

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrrsETag :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Core.Maybe Core.Text)
csdwtrrsETag = Lens.field @"eTag"
{-# INLINEABLE csdwtrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The fully qualified URI of the new streaming distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrrsLocation :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Core.Maybe Core.Text)
csdwtrrsLocation = Lens.field @"location"
{-# INLINEABLE csdwtrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The streaming distribution's information. 
--
-- /Note:/ Consider using 'streamingDistribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrrsStreamingDistribution :: Lens.Lens' CreateStreamingDistributionWithTagsResponse (Core.Maybe Types.StreamingDistribution)
csdwtrrsStreamingDistribution = Lens.field @"streamingDistribution"
{-# INLINEABLE csdwtrrsStreamingDistribution #-}
{-# DEPRECATED streamingDistribution "Use generic-lens or generic-optics with 'streamingDistribution' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdwtrrsResponseStatus :: Lens.Lens' CreateStreamingDistributionWithTagsResponse Core.Int
csdwtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csdwtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
