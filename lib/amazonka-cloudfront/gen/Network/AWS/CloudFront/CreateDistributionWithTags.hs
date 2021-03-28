{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateDistributionWithTags (..)
    , mkCreateDistributionWithTags
    -- ** Request lenses
    , cdwtDistributionConfigWithTags

    -- * Destructuring the response
    , CreateDistributionWithTagsResponse (..)
    , mkCreateDistributionWithTagsResponse
    -- ** Response lenses
    , cdwtrrsDistribution
    , cdwtrrsETag
    , cdwtrrsLocation
    , cdwtrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution with tags. 
--
-- /See:/ 'mkCreateDistributionWithTags' smart constructor.
newtype CreateDistributionWithTags = CreateDistributionWithTags'
  { distributionConfigWithTags :: Types.DistributionConfigWithTags
    -- ^ The distribution's configuration information. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDistributionWithTags' value with any optional fields omitted.
mkCreateDistributionWithTags
    :: Types.DistributionConfigWithTags -- ^ 'distributionConfigWithTags'
    -> CreateDistributionWithTags
mkCreateDistributionWithTags distributionConfigWithTags
  = CreateDistributionWithTags'{distributionConfigWithTags}

-- | The distribution's configuration information. 
--
-- /Note:/ Consider using 'distributionConfigWithTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtDistributionConfigWithTags :: Lens.Lens' CreateDistributionWithTags Types.DistributionConfigWithTags
cdwtDistributionConfigWithTags = Lens.field @"distributionConfigWithTags"
{-# INLINEABLE cdwtDistributionConfigWithTags #-}
{-# DEPRECATED distributionConfigWithTags "Use generic-lens or generic-optics with 'distributionConfigWithTags' instead"  #-}

instance Core.ToQuery CreateDistributionWithTags where
        toQuery CreateDistributionWithTags{..}
          = Core.toQueryPair "WithTags" ("" :: Core.Text)

instance Core.ToHeaders CreateDistributionWithTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDistributionWithTags where
        type Rs CreateDistributionWithTags =
             CreateDistributionWithTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-05-31/distribution",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateDistributionWithTagsResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.parseHeaderMaybe "ETag" h Core.<*>
                     Core.parseHeaderMaybe "Location" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The returned result of the corresponding request. 
--
-- /See:/ 'mkCreateDistributionWithTagsResponse' smart constructor.
data CreateDistributionWithTagsResponse = CreateDistributionWithTagsResponse'
  { distribution :: Core.Maybe Types.Distribution
    -- ^ The distribution's information. 
  , eTag :: Core.Maybe Core.Text
    -- ^ The current version of the distribution created.
  , location :: Core.Maybe Core.Text
    -- ^ The fully qualified URI of the new distribution resource just created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDistributionWithTagsResponse' value with any optional fields omitted.
mkCreateDistributionWithTagsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDistributionWithTagsResponse
mkCreateDistributionWithTagsResponse responseStatus
  = CreateDistributionWithTagsResponse'{distribution = Core.Nothing,
                                        eTag = Core.Nothing, location = Core.Nothing,
                                        responseStatus}

-- | The distribution's information. 
--
-- /Note:/ Consider using 'distribution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsDistribution :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Types.Distribution)
cdwtrrsDistribution = Lens.field @"distribution"
{-# INLINEABLE cdwtrrsDistribution #-}
{-# DEPRECATED distribution "Use generic-lens or generic-optics with 'distribution' instead"  #-}

-- | The current version of the distribution created.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsETag :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Core.Text)
cdwtrrsETag = Lens.field @"eTag"
{-# INLINEABLE cdwtrrsETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | The fully qualified URI of the new distribution resource just created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsLocation :: Lens.Lens' CreateDistributionWithTagsResponse (Core.Maybe Core.Text)
cdwtrrsLocation = Lens.field @"location"
{-# INLINEABLE cdwtrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdwtrrsResponseStatus :: Lens.Lens' CreateDistributionWithTagsResponse Core.Int
cdwtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdwtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
