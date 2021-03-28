{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a Kinesis Analytics application. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging> .
module Network.AWS.KinesisAnalytics.UntagResource
    (
    -- * Creating a request
      UntagResource (..)
    , mkUntagResource
    -- ** Request lenses
    , urResourceARN
    , urTagKeys

    -- * Destructuring the response
    , UntagResourceResponse (..)
    , mkUntagResourceResponse
    -- ** Response lenses
    , urrrsResponseStatus
    ) where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { resourceARN :: Types.KinesisAnalyticsARN
    -- ^ The ARN of the Kinesis Analytics application from which to remove the tags.
  , tagKeys :: Core.NonEmpty Types.TagKey
    -- ^ A list of keys of tags to remove from the specified application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource
    :: Types.KinesisAnalyticsARN -- ^ 'resourceARN'
    -> Core.NonEmpty Types.TagKey -- ^ 'tagKeys'
    -> UntagResource
mkUntagResource resourceARN tagKeys
  = UntagResource'{resourceARN, tagKeys}

-- | The ARN of the Kinesis Analytics application from which to remove the tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource Types.KinesisAnalyticsARN
urResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE urResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | A list of keys of tags to remove from the specified application.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource (Core.NonEmpty Types.TagKey)
urTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE urTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UntagResource where
        toHeaders UntagResource{..}
          = Core.pure
              ("X-Amz-Target", "KinesisAnalytics_20150814.UntagResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UntagResource where
        toJSON UntagResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UntagResourceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
newtype UntagResourceResponse = UntagResourceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourceResponse' value with any optional fields omitted.
mkUntagResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UntagResourceResponse
mkUntagResourceResponse responseStatus
  = UntagResourceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UntagResourceResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
