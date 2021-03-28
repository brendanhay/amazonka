{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified resource.
module Network.AWS.CloudWatch.UntagResource
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

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { resourceARN :: Types.ResourceARN
    -- ^ The ARN of the CloudWatch resource that you're removing tags from.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @ 
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @ 
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
  , tagKeys :: [Types.TagKey]
    -- ^ The list of tag keys to remove from the resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> UntagResource
mkUntagResource resourceARN
  = UntagResource'{resourceARN, tagKeys = Core.mempty}

-- | The ARN of the CloudWatch resource that you're removing tags from.
--
-- The ARN format of an alarm is @arn:aws:cloudwatch:/Region/ :/account-id/ :alarm:/alarm-name/ @ 
-- The ARN format of a Contributor Insights rule is @arn:aws:cloudwatch:/Region/ :/account-id/ :insight-rule:/insight-rule-name/ @ 
-- For more information about ARN format, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazoncloudwatch.html#amazoncloudwatch-resources-for-iam-policies Resource Types Defined by Amazon CloudWatch> in the /Amazon Web Services General Reference/ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urResourceARN :: Lens.Lens' UntagResource Types.ResourceARN
urResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE urResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The list of tag keys to remove from the resource.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Types.TagKey]
urTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE urTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery UntagResource where
        toQuery UntagResource{..}
          = Core.toQueryPair "Action" ("UntagResource" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceARN" resourceARN
              Core.<>
              Core.toQueryPair "TagKeys" (Core.toQueryList "member" tagKeys)

instance Core.ToHeaders UntagResource where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UntagResourceResult"
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
