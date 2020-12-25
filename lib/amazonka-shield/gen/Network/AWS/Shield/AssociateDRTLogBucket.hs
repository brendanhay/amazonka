{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.AssociateDRTLogBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to access the specified Amazon S3 bucket containing your AWS WAF logs. You can associate up to 10 Amazon S3 buckets with your subscription.
--
-- To use the services of the DRT and make an @AssociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> .
module Network.AWS.Shield.AssociateDRTLogBucket
  ( -- * Creating a request
    AssociateDRTLogBucket (..),
    mkAssociateDRTLogBucket,

    -- ** Request lenses
    adrtlbLogBucket,

    -- * Destructuring the response
    AssociateDRTLogBucketResponse (..),
    mkAssociateDRTLogBucketResponse,

    -- ** Response lenses
    adrtlbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkAssociateDRTLogBucket' smart constructor.
newtype AssociateDRTLogBucket = AssociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Types.LogBucket
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDRTLogBucket' value with any optional fields omitted.
mkAssociateDRTLogBucket ::
  -- | 'logBucket'
  Types.LogBucket ->
  AssociateDRTLogBucket
mkAssociateDRTLogBucket logBucket =
  AssociateDRTLogBucket' {logBucket}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtlbLogBucket :: Lens.Lens' AssociateDRTLogBucket Types.LogBucket
adrtlbLogBucket = Lens.field @"logBucket"
{-# DEPRECATED adrtlbLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

instance Core.FromJSON AssociateDRTLogBucket where
  toJSON AssociateDRTLogBucket {..} =
    Core.object
      (Core.catMaybes [Core.Just ("LogBucket" Core..= logBucket)])

instance Core.AWSRequest AssociateDRTLogBucket where
  type Rs AssociateDRTLogBucket = AssociateDRTLogBucketResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.AssociateDRTLogBucket")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDRTLogBucketResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateDRTLogBucketResponse' smart constructor.
newtype AssociateDRTLogBucketResponse = AssociateDRTLogBucketResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateDRTLogBucketResponse' value with any optional fields omitted.
mkAssociateDRTLogBucketResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateDRTLogBucketResponse
mkAssociateDRTLogBucketResponse responseStatus =
  AssociateDRTLogBucketResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrtlbrrsResponseStatus :: Lens.Lens' AssociateDRTLogBucketResponse Core.Int
adrtlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED adrtlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
