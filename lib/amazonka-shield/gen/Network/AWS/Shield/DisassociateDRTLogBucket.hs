{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateDRTLogBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team's (DRT) access to the specified Amazon S3 bucket containing your AWS WAF logs.
--
-- To make a @DisassociateDRTLogBucket@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTLogBucket@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTLogBucket
  ( -- * Creating a request
    DisassociateDRTLogBucket (..),
    mkDisassociateDRTLogBucket,

    -- ** Request lenses
    ddrtlbLogBucket,

    -- * Destructuring the response
    DisassociateDRTLogBucketResponse (..),
    mkDisassociateDRTLogBucketResponse,

    -- ** Response lenses
    ddrtlbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDisassociateDRTLogBucket' smart constructor.
newtype DisassociateDRTLogBucket = DisassociateDRTLogBucket'
  { -- | The Amazon S3 bucket that contains your AWS WAF logs.
    logBucket :: Types.LogBucket
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDRTLogBucket' value with any optional fields omitted.
mkDisassociateDRTLogBucket ::
  -- | 'logBucket'
  Types.LogBucket ->
  DisassociateDRTLogBucket
mkDisassociateDRTLogBucket logBucket =
  DisassociateDRTLogBucket' {logBucket}

-- | The Amazon S3 bucket that contains your AWS WAF logs.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtlbLogBucket :: Lens.Lens' DisassociateDRTLogBucket Types.LogBucket
ddrtlbLogBucket = Lens.field @"logBucket"
{-# DEPRECATED ddrtlbLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

instance Core.FromJSON DisassociateDRTLogBucket where
  toJSON DisassociateDRTLogBucket {..} =
    Core.object
      (Core.catMaybes [Core.Just ("LogBucket" Core..= logBucket)])

instance Core.AWSRequest DisassociateDRTLogBucket where
  type Rs DisassociateDRTLogBucket = DisassociateDRTLogBucketResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSShield_20160616.DisassociateDRTLogBucket")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDRTLogBucketResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateDRTLogBucketResponse' smart constructor.
newtype DisassociateDRTLogBucketResponse = DisassociateDRTLogBucketResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateDRTLogBucketResponse' value with any optional fields omitted.
mkDisassociateDRTLogBucketResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateDRTLogBucketResponse
mkDisassociateDRTLogBucketResponse responseStatus =
  DisassociateDRTLogBucketResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtlbrrsResponseStatus :: Lens.Lens' DisassociateDRTLogBucketResponse Core.Int
ddrtlbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrtlbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
