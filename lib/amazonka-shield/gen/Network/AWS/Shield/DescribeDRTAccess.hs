{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeDRTAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current role and list of Amazon S3 log buckets used by the DDoS Response Team (DRT) to access your AWS account while assisting with attack mitigation.
module Network.AWS.Shield.DescribeDRTAccess
  ( -- * Creating a request
    DescribeDRTAccess (..),
    mkDescribeDRTAccess,

    -- * Destructuring the response
    DescribeDRTAccessResponse (..),
    mkDescribeDRTAccessResponse,

    -- ** Response lenses
    ddrtarrsLogBucketList,
    ddrtarrsRoleArn,
    ddrtarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeDRTAccess' smart constructor.
data DescribeDRTAccess = DescribeDRTAccess'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDRTAccess' value with any optional fields omitted.
mkDescribeDRTAccess ::
  DescribeDRTAccess
mkDescribeDRTAccess = DescribeDRTAccess'

instance Core.FromJSON DescribeDRTAccess where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeDRTAccess where
  type Rs DescribeDRTAccess = DescribeDRTAccessResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.DescribeDRTAccess")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDRTAccessResponse'
            Core.<$> (x Core..:? "LogBucketList")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDRTAccessResponse' smart constructor.
data DescribeDRTAccessResponse = DescribeDRTAccessResponse'
  { -- | The list of Amazon S3 buckets accessed by the DRT.
    logBucketList :: Core.Maybe [Types.LogBucket],
    -- | The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDRTAccessResponse' value with any optional fields omitted.
mkDescribeDRTAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDRTAccessResponse
mkDescribeDRTAccessResponse responseStatus =
  DescribeDRTAccessResponse'
    { logBucketList = Core.Nothing,
      roleArn = Core.Nothing,
      responseStatus
    }

-- | The list of Amazon S3 buckets accessed by the DRT.
--
-- /Note:/ Consider using 'logBucketList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarrsLogBucketList :: Lens.Lens' DescribeDRTAccessResponse (Core.Maybe [Types.LogBucket])
ddrtarrsLogBucketList = Lens.field @"logBucketList"
{-# DEPRECATED ddrtarrsLogBucketList "Use generic-lens or generic-optics with 'logBucketList' instead." #-}

-- | The Amazon Resource Name (ARN) of the role the DRT used to access your AWS account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarrsRoleArn :: Lens.Lens' DescribeDRTAccessResponse (Core.Maybe Types.RoleArn)
ddrtarrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED ddrtarrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtarrsResponseStatus :: Lens.Lens' DescribeDRTAccessResponse Core.Int
ddrtarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
