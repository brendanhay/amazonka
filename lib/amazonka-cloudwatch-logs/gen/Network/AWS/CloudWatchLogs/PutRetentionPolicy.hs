{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
  ( -- * Creating a request
    PutRetentionPolicy (..),
    mkPutRetentionPolicy,

    -- ** Request lenses
    prpLogGroupName,
    prpRetentionInDays,

    -- * Destructuring the response
    PutRetentionPolicyResponse (..),
    mkPutRetentionPolicyResponse,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutRetentionPolicy' smart constructor.
data PutRetentionPolicy = PutRetentionPolicy'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    retentionInDays :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicy' value with any optional fields omitted.
mkPutRetentionPolicy ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'retentionInDays'
  Core.Int ->
  PutRetentionPolicy
mkPutRetentionPolicy logGroupName retentionInDays =
  PutRetentionPolicy' {logGroupName, retentionInDays}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpLogGroupName :: Lens.Lens' PutRetentionPolicy Types.LogGroupName
prpLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED prpLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'retentionInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpRetentionInDays :: Lens.Lens' PutRetentionPolicy Core.Int
prpRetentionInDays = Lens.field @"retentionInDays"
{-# DEPRECATED prpRetentionInDays "Use generic-lens or generic-optics with 'retentionInDays' instead." #-}

instance Core.FromJSON PutRetentionPolicy where
  toJSON PutRetentionPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("retentionInDays" Core..= retentionInDays)
          ]
      )

instance Core.AWSRequest PutRetentionPolicy where
  type Rs PutRetentionPolicy = PutRetentionPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.PutRetentionPolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull PutRetentionPolicyResponse'

-- | /See:/ 'mkPutRetentionPolicyResponse' smart constructor.
data PutRetentionPolicyResponse = PutRetentionPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutRetentionPolicyResponse' value with any optional fields omitted.
mkPutRetentionPolicyResponse ::
  PutRetentionPolicyResponse
mkPutRetentionPolicyResponse = PutRetentionPolicyResponse'
