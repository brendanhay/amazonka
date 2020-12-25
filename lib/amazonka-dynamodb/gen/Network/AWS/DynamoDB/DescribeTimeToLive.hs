{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gives a description of the Time to Live (TTL) status on the specified table.
module Network.AWS.DynamoDB.DescribeTimeToLive
  ( -- * Creating a request
    DescribeTimeToLive (..),
    mkDescribeTimeToLive,

    -- ** Request lenses
    dttlTableName,

    -- * Destructuring the response
    DescribeTimeToLiveResponse (..),
    mkDescribeTimeToLiveResponse,

    -- ** Response lenses
    dttlrrsTimeToLiveDescription,
    dttlrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeTimeToLive' smart constructor.
newtype DescribeTimeToLive = DescribeTimeToLive'
  { -- | The name of the table to be described.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTimeToLive' value with any optional fields omitted.
mkDescribeTimeToLive ::
  -- | 'tableName'
  Types.TableName ->
  DescribeTimeToLive
mkDescribeTimeToLive tableName = DescribeTimeToLive' {tableName}

-- | The name of the table to be described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlTableName :: Lens.Lens' DescribeTimeToLive Types.TableName
dttlTableName = Lens.field @"tableName"
{-# DEPRECATED dttlTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON DescribeTimeToLive where
  toJSON DescribeTimeToLive {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest DescribeTimeToLive where
  type Rs DescribeTimeToLive = DescribeTimeToLiveResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeTimeToLive")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeToLiveResponse'
            Core.<$> (x Core..:? "TimeToLiveDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTimeToLiveResponse' smart constructor.
data DescribeTimeToLiveResponse = DescribeTimeToLiveResponse'
  { -- |
    timeToLiveDescription :: Core.Maybe Types.TimeToLiveDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeTimeToLiveResponse' value with any optional fields omitted.
mkDescribeTimeToLiveResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTimeToLiveResponse
mkDescribeTimeToLiveResponse responseStatus =
  DescribeTimeToLiveResponse'
    { timeToLiveDescription = Core.Nothing,
      responseStatus
    }

-- |
--
-- /Note:/ Consider using 'timeToLiveDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlrrsTimeToLiveDescription :: Lens.Lens' DescribeTimeToLiveResponse (Core.Maybe Types.TimeToLiveDescription)
dttlrrsTimeToLiveDescription = Lens.field @"timeToLiveDescription"
{-# DEPRECATED dttlrrsTimeToLiveDescription "Use generic-lens or generic-optics with 'timeToLiveDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttlrrsResponseStatus :: Lens.Lens' DescribeTimeToLiveResponse Core.Int
dttlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dttlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
