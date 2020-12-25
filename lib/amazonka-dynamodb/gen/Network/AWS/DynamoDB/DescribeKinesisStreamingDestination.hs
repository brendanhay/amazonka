{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of Kinesis streaming.
module Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
  ( -- * Creating a request
    DescribeKinesisStreamingDestination (..),
    mkDescribeKinesisStreamingDestination,

    -- ** Request lenses
    dksdfTableName,

    -- * Destructuring the response
    DescribeKinesisStreamingDestinationResponse (..),
    mkDescribeKinesisStreamingDestinationResponse,

    -- ** Response lenses
    dksdrrsKinesisDataStreamDestinations,
    dksdrrsTableName,
    dksdrrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeKinesisStreamingDestination' smart constructor.
newtype DescribeKinesisStreamingDestination = DescribeKinesisStreamingDestination'
  { -- | The name of the table being described.
    tableName :: Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKinesisStreamingDestination' value with any optional fields omitted.
mkDescribeKinesisStreamingDestination ::
  -- | 'tableName'
  Types.TableName ->
  DescribeKinesisStreamingDestination
mkDescribeKinesisStreamingDestination tableName =
  DescribeKinesisStreamingDestination' {tableName}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdfTableName :: Lens.Lens' DescribeKinesisStreamingDestination Types.TableName
dksdfTableName = Lens.field @"tableName"
{-# DEPRECATED dksdfTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON DescribeKinesisStreamingDestination where
  toJSON DescribeKinesisStreamingDestination {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TableName" Core..= tableName)])

instance Core.AWSRequest DescribeKinesisStreamingDestination where
  type
    Rs DescribeKinesisStreamingDestination =
      DescribeKinesisStreamingDestinationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DynamoDB_20120810.DescribeKinesisStreamingDestination"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKinesisStreamingDestinationResponse'
            Core.<$> (x Core..:? "KinesisDataStreamDestinations")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeKinesisStreamingDestinationResponse' smart constructor.
data DescribeKinesisStreamingDestinationResponse = DescribeKinesisStreamingDestinationResponse'
  { -- | The list of replica structures for the table being described.
    kinesisDataStreamDestinations :: Core.Maybe [Types.KinesisDataStreamDestination],
    -- | The name of the table being described.
    tableName :: Core.Maybe Types.TableName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKinesisStreamingDestinationResponse' value with any optional fields omitted.
mkDescribeKinesisStreamingDestinationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeKinesisStreamingDestinationResponse
mkDescribeKinesisStreamingDestinationResponse responseStatus =
  DescribeKinesisStreamingDestinationResponse'
    { kinesisDataStreamDestinations =
        Core.Nothing,
      tableName = Core.Nothing,
      responseStatus
    }

-- | The list of replica structures for the table being described.
--
-- /Note:/ Consider using 'kinesisDataStreamDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrrsKinesisDataStreamDestinations :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Core.Maybe [Types.KinesisDataStreamDestination])
dksdrrsKinesisDataStreamDestinations = Lens.field @"kinesisDataStreamDestinations"
{-# DEPRECATED dksdrrsKinesisDataStreamDestinations "Use generic-lens or generic-optics with 'kinesisDataStreamDestinations' instead." #-}

-- | The name of the table being described.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrrsTableName :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Core.Maybe Types.TableName)
dksdrrsTableName = Lens.field @"tableName"
{-# DEPRECATED dksdrrsTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdrrsResponseStatus :: Lens.Lens' DescribeKinesisStreamingDestinationResponse Core.Int
dksdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dksdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
