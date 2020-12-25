{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopActivityStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a database activity stream that was started using the AWS console, the @start-activity-stream@ AWS CLI command, or the @StartActivityStream@ action.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.StopActivityStream
  ( -- * Creating a request
    StopActivityStream (..),
    mkStopActivityStream,

    -- ** Request lenses
    sasResourceArn,
    sasApplyImmediately,

    -- * Destructuring the response
    StopActivityStreamResponse (..),
    mkStopActivityStreamResponse,

    -- ** Response lenses
    sasrrsKinesisStreamName,
    sasrrsKmsKeyId,
    sasrrsStatus,
    sasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopActivityStream' smart constructor.
data StopActivityStream = StopActivityStream'
  { -- | The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
    resourceArn :: Types.String,
    -- | Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopActivityStream' value with any optional fields omitted.
mkStopActivityStream ::
  -- | 'resourceArn'
  Types.String ->
  StopActivityStream
mkStopActivityStream resourceArn =
  StopActivityStream' {resourceArn, applyImmediately = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasResourceArn :: Lens.Lens' StopActivityStream Types.String
sasResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED sasResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasApplyImmediately :: Lens.Lens' StopActivityStream (Core.Maybe Core.Bool)
sasApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED sasApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Core.AWSRequest StopActivityStream where
  type Rs StopActivityStream = StopActivityStreamResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "StopActivityStream")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "ResourceArn" resourceArn)
                Core.<> (Core.toQueryValue "ApplyImmediately" Core.<$> applyImmediately)
            )
      }
  response =
    Response.receiveXMLWrapper
      "StopActivityStreamResult"
      ( \s h x ->
          StopActivityStreamResponse'
            Core.<$> (x Core..@? "KinesisStreamName")
            Core.<*> (x Core..@? "KmsKeyId")
            Core.<*> (x Core..@? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopActivityStreamResponse' smart constructor.
data StopActivityStreamResponse = StopActivityStreamResponse'
  { -- | The name of the Amazon Kinesis data stream used for the database activity stream.
    kinesisStreamName :: Core.Maybe Types.KinesisStreamName,
    -- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The status of the database activity stream.
    status :: Core.Maybe Types.ActivityStreamStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopActivityStreamResponse' value with any optional fields omitted.
mkStopActivityStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopActivityStreamResponse
mkStopActivityStreamResponse responseStatus =
  StopActivityStreamResponse'
    { kinesisStreamName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | The name of the Amazon Kinesis data stream used for the database activity stream.
--
-- /Note:/ Consider using 'kinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrrsKinesisStreamName :: Lens.Lens' StopActivityStreamResponse (Core.Maybe Types.KinesisStreamName)
sasrrsKinesisStreamName = Lens.field @"kinesisStreamName"
{-# DEPRECATED sasrrsKinesisStreamName "Use generic-lens or generic-optics with 'kinesisStreamName' instead." #-}

-- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrrsKmsKeyId :: Lens.Lens' StopActivityStreamResponse (Core.Maybe Types.KmsKeyId)
sasrrsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED sasrrsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrrsStatus :: Lens.Lens' StopActivityStreamResponse (Core.Maybe Types.ActivityStreamStatus)
sasrrsStatus = Lens.field @"status"
{-# DEPRECATED sasrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrrsResponseStatus :: Lens.Lens' StopActivityStreamResponse Core.Int
sasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
