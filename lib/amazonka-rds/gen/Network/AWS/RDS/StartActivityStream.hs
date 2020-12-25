{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartActivityStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a database activity stream to monitor activity on the database. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/DBActivityStreams.html Database Activity Streams> in the /Amazon Aurora User Guide/ .
module Network.AWS.RDS.StartActivityStream
  ( -- * Creating a request
    StartActivityStream (..),
    mkStartActivityStream,

    -- ** Request lenses
    sResourceArn,
    sMode,
    sKmsKeyId,
    sApplyImmediately,

    -- * Destructuring the response
    StartActivityStreamResponse (..),
    mkStartActivityStreamResponse,

    -- ** Response lenses
    srsApplyImmediately,
    srsKinesisStreamName,
    srsKmsKeyId,
    srsMode,
    srsStatus,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartActivityStream' smart constructor.
data StartActivityStream = StartActivityStream'
  { -- | The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
    resourceArn :: Types.String,
    -- | Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
    mode :: Types.ActivityStreamMode,
    -- | The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
    kmsKeyId :: Types.String,
    -- | Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartActivityStream' value with any optional fields omitted.
mkStartActivityStream ::
  -- | 'resourceArn'
  Types.String ->
  -- | 'mode'
  Types.ActivityStreamMode ->
  -- | 'kmsKeyId'
  Types.String ->
  StartActivityStream
mkStartActivityStream resourceArn mode kmsKeyId =
  StartActivityStream'
    { resourceArn,
      mode,
      kmsKeyId,
      applyImmediately = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceArn :: Lens.Lens' StartActivityStream Types.String
sResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED sResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

-- | Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMode :: Lens.Lens' StartActivityStream Types.ActivityStreamMode
sMode = Lens.field @"mode"
{-# DEPRECATED sMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' StartActivityStream Types.String
sKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED sKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplyImmediately :: Lens.Lens' StartActivityStream (Core.Maybe Core.Bool)
sApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED sApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Core.AWSRequest StartActivityStream where
  type Rs StartActivityStream = StartActivityStreamResponse
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
            ( Core.pure ("Action", "StartActivityStream")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "ResourceArn" resourceArn)
                Core.<> (Core.toQueryValue "Mode" mode)
                Core.<> (Core.toQueryValue "KmsKeyId" kmsKeyId)
                Core.<> (Core.toQueryValue "ApplyImmediately" Core.<$> applyImmediately)
            )
      }
  response =
    Response.receiveXMLWrapper
      "StartActivityStreamResult"
      ( \s h x ->
          StartActivityStreamResponse'
            Core.<$> (x Core..@? "ApplyImmediately")
            Core.<*> (x Core..@? "KinesisStreamName")
            Core.<*> (x Core..@? "KmsKeyId")
            Core.<*> (x Core..@? "Mode")
            Core.<*> (x Core..@? "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartActivityStreamResponse' smart constructor.
data StartActivityStreamResponse = StartActivityStreamResponse'
  { -- | Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Core.Maybe Core.Bool,
    -- | The name of the Amazon Kinesis data stream to be used for the database activity stream.
    kinesisStreamName :: Core.Maybe Types.String,
    -- | The AWS KMS key identifier for encryption of messages in the database activity stream.
    kmsKeyId :: Core.Maybe Types.String,
    -- | The mode of the database activity stream.
    mode :: Core.Maybe Types.ActivityStreamMode,
    -- | The status of the database activity stream.
    status :: Core.Maybe Types.ActivityStreamStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartActivityStreamResponse' value with any optional fields omitted.
mkStartActivityStreamResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartActivityStreamResponse
mkStartActivityStreamResponse responseStatus =
  StartActivityStreamResponse'
    { applyImmediately = Core.Nothing,
      kinesisStreamName = Core.Nothing,
      kmsKeyId = Core.Nothing,
      mode = Core.Nothing,
      status = Core.Nothing,
      responseStatus
    }

-- | Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsApplyImmediately :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Bool)
srsApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED srsApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The name of the Amazon Kinesis data stream to be used for the database activity stream.
--
-- /Note:/ Consider using 'kinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKinesisStreamName :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.String)
srsKinesisStreamName = Lens.field @"kinesisStreamName"
{-# DEPRECATED srsKinesisStreamName "Use generic-lens or generic-optics with 'kinesisStreamName' instead." #-}

-- | The AWS KMS key identifier for encryption of messages in the database activity stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKmsKeyId :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.String)
srsKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED srsKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The mode of the database activity stream.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsMode :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.ActivityStreamMode)
srsMode = Lens.field @"mode"
{-# DEPRECATED srsMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStatus :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.ActivityStreamStatus)
srsStatus = Lens.field @"status"
{-# DEPRECATED srsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartActivityStreamResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
