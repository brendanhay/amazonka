{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartActivityStream (..)
    , mkStartActivityStream
    -- ** Request lenses
    , sResourceArn
    , sMode
    , sKmsKeyId
    , sApplyImmediately

    -- * Destructuring the response
    , StartActivityStreamResponse (..)
    , mkStartActivityStreamResponse
    -- ** Response lenses
    , srsApplyImmediately
    , srsKinesisStreamName
    , srsKmsKeyId
    , srsMode
    , srsStatus
    , srsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartActivityStream' smart constructor.
data StartActivityStream = StartActivityStream'
  { resourceArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
  , mode :: Types.ActivityStreamMode
    -- ^ Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously. 
  , kmsKeyId :: Core.Text
    -- ^ The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
  , applyImmediately :: Core.Maybe Core.Bool
    -- ^ Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartActivityStream' value with any optional fields omitted.
mkStartActivityStream
    :: Core.Text -- ^ 'resourceArn'
    -> Types.ActivityStreamMode -- ^ 'mode'
    -> Core.Text -- ^ 'kmsKeyId'
    -> StartActivityStream
mkStartActivityStream resourceArn mode kmsKeyId
  = StartActivityStream'{resourceArn, mode, kmsKeyId,
                         applyImmediately = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceArn :: Lens.Lens' StartActivityStream Core.Text
sResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE sResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously. 
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMode :: Lens.Lens' StartActivityStream Types.ActivityStreamMode
sMode = Lens.field @"mode"
{-# INLINEABLE sMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKmsKeyId :: Lens.Lens' StartActivityStream Core.Text
sKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE sKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplyImmediately :: Lens.Lens' StartActivityStream (Core.Maybe Core.Bool)
sApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE sApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

instance Core.ToQuery StartActivityStream where
        toQuery StartActivityStream{..}
          = Core.toQueryPair "Action" ("StartActivityStream" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceArn" resourceArn
              Core.<> Core.toQueryPair "Mode" mode
              Core.<> Core.toQueryPair "KmsKeyId" kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyImmediately")
                applyImmediately

instance Core.ToHeaders StartActivityStream where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest StartActivityStream where
        type Rs StartActivityStream = StartActivityStreamResponse
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
          = Response.receiveXMLWrapper "StartActivityStreamResult"
              (\ s h x ->
                 StartActivityStreamResponse' Core.<$>
                   (x Core..@? "ApplyImmediately") Core.<*>
                     x Core..@? "KinesisStreamName"
                     Core.<*> x Core..@? "KmsKeyId"
                     Core.<*> x Core..@? "Mode"
                     Core.<*> x Core..@? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartActivityStreamResponse' smart constructor.
data StartActivityStreamResponse = StartActivityStreamResponse'
  { applyImmediately :: Core.Maybe Core.Bool
    -- ^ Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
  , kinesisStreamName :: Core.Maybe Core.Text
    -- ^ The name of the Amazon Kinesis data stream to be used for the database activity stream.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier for encryption of messages in the database activity stream.
  , mode :: Core.Maybe Types.ActivityStreamMode
    -- ^ The mode of the database activity stream.
  , status :: Core.Maybe Types.ActivityStreamStatus
    -- ^ The status of the database activity stream.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartActivityStreamResponse' value with any optional fields omitted.
mkStartActivityStreamResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartActivityStreamResponse
mkStartActivityStreamResponse responseStatus
  = StartActivityStreamResponse'{applyImmediately = Core.Nothing,
                                 kinesisStreamName = Core.Nothing, kmsKeyId = Core.Nothing,
                                 mode = Core.Nothing, status = Core.Nothing, responseStatus}

-- | Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsApplyImmediately :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Bool)
srsApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE srsApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

-- | The name of the Amazon Kinesis data stream to be used for the database activity stream.
--
-- /Note:/ Consider using 'kinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKinesisStreamName :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Text)
srsKinesisStreamName = Lens.field @"kinesisStreamName"
{-# INLINEABLE srsKinesisStreamName #-}
{-# DEPRECATED kinesisStreamName "Use generic-lens or generic-optics with 'kinesisStreamName' instead"  #-}

-- | The AWS KMS key identifier for encryption of messages in the database activity stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKmsKeyId :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Core.Text)
srsKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE srsKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The mode of the database activity stream.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsMode :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.ActivityStreamMode)
srsMode = Lens.field @"mode"
{-# INLINEABLE srsMode #-}
{-# DEPRECATED mode "Use generic-lens or generic-optics with 'mode' instead"  #-}

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStatus :: Lens.Lens' StartActivityStreamResponse (Core.Maybe Types.ActivityStreamStatus)
srsStatus = Lens.field @"status"
{-# INLINEABLE srsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartActivityStreamResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
