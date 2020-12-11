{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sasApplyImmediately,
    sasResourceARN,

    -- * Destructuring the response
    StopActivityStreamResponse (..),
    mkStopActivityStreamResponse,

    -- ** Response lenses
    sasrsStatus,
    sasrsKinesisStreamName,
    sasrsKMSKeyId,
    sasrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopActivityStream' smart constructor.
data StopActivityStream = StopActivityStream'
  { applyImmediately ::
      Lude.Maybe Lude.Bool,
    resourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopActivityStream' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
mkStopActivityStream ::
  -- | 'resourceARN'
  Lude.Text ->
  StopActivityStream
mkStopActivityStream pResourceARN_ =
  StopActivityStream'
    { applyImmediately = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | Specifies whether or not the database activity stream is to stop as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasApplyImmediately :: Lens.Lens' StopActivityStream (Lude.Maybe Lude.Bool)
sasApplyImmediately = Lens.lens (applyImmediately :: StopActivityStream -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: StopActivityStream)
{-# DEPRECATED sasApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The Amazon Resource Name (ARN) of the DB cluster for the database activity stream. For example, @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasResourceARN :: Lens.Lens' StopActivityStream Lude.Text
sasResourceARN = Lens.lens (resourceARN :: StopActivityStream -> Lude.Text) (\s a -> s {resourceARN = a} :: StopActivityStream)
{-# DEPRECATED sasResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest StopActivityStream where
  type Rs StopActivityStream = StopActivityStreamResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StopActivityStreamResult"
      ( \s h x ->
          StopActivityStreamResponse'
            Lude.<$> (x Lude..@? "Status")
            Lude.<*> (x Lude..@? "KinesisStreamName")
            Lude.<*> (x Lude..@? "KmsKeyId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopActivityStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StopActivityStream where
  toPath = Lude.const "/"

instance Lude.ToQuery StopActivityStream where
  toQuery StopActivityStream' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StopActivityStream" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ApplyImmediately" Lude.=: applyImmediately,
        "ResourceArn" Lude.=: resourceARN
      ]

-- | /See:/ 'mkStopActivityStreamResponse' smart constructor.
data StopActivityStreamResponse = StopActivityStreamResponse'
  { status ::
      Lude.Maybe ActivityStreamStatus,
    kinesisStreamName ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopActivityStreamResponse' with the minimum fields required to make a request.
--
-- * 'kinesisStreamName' - The name of the Amazon Kinesis data stream used for the database activity stream.
-- * 'kmsKeyId' - The AWS KMS key identifier used for encrypting messages in the database activity stream.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the database activity stream.
mkStopActivityStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopActivityStreamResponse
mkStopActivityStreamResponse pResponseStatus_ =
  StopActivityStreamResponse'
    { status = Lude.Nothing,
      kinesisStreamName = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrsStatus :: Lens.Lens' StopActivityStreamResponse (Lude.Maybe ActivityStreamStatus)
sasrsStatus = Lens.lens (status :: StopActivityStreamResponse -> Lude.Maybe ActivityStreamStatus) (\s a -> s {status = a} :: StopActivityStreamResponse)
{-# DEPRECATED sasrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the Amazon Kinesis data stream used for the database activity stream.
--
-- /Note:/ Consider using 'kinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrsKinesisStreamName :: Lens.Lens' StopActivityStreamResponse (Lude.Maybe Lude.Text)
sasrsKinesisStreamName = Lens.lens (kinesisStreamName :: StopActivityStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {kinesisStreamName = a} :: StopActivityStreamResponse)
{-# DEPRECATED sasrsKinesisStreamName "Use generic-lens or generic-optics with 'kinesisStreamName' instead." #-}

-- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrsKMSKeyId :: Lens.Lens' StopActivityStreamResponse (Lude.Maybe Lude.Text)
sasrsKMSKeyId = Lens.lens (kmsKeyId :: StopActivityStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: StopActivityStreamResponse)
{-# DEPRECATED sasrsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasrsResponseStatus :: Lens.Lens' StopActivityStreamResponse Lude.Int
sasrsResponseStatus = Lens.lens (responseStatus :: StopActivityStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopActivityStreamResponse)
{-# DEPRECATED sasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
