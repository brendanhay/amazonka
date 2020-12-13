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
    sMode,
    sKMSKeyId,
    sResourceARN,
    sApplyImmediately,

    -- * Destructuring the response
    StartActivityStreamResponse (..),
    mkStartActivityStreamResponse,

    -- ** Response lenses
    srsStatus,
    srsKinesisStreamName,
    srsMode,
    srsKMSKeyId,
    srsApplyImmediately,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartActivityStream' smart constructor.
data StartActivityStream = StartActivityStream'
  { -- | Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
    mode :: ActivityStreamMode,
    -- | The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
    kmsKeyId :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
    resourceARN :: Lude.Text,
    -- | Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartActivityStream' with the minimum fields required to make a request.
--
-- * 'mode' - Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
-- * 'kmsKeyId' - The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
-- * 'applyImmediately' - Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
mkStartActivityStream ::
  -- | 'mode'
  ActivityStreamMode ->
  -- | 'kmsKeyId'
  Lude.Text ->
  -- | 'resourceARN'
  Lude.Text ->
  StartActivityStream
mkStartActivityStream pMode_ pKMSKeyId_ pResourceARN_ =
  StartActivityStream'
    { mode = pMode_,
      kmsKeyId = pKMSKeyId_,
      resourceARN = pResourceARN_,
      applyImmediately = Lude.Nothing
    }

-- | Specifies the mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMode :: Lens.Lens' StartActivityStream ActivityStreamMode
sMode = Lens.lens (mode :: StartActivityStream -> ActivityStreamMode) (\s a -> s {mode = a} :: StartActivityStream)
{-# DEPRECATED sMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The AWS KMS key identifier for encrypting messages in the database activity stream. The key identifier can be either a key ID, a key ARN, or a key alias.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKMSKeyId :: Lens.Lens' StartActivityStream Lude.Text
sKMSKeyId = Lens.lens (kmsKeyId :: StartActivityStream -> Lude.Text) (\s a -> s {kmsKeyId = a} :: StartActivityStream)
{-# DEPRECATED sKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Amazon Resource Name (ARN) of the DB cluster, for example @arn:aws:rds:us-east-1:12345667890:cluster:das-cluster@ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceARN :: Lens.Lens' StartActivityStream Lude.Text
sResourceARN = Lens.lens (resourceARN :: StartActivityStream -> Lude.Text) (\s a -> s {resourceARN = a} :: StartActivityStream)
{-# DEPRECATED sResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Specifies whether or not the database activity stream is to start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplyImmediately :: Lens.Lens' StartActivityStream (Lude.Maybe Lude.Bool)
sApplyImmediately = Lens.lens (applyImmediately :: StartActivityStream -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: StartActivityStream)
{-# DEPRECATED sApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

instance Lude.AWSRequest StartActivityStream where
  type Rs StartActivityStream = StartActivityStreamResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "StartActivityStreamResult"
      ( \s h x ->
          StartActivityStreamResponse'
            Lude.<$> (x Lude..@? "Status")
            Lude.<*> (x Lude..@? "KinesisStreamName")
            Lude.<*> (x Lude..@? "Mode")
            Lude.<*> (x Lude..@? "KmsKeyId")
            Lude.<*> (x Lude..@? "ApplyImmediately")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartActivityStream where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartActivityStream where
  toPath = Lude.const "/"

instance Lude.ToQuery StartActivityStream where
  toQuery StartActivityStream' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartActivityStream" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "Mode" Lude.=: mode,
        "KmsKeyId" Lude.=: kmsKeyId,
        "ResourceArn" Lude.=: resourceARN,
        "ApplyImmediately" Lude.=: applyImmediately
      ]

-- | /See:/ 'mkStartActivityStreamResponse' smart constructor.
data StartActivityStreamResponse = StartActivityStreamResponse'
  { -- | The status of the database activity stream.
    status :: Lude.Maybe ActivityStreamStatus,
    -- | The name of the Amazon Kinesis data stream to be used for the database activity stream.
    kinesisStreamName :: Lude.Maybe Lude.Text,
    -- | The mode of the database activity stream.
    mode :: Lude.Maybe ActivityStreamMode,
    -- | The AWS KMS key identifier for encryption of messages in the database activity stream.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
    applyImmediately :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartActivityStreamResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status of the database activity stream.
-- * 'kinesisStreamName' - The name of the Amazon Kinesis data stream to be used for the database activity stream.
-- * 'mode' - The mode of the database activity stream.
-- * 'kmsKeyId' - The AWS KMS key identifier for encryption of messages in the database activity stream.
-- * 'applyImmediately' - Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
-- * 'responseStatus' - The response status code.
mkStartActivityStreamResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartActivityStreamResponse
mkStartActivityStreamResponse pResponseStatus_ =
  StartActivityStreamResponse'
    { status = Lude.Nothing,
      kinesisStreamName = Lude.Nothing,
      mode = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsStatus :: Lens.Lens' StartActivityStreamResponse (Lude.Maybe ActivityStreamStatus)
srsStatus = Lens.lens (status :: StartActivityStreamResponse -> Lude.Maybe ActivityStreamStatus) (\s a -> s {status = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the Amazon Kinesis data stream to be used for the database activity stream.
--
-- /Note:/ Consider using 'kinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKinesisStreamName :: Lens.Lens' StartActivityStreamResponse (Lude.Maybe Lude.Text)
srsKinesisStreamName = Lens.lens (kinesisStreamName :: StartActivityStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {kinesisStreamName = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsKinesisStreamName "Use generic-lens or generic-optics with 'kinesisStreamName' instead." #-}

-- | The mode of the database activity stream.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsMode :: Lens.Lens' StartActivityStreamResponse (Lude.Maybe ActivityStreamMode)
srsMode = Lens.lens (mode :: StartActivityStreamResponse -> Lude.Maybe ActivityStreamMode) (\s a -> s {mode = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The AWS KMS key identifier for encryption of messages in the database activity stream.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsKMSKeyId :: Lens.Lens' StartActivityStreamResponse (Lude.Maybe Lude.Text)
srsKMSKeyId = Lens.lens (kmsKeyId :: StartActivityStreamResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Indicates whether or not the database activity stream will start as soon as possible, regardless of the maintenance window for the database.
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsApplyImmediately :: Lens.Lens' StartActivityStreamResponse (Lude.Maybe Lude.Bool)
srsApplyImmediately = Lens.lens (applyImmediately :: StartActivityStreamResponse -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartActivityStreamResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartActivityStreamResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartActivityStreamResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
