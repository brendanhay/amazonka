{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WriteEventStream
  ( WriteEventStream (..),

    -- * Smart constructor
    mkWriteEventStream,

    -- * Lenses
    wesRoleARN,
    wesDestinationStreamARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the Amazon Resource Name (ARN) of an event stream to publish events to and the AWS Identity and Access Management (IAM) role to use when publishing those events.
--
-- /See:/ 'mkWriteEventStream' smart constructor.
data WriteEventStream = WriteEventStream'
  { roleARN :: Lude.Text,
    destinationStreamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WriteEventStream' with the minimum fields required to make a request.
--
-- * 'destinationStreamARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
-- * 'roleARN' - The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
mkWriteEventStream ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'destinationStreamARN'
  Lude.Text ->
  WriteEventStream
mkWriteEventStream pRoleARN_ pDestinationStreamARN_ =
  WriteEventStream'
    { roleARN = pRoleARN_,
      destinationStreamARN = pDestinationStreamARN_
    }

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wesRoleARN :: Lens.Lens' WriteEventStream Lude.Text
wesRoleARN = Lens.lens (roleARN :: WriteEventStream -> Lude.Text) (\s a -> s {roleARN = a} :: WriteEventStream)
{-# DEPRECATED wesRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
--
-- /Note:/ Consider using 'destinationStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wesDestinationStreamARN :: Lens.Lens' WriteEventStream Lude.Text
wesDestinationStreamARN = Lens.lens (destinationStreamARN :: WriteEventStream -> Lude.Text) (\s a -> s {destinationStreamARN = a} :: WriteEventStream)
{-# DEPRECATED wesDestinationStreamARN "Use generic-lens or generic-optics with 'destinationStreamARN' instead." #-}

instance Lude.ToJSON WriteEventStream where
  toJSON WriteEventStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RoleArn" Lude..= roleARN),
            Lude.Just ("DestinationStreamArn" Lude..= destinationStreamARN)
          ]
      )
