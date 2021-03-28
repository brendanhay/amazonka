{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WriteEventStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.WriteEventStream
  ( WriteEventStream (..)
  -- * Smart constructor
  , mkWriteEventStream
  -- * Lenses
  , wesRoleArn
  , wesDestinationStreamArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the Amazon Resource Name (ARN) of an event stream to publish events to and the AWS Identity and Access Management (IAM) role to use when publishing those events.
--
-- /See:/ 'mkWriteEventStream' smart constructor.
data WriteEventStream = WriteEventStream'
  { roleArn :: Core.Text
    -- ^ The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
  , destinationStreamArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WriteEventStream' value with any optional fields omitted.
mkWriteEventStream
    :: Core.Text -- ^ 'roleArn'
    -> Core.Text -- ^ 'destinationStreamArn'
    -> WriteEventStream
mkWriteEventStream roleArn destinationStreamArn
  = WriteEventStream'{roleArn, destinationStreamArn}

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to publish event data to the stream in your AWS account.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wesRoleArn :: Lens.Lens' WriteEventStream Core.Text
wesRoleArn = Lens.field @"roleArn"
{-# INLINEABLE wesRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or Amazon Kinesis Data Firehose delivery stream that you want to publish event data to.
--
-- For a Kinesis data stream, the ARN format is: arn:aws:kinesis:<replaceable>region:<replaceable>account-id:stream/<replaceable>stream_name
-- For a Kinesis Data Firehose delivery stream, the ARN format is: arn:aws:firehose:<replaceable>region:<replaceable>account-id:deliverystream/<replaceable>stream_name
--
-- /Note:/ Consider using 'destinationStreamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wesDestinationStreamArn :: Lens.Lens' WriteEventStream Core.Text
wesDestinationStreamArn = Lens.field @"destinationStreamArn"
{-# INLINEABLE wesDestinationStreamArn #-}
{-# DEPRECATED destinationStreamArn "Use generic-lens or generic-optics with 'destinationStreamArn' instead"  #-}

instance Core.FromJSON WriteEventStream where
        toJSON WriteEventStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoleArn" Core..= roleArn),
                  Core.Just ("DestinationStreamArn" Core..= destinationStreamArn)])
