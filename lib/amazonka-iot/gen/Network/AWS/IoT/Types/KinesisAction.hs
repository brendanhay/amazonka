{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.KinesisAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KinesisAction
  ( KinesisAction (..),

    -- * Smart constructor
    mkKinesisAction,

    -- * Lenses
    kaRoleArn,
    kaStreamName,
    kaPartitionKey,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.PartitionKey as Types
import qualified Network.AWS.IoT.Types.StreamName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to write data to an Amazon Kinesis stream.
--
-- /See:/ 'mkKinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { -- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
    roleArn :: Types.AwsArn,
    -- | The name of the Amazon Kinesis stream.
    streamName :: Types.StreamName,
    -- | The partition key.
    partitionKey :: Core.Maybe Types.PartitionKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisAction' value with any optional fields omitted.
mkKinesisAction ::
  -- | 'roleArn'
  Types.AwsArn ->
  -- | 'streamName'
  Types.StreamName ->
  KinesisAction
mkKinesisAction roleArn streamName =
  KinesisAction' {roleArn, streamName, partitionKey = Core.Nothing}

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaRoleArn :: Lens.Lens' KinesisAction Types.AwsArn
kaRoleArn = Lens.field @"roleArn"
{-# DEPRECATED kaRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The name of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaStreamName :: Lens.Lens' KinesisAction Types.StreamName
kaStreamName = Lens.field @"streamName"
{-# DEPRECATED kaStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The partition key.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaPartitionKey :: Lens.Lens' KinesisAction (Core.Maybe Types.PartitionKey)
kaPartitionKey = Lens.field @"partitionKey"
{-# DEPRECATED kaPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

instance Core.FromJSON KinesisAction where
  toJSON KinesisAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("streamName" Core..= streamName),
            ("partitionKey" Core..=) Core.<$> partitionKey
          ]
      )

instance Core.FromJSON KinesisAction where
  parseJSON =
    Core.withObject "KinesisAction" Core.$
      \x ->
        KinesisAction'
          Core.<$> (x Core..: "roleArn")
          Core.<*> (x Core..: "streamName")
          Core.<*> (x Core..:? "partitionKey")
