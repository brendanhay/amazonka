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
    kaPartitionKey,
    kaRoleARN,
    kaStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to write data to an Amazon Kinesis stream.
--
-- /See:/ 'mkKinesisAction' smart constructor.
data KinesisAction = KinesisAction'
  { partitionKey ::
      Lude.Maybe Lude.Text,
    roleARN :: Lude.Text,
    streamName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisAction' with the minimum fields required to make a request.
--
-- * 'partitionKey' - The partition key.
-- * 'roleARN' - The ARN of the IAM role that grants access to the Amazon Kinesis stream.
-- * 'streamName' - The name of the Amazon Kinesis stream.
mkKinesisAction ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'streamName'
  Lude.Text ->
  KinesisAction
mkKinesisAction pRoleARN_ pStreamName_ =
  KinesisAction'
    { partitionKey = Lude.Nothing,
      roleARN = pRoleARN_,
      streamName = pStreamName_
    }

-- | The partition key.
--
-- /Note:/ Consider using 'partitionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaPartitionKey :: Lens.Lens' KinesisAction (Lude.Maybe Lude.Text)
kaPartitionKey = Lens.lens (partitionKey :: KinesisAction -> Lude.Maybe Lude.Text) (\s a -> s {partitionKey = a} :: KinesisAction)
{-# DEPRECATED kaPartitionKey "Use generic-lens or generic-optics with 'partitionKey' instead." #-}

-- | The ARN of the IAM role that grants access to the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaRoleARN :: Lens.Lens' KinesisAction Lude.Text
kaRoleARN = Lens.lens (roleARN :: KinesisAction -> Lude.Text) (\s a -> s {roleARN = a} :: KinesisAction)
{-# DEPRECATED kaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaStreamName :: Lens.Lens' KinesisAction Lude.Text
kaStreamName = Lens.lens (streamName :: KinesisAction -> Lude.Text) (\s a -> s {streamName = a} :: KinesisAction)
{-# DEPRECATED kaStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.FromJSON KinesisAction where
  parseJSON =
    Lude.withObject
      "KinesisAction"
      ( \x ->
          KinesisAction'
            Lude.<$> (x Lude..:? "partitionKey")
            Lude.<*> (x Lude..: "roleArn")
            Lude.<*> (x Lude..: "streamName")
      )

instance Lude.ToJSON KinesisAction where
  toJSON KinesisAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("partitionKey" Lude..=) Lude.<$> partitionKey,
            Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("streamName" Lude..= streamName)
          ]
      )
