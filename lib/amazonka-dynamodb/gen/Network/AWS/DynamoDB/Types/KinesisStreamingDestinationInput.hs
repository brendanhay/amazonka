{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationInput
  ( KinesisStreamingDestinationInput (..),

    -- * Smart constructor
    mkKinesisStreamingDestinationInput,

    -- * Lenses
    ksdiStreamARN,
    ksdiTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkKinesisStreamingDestinationInput' smart constructor.
data KinesisStreamingDestinationInput = KinesisStreamingDestinationInput'
  { -- | The ARN for a Kinesis data stream.
    streamARN :: Lude.Text,
    -- | The name of the DynamoDB table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisStreamingDestinationInput' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN for a Kinesis data stream.
-- * 'tableName' - The name of the DynamoDB table.
mkKinesisStreamingDestinationInput ::
  -- | 'streamARN'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  KinesisStreamingDestinationInput
mkKinesisStreamingDestinationInput pStreamARN_ pTableName_ =
  KinesisStreamingDestinationInput'
    { streamARN = pStreamARN_,
      tableName = pTableName_
    }

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdiStreamARN :: Lens.Lens' KinesisStreamingDestinationInput Lude.Text
ksdiStreamARN = Lens.lens (streamARN :: KinesisStreamingDestinationInput -> Lude.Text) (\s a -> s {streamARN = a} :: KinesisStreamingDestinationInput)
{-# DEPRECATED ksdiStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdiTableName :: Lens.Lens' KinesisStreamingDestinationInput Lude.Text
ksdiTableName = Lens.lens (tableName :: KinesisStreamingDestinationInput -> Lude.Text) (\s a -> s {tableName = a} :: KinesisStreamingDestinationInput)
{-# DEPRECATED ksdiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.ToJSON KinesisStreamingDestinationInput where
  toJSON KinesisStreamingDestinationInput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamArn" Lude..= streamARN),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )
