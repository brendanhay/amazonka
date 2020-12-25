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
    ksdiTableName,
    ksdiStreamArn,
  )
where

import qualified Network.AWS.DynamoDB.Types.StreamArn as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkKinesisStreamingDestinationInput' smart constructor.
data KinesisStreamingDestinationInput = KinesisStreamingDestinationInput'
  { -- | The name of the DynamoDB table.
    tableName :: Types.TableName,
    -- | The ARN for a Kinesis data stream.
    streamArn :: Types.StreamArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamingDestinationInput' value with any optional fields omitted.
mkKinesisStreamingDestinationInput ::
  -- | 'tableName'
  Types.TableName ->
  -- | 'streamArn'
  Types.StreamArn ->
  KinesisStreamingDestinationInput
mkKinesisStreamingDestinationInput tableName streamArn =
  KinesisStreamingDestinationInput' {tableName, streamArn}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdiTableName :: Lens.Lens' KinesisStreamingDestinationInput Types.TableName
ksdiTableName = Lens.field @"tableName"
{-# DEPRECATED ksdiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdiStreamArn :: Lens.Lens' KinesisStreamingDestinationInput Types.StreamArn
ksdiStreamArn = Lens.field @"streamArn"
{-# DEPRECATED ksdiStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

instance Core.FromJSON KinesisStreamingDestinationInput where
  toJSON KinesisStreamingDestinationInput {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just ("StreamArn" Core..= streamArn)
          ]
      )
