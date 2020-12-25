{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisStreamConfig
  ( KinesisStreamConfig (..),

    -- * Smart constructor
    mkKinesisStreamConfig,

    -- * Lenses
    kscStreamArn,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information of a Kinesis data stream.
--
-- /See:/ 'mkKinesisStreamConfig' smart constructor.
newtype KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of the data stream.
    streamArn :: Types.ARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamConfig' value with any optional fields omitted.
mkKinesisStreamConfig ::
  -- | 'streamArn'
  Types.ARN ->
  KinesisStreamConfig
mkKinesisStreamConfig streamArn = KinesisStreamConfig' {streamArn}

-- | The Amazon Resource Name (ARN) of the data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kscStreamArn :: Lens.Lens' KinesisStreamConfig Types.ARN
kscStreamArn = Lens.field @"streamArn"
{-# DEPRECATED kscStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

instance Core.FromJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig {..} =
    Core.object
      (Core.catMaybes [Core.Just ("StreamArn" Core..= streamArn)])

instance Core.FromJSON KinesisStreamConfig where
  parseJSON =
    Core.withObject "KinesisStreamConfig" Core.$
      \x -> KinesisStreamConfig' Core.<$> (x Core..: "StreamArn")
