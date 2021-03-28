{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
  ( InputStartingPositionConfiguration (..)
  -- * Smart constructor
  , mkInputStartingPositionConfiguration
  -- * Lenses
  , ispcInputStartingPosition
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.InputStartingPosition as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the point at which the application reads from the streaming source.
--
-- /See:/ 'mkInputStartingPositionConfiguration' smart constructor.
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration'
  { inputStartingPosition :: Core.Maybe Types.InputStartingPosition
    -- ^ The starting position on the stream.
--
--
--     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.
--
--
--     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.
--
--
--     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputStartingPositionConfiguration' value with any optional fields omitted.
mkInputStartingPositionConfiguration
    :: InputStartingPositionConfiguration
mkInputStartingPositionConfiguration
  = InputStartingPositionConfiguration'{inputStartingPosition =
                                          Core.Nothing}

-- | The starting position on the stream.
--
--
--     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.
--
--
--     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.
--
--
--     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
--
--
--
-- /Note:/ Consider using 'inputStartingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ispcInputStartingPosition :: Lens.Lens' InputStartingPositionConfiguration (Core.Maybe Types.InputStartingPosition)
ispcInputStartingPosition = Lens.field @"inputStartingPosition"
{-# INLINEABLE ispcInputStartingPosition #-}
{-# DEPRECATED inputStartingPosition "Use generic-lens or generic-optics with 'inputStartingPosition' instead"  #-}

instance Core.FromJSON InputStartingPositionConfiguration where
        toJSON InputStartingPositionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("InputStartingPosition" Core..=) Core.<$> inputStartingPosition])

instance Core.FromJSON InputStartingPositionConfiguration where
        parseJSON
          = Core.withObject "InputStartingPositionConfiguration" Core.$
              \ x ->
                InputStartingPositionConfiguration' Core.<$>
                  (x Core..:? "InputStartingPosition")
