{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
  ( InputStartingPositionConfiguration (..),

    -- * Smart constructor
    mkInputStartingPositionConfiguration,

    -- * Lenses
    ispcInputStartingPosition,
  )
where

import Network.AWS.KinesisAnalytics.Types.InputStartingPosition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the point at which the application reads from the streaming source.
--
-- /See:/ 'mkInputStartingPositionConfiguration' smart constructor.
newtype InputStartingPositionConfiguration = InputStartingPositionConfiguration'
  { -- | The starting position on the stream.
    --
    --
    --     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.
    --
    --
    --     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.
    --
    --
    --     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
    inputStartingPosition :: Lude.Maybe InputStartingPosition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputStartingPositionConfiguration' with the minimum fields required to make a request.
--
-- * 'inputStartingPosition' - The starting position on the stream.
--
--
--     * @NOW@ - Start reading just after the most recent record in the stream, start at the request time stamp that the customer issued.
--
--
--     * @TRIM_HORIZON@ - Start reading at the last untrimmed record in the stream, which is the oldest record available in the stream. This option is not available for an Amazon Kinesis Firehose delivery stream.
--
--
--     * @LAST_STOPPED_POINT@ - Resume reading from where the application last stopped reading.
mkInputStartingPositionConfiguration ::
  InputStartingPositionConfiguration
mkInputStartingPositionConfiguration =
  InputStartingPositionConfiguration'
    { inputStartingPosition =
        Lude.Nothing
    }

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
ispcInputStartingPosition :: Lens.Lens' InputStartingPositionConfiguration (Lude.Maybe InputStartingPosition)
ispcInputStartingPosition = Lens.lens (inputStartingPosition :: InputStartingPositionConfiguration -> Lude.Maybe InputStartingPosition) (\s a -> s {inputStartingPosition = a} :: InputStartingPositionConfiguration)
{-# DEPRECATED ispcInputStartingPosition "Use generic-lens or generic-optics with 'inputStartingPosition' instead." #-}

instance Lude.FromJSON InputStartingPositionConfiguration where
  parseJSON =
    Lude.withObject
      "InputStartingPositionConfiguration"
      ( \x ->
          InputStartingPositionConfiguration'
            Lude.<$> (x Lude..:? "InputStartingPosition")
      )

instance Lude.ToJSON InputStartingPositionConfiguration where
  toJSON InputStartingPositionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("InputStartingPosition" Lude..=) Lude.<$> inputStartingPosition]
      )
