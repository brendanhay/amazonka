{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTime
  ( DeltaTime (..),

    -- * Smart constructor
    mkDeltaTime,

    -- * Lenses
    dtOffsetSeconds,
    dtTimeExpression,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.TimeExpression as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Used to limit data to that which has arrived since the last execution of the action.
--
-- /See:/ 'mkDeltaTime' smart constructor.
data DeltaTime = DeltaTime'
  { -- | The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
    offsetSeconds :: Core.Int,
    -- | An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
    timeExpression :: Types.TimeExpression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeltaTime' value with any optional fields omitted.
mkDeltaTime ::
  -- | 'offsetSeconds'
  Core.Int ->
  -- | 'timeExpression'
  Types.TimeExpression ->
  DeltaTime
mkDeltaTime offsetSeconds timeExpression =
  DeltaTime' {offsetSeconds, timeExpression}

-- | The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
--
-- /Note:/ Consider using 'offsetSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtOffsetSeconds :: Lens.Lens' DeltaTime Core.Int
dtOffsetSeconds = Lens.field @"offsetSeconds"
{-# DEPRECATED dtOffsetSeconds "Use generic-lens or generic-optics with 'offsetSeconds' instead." #-}

-- | An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
--
-- /Note:/ Consider using 'timeExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTimeExpression :: Lens.Lens' DeltaTime Types.TimeExpression
dtTimeExpression = Lens.field @"timeExpression"
{-# DEPRECATED dtTimeExpression "Use generic-lens or generic-optics with 'timeExpression' instead." #-}

instance Core.FromJSON DeltaTime where
  toJSON DeltaTime {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("offsetSeconds" Core..= offsetSeconds),
            Core.Just ("timeExpression" Core..= timeExpression)
          ]
      )

instance Core.FromJSON DeltaTime where
  parseJSON =
    Core.withObject "DeltaTime" Core.$
      \x ->
        DeltaTime'
          Core.<$> (x Core..: "offsetSeconds") Core.<*> (x Core..: "timeExpression")
