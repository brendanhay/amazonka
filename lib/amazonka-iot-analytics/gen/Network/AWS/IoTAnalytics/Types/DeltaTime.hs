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
    dtTimeExpression,
    dtOffsetSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to limit data to that which has arrived since the last execution of the action.
--
-- /See:/ 'mkDeltaTime' smart constructor.
data DeltaTime = DeltaTime'
  { -- | An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
    timeExpression :: Lude.Text,
    -- | The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
    offsetSeconds :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeltaTime' with the minimum fields required to make a request.
--
-- * 'timeExpression' - An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
-- * 'offsetSeconds' - The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
mkDeltaTime ::
  -- | 'timeExpression'
  Lude.Text ->
  -- | 'offsetSeconds'
  Lude.Int ->
  DeltaTime
mkDeltaTime pTimeExpression_ pOffsetSeconds_ =
  DeltaTime'
    { timeExpression = pTimeExpression_,
      offsetSeconds = pOffsetSeconds_
    }

-- | An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
--
-- /Note:/ Consider using 'timeExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTimeExpression :: Lens.Lens' DeltaTime Lude.Text
dtTimeExpression = Lens.lens (timeExpression :: DeltaTime -> Lude.Text) (\s a -> s {timeExpression = a} :: DeltaTime)
{-# DEPRECATED dtTimeExpression "Use generic-lens or generic-optics with 'timeExpression' instead." #-}

-- | The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
--
-- /Note:/ Consider using 'offsetSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtOffsetSeconds :: Lens.Lens' DeltaTime Lude.Int
dtOffsetSeconds = Lens.lens (offsetSeconds :: DeltaTime -> Lude.Int) (\s a -> s {offsetSeconds = a} :: DeltaTime)
{-# DEPRECATED dtOffsetSeconds "Use generic-lens or generic-optics with 'offsetSeconds' instead." #-}

instance Lude.FromJSON DeltaTime where
  parseJSON =
    Lude.withObject
      "DeltaTime"
      ( \x ->
          DeltaTime'
            Lude.<$> (x Lude..: "timeExpression") Lude.<*> (x Lude..: "offsetSeconds")
      )

instance Lude.ToJSON DeltaTime where
  toJSON DeltaTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("timeExpression" Lude..= timeExpression),
            Lude.Just ("offsetSeconds" Lude..= offsetSeconds)
          ]
      )
