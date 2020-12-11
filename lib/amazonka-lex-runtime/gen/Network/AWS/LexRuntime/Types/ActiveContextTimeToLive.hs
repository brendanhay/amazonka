-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ActiveContextTimeToLive
  ( ActiveContextTimeToLive (..),

    -- * Smart constructor
    mkActiveContextTimeToLive,

    -- * Lenses
    acttlTurnsToLive,
    acttlTimeToLiveInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The length of time or number of turns that a context remains active.
--
-- /See:/ 'mkActiveContextTimeToLive' smart constructor.
data ActiveContextTimeToLive = ActiveContextTimeToLive'
  { turnsToLive ::
      Lude.Maybe Lude.Natural,
    timeToLiveInSeconds ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActiveContextTimeToLive' with the minimum fields required to make a request.
--
-- * 'timeToLiveInSeconds' - The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
-- * 'turnsToLive' - The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
mkActiveContextTimeToLive ::
  ActiveContextTimeToLive
mkActiveContextTimeToLive =
  ActiveContextTimeToLive'
    { turnsToLive = Lude.Nothing,
      timeToLiveInSeconds = Lude.Nothing
    }

-- | The number of conversation turns that the context should be active. A conversation turn is one @PostContent@ or @PostText@ request and the corresponding response from Amazon Lex.
--
-- /Note:/ Consider using 'turnsToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acttlTurnsToLive :: Lens.Lens' ActiveContextTimeToLive (Lude.Maybe Lude.Natural)
acttlTurnsToLive = Lens.lens (turnsToLive :: ActiveContextTimeToLive -> Lude.Maybe Lude.Natural) (\s a -> s {turnsToLive = a} :: ActiveContextTimeToLive)
{-# DEPRECATED acttlTurnsToLive "Use generic-lens or generic-optics with 'turnsToLive' instead." #-}

-- | The number of seconds that the context should be active after it is first sent in a @PostContent@ or @PostText@ response. You can set the value between 5 and 86,400 seconds (24 hours).
--
-- /Note:/ Consider using 'timeToLiveInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acttlTimeToLiveInSeconds :: Lens.Lens' ActiveContextTimeToLive (Lude.Maybe Lude.Natural)
acttlTimeToLiveInSeconds = Lens.lens (timeToLiveInSeconds :: ActiveContextTimeToLive -> Lude.Maybe Lude.Natural) (\s a -> s {timeToLiveInSeconds = a} :: ActiveContextTimeToLive)
{-# DEPRECATED acttlTimeToLiveInSeconds "Use generic-lens or generic-optics with 'timeToLiveInSeconds' instead." #-}

instance Lude.FromJSON ActiveContextTimeToLive where
  parseJSON =
    Lude.withObject
      "ActiveContextTimeToLive"
      ( \x ->
          ActiveContextTimeToLive'
            Lude.<$> (x Lude..:? "turnsToLive")
            Lude.<*> (x Lude..:? "timeToLiveInSeconds")
      )

instance Lude.ToJSON ActiveContextTimeToLive where
  toJSON ActiveContextTimeToLive' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("turnsToLive" Lude..=) Lude.<$> turnsToLive,
            ("timeToLiveInSeconds" Lude..=) Lude.<$> timeToLiveInSeconds
          ]
      )
