-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Session
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Session
  ( Session (..),

    -- * Smart constructor
    mkSession,

    -- * Lenses
    sesStopTimestamp,
    sesDuration,
    sesStartTimestamp,
    sesId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a session.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { stopTimestamp :: Lude.Maybe Lude.Text,
    duration :: Lude.Maybe Lude.Int,
    startTimestamp :: Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- * 'duration' - The duration of the session, in milliseconds.
-- * 'id' - The unique identifier for the session.
-- * 'startTimestamp' - The date and time when the session began.
-- * 'stopTimestamp' - The date and time when the session ended.
mkSession ::
  -- | 'startTimestamp'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  Session
mkSession pStartTimestamp_ pId_ =
  Session'
    { stopTimestamp = Lude.Nothing,
      duration = Lude.Nothing,
      startTimestamp = pStartTimestamp_,
      id = pId_
    }

-- | The date and time when the session ended.
--
-- /Note:/ Consider using 'stopTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesStopTimestamp :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sesStopTimestamp = Lens.lens (stopTimestamp :: Session -> Lude.Maybe Lude.Text) (\s a -> s {stopTimestamp = a} :: Session)
{-# DEPRECATED sesStopTimestamp "Use generic-lens or generic-optics with 'stopTimestamp' instead." #-}

-- | The duration of the session, in milliseconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesDuration :: Lens.Lens' Session (Lude.Maybe Lude.Int)
sesDuration = Lens.lens (duration :: Session -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Session)
{-# DEPRECATED sesDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The date and time when the session began.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesStartTimestamp :: Lens.Lens' Session Lude.Text
sesStartTimestamp = Lens.lens (startTimestamp :: Session -> Lude.Text) (\s a -> s {startTimestamp = a} :: Session)
{-# DEPRECATED sesStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sesId :: Lens.Lens' Session Lude.Text
sesId = Lens.lens (id :: Session -> Lude.Text) (\s a -> s {id = a} :: Session)
{-# DEPRECATED sesId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.ToJSON Session where
  toJSON Session' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StopTimestamp" Lude..=) Lude.<$> stopTimestamp,
            ("Duration" Lude..=) Lude.<$> duration,
            Lude.Just ("StartTimestamp" Lude..= startTimestamp),
            Lude.Just ("Id" Lude..= id)
          ]
      )
