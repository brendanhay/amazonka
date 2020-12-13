{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    sfStopTimestamp,
    sfId,
    sfStartTimestamp,
    sfDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a session.
--
-- /See:/ 'mkSession' smart constructor.
data Session = Session'
  { -- | The date and time when the session ended.
    stopTimestamp :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the session.
    id :: Lude.Text,
    -- | The date and time when the session began.
    startTimestamp :: Lude.Text,
    -- | The duration of the session, in milliseconds.
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Session' with the minimum fields required to make a request.
--
-- * 'stopTimestamp' - The date and time when the session ended.
-- * 'id' - The unique identifier for the session.
-- * 'startTimestamp' - The date and time when the session began.
-- * 'duration' - The duration of the session, in milliseconds.
mkSession ::
  -- | 'id'
  Lude.Text ->
  -- | 'startTimestamp'
  Lude.Text ->
  Session
mkSession pId_ pStartTimestamp_ =
  Session'
    { stopTimestamp = Lude.Nothing,
      id = pId_,
      startTimestamp = pStartTimestamp_,
      duration = Lude.Nothing
    }

-- | The date and time when the session ended.
--
-- /Note:/ Consider using 'stopTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStopTimestamp :: Lens.Lens' Session (Lude.Maybe Lude.Text)
sfStopTimestamp = Lens.lens (stopTimestamp :: Session -> Lude.Maybe Lude.Text) (\s a -> s {stopTimestamp = a} :: Session)
{-# DEPRECATED sfStopTimestamp "Use generic-lens or generic-optics with 'stopTimestamp' instead." #-}

-- | The unique identifier for the session.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfId :: Lens.Lens' Session Lude.Text
sfId = Lens.lens (id :: Session -> Lude.Text) (\s a -> s {id = a} :: Session)
{-# DEPRECATED sfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the session began.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStartTimestamp :: Lens.Lens' Session Lude.Text
sfStartTimestamp = Lens.lens (startTimestamp :: Session -> Lude.Text) (\s a -> s {startTimestamp = a} :: Session)
{-# DEPRECATED sfStartTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead." #-}

-- | The duration of the session, in milliseconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDuration :: Lens.Lens' Session (Lude.Maybe Lude.Int)
sfDuration = Lens.lens (duration :: Session -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: Session)
{-# DEPRECATED sfDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.ToJSON Session where
  toJSON Session' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StopTimestamp" Lude..=) Lude.<$> stopTimestamp,
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("StartTimestamp" Lude..= startTimestamp),
            ("Duration" Lude..=) Lude.<$> duration
          ]
      )
