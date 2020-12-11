-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateInstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateInstantBooking
  ( CreateInstantBooking (..),

    -- * Smart constructor
    mkCreateInstantBooking,

    -- * Lenses
    cibDurationInMinutes,
    cibEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
-- /See:/ 'mkCreateInstantBooking' smart constructor.
data CreateInstantBooking = CreateInstantBooking'
  { durationInMinutes ::
      Lude.Int,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstantBooking' with the minimum fields required to make a request.
--
-- * 'durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
-- * 'enabled' - Whether instant booking is enabled or not.
mkCreateInstantBooking ::
  -- | 'durationInMinutes'
  Lude.Int ->
  -- | 'enabled'
  Lude.Bool ->
  CreateInstantBooking
mkCreateInstantBooking pDurationInMinutes_ pEnabled_ =
  CreateInstantBooking'
    { durationInMinutes = pDurationInMinutes_,
      enabled = pEnabled_
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDurationInMinutes :: Lens.Lens' CreateInstantBooking Lude.Int
cibDurationInMinutes = Lens.lens (durationInMinutes :: CreateInstantBooking -> Lude.Int) (\s a -> s {durationInMinutes = a} :: CreateInstantBooking)
{-# DEPRECATED cibDurationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead." #-}

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibEnabled :: Lens.Lens' CreateInstantBooking Lude.Bool
cibEnabled = Lens.lens (enabled :: CreateInstantBooking -> Lude.Bool) (\s a -> s {enabled = a} :: CreateInstantBooking)
{-# DEPRECATED cibEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToJSON CreateInstantBooking where
  toJSON CreateInstantBooking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DurationInMinutes" Lude..= durationInMinutes),
            Lude.Just ("Enabled" Lude..= enabled)
          ]
      )
