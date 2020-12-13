{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NotificationProperty
  ( NotificationProperty (..),

    -- * Smart constructor
    mkNotificationProperty,

    -- * Lenses
    npNotifyDelayAfter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration properties of a notification.
--
-- /See:/ 'mkNotificationProperty' smart constructor.
newtype NotificationProperty = NotificationProperty'
  { -- | After a job run starts, the number of minutes to wait before sending a job run delay notification.
    notifyDelayAfter :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotificationProperty' with the minimum fields required to make a request.
--
-- * 'notifyDelayAfter' - After a job run starts, the number of minutes to wait before sending a job run delay notification.
mkNotificationProperty ::
  NotificationProperty
mkNotificationProperty =
  NotificationProperty' {notifyDelayAfter = Lude.Nothing}

-- | After a job run starts, the number of minutes to wait before sending a job run delay notification.
--
-- /Note:/ Consider using 'notifyDelayAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNotifyDelayAfter :: Lens.Lens' NotificationProperty (Lude.Maybe Lude.Natural)
npNotifyDelayAfter = Lens.lens (notifyDelayAfter :: NotificationProperty -> Lude.Maybe Lude.Natural) (\s a -> s {notifyDelayAfter = a} :: NotificationProperty)
{-# DEPRECATED npNotifyDelayAfter "Use generic-lens or generic-optics with 'notifyDelayAfter' instead." #-}

instance Lude.FromJSON NotificationProperty where
  parseJSON =
    Lude.withObject
      "NotificationProperty"
      ( \x ->
          NotificationProperty' Lude.<$> (x Lude..:? "NotifyDelayAfter")
      )

instance Lude.ToJSON NotificationProperty where
  toJSON NotificationProperty' {..} =
    Lude.object
      ( Lude.catMaybes
          [("NotifyDelayAfter" Lude..=) Lude.<$> notifyDelayAfter]
      )
