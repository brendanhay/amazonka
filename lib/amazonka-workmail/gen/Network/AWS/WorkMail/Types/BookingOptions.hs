{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.BookingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.BookingOptions
  ( BookingOptions (..),

    -- * Smart constructor
    mkBookingOptions,

    -- * Lenses
    boAutoDeclineConflictingRequests,
    boAutoDeclineRecurringRequests,
    boAutoAcceptRequests,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | At least one delegate must be associated to the resource to disable automatic replies from the resource.
--
-- /See:/ 'mkBookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { -- | The resource's ability to automatically decline any conflicting requests.
    autoDeclineConflictingRequests :: Lude.Maybe Lude.Bool,
    -- | The resource's ability to automatically decline any recurring requests.
    autoDeclineRecurringRequests :: Lude.Maybe Lude.Bool,
    -- | The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
    autoAcceptRequests :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BookingOptions' with the minimum fields required to make a request.
--
-- * 'autoDeclineConflictingRequests' - The resource's ability to automatically decline any conflicting requests.
-- * 'autoDeclineRecurringRequests' - The resource's ability to automatically decline any recurring requests.
-- * 'autoAcceptRequests' - The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
mkBookingOptions ::
  BookingOptions
mkBookingOptions =
  BookingOptions'
    { autoDeclineConflictingRequests = Lude.Nothing,
      autoDeclineRecurringRequests = Lude.Nothing,
      autoAcceptRequests = Lude.Nothing
    }

-- | The resource's ability to automatically decline any conflicting requests.
--
-- /Note:/ Consider using 'autoDeclineConflictingRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoDeclineConflictingRequests :: Lens.Lens' BookingOptions (Lude.Maybe Lude.Bool)
boAutoDeclineConflictingRequests = Lens.lens (autoDeclineConflictingRequests :: BookingOptions -> Lude.Maybe Lude.Bool) (\s a -> s {autoDeclineConflictingRequests = a} :: BookingOptions)
{-# DEPRECATED boAutoDeclineConflictingRequests "Use generic-lens or generic-optics with 'autoDeclineConflictingRequests' instead." #-}

-- | The resource's ability to automatically decline any recurring requests.
--
-- /Note:/ Consider using 'autoDeclineRecurringRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoDeclineRecurringRequests :: Lens.Lens' BookingOptions (Lude.Maybe Lude.Bool)
boAutoDeclineRecurringRequests = Lens.lens (autoDeclineRecurringRequests :: BookingOptions -> Lude.Maybe Lude.Bool) (\s a -> s {autoDeclineRecurringRequests = a} :: BookingOptions)
{-# DEPRECATED boAutoDeclineRecurringRequests "Use generic-lens or generic-optics with 'autoDeclineRecurringRequests' instead." #-}

-- | The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
--
-- /Note:/ Consider using 'autoAcceptRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
boAutoAcceptRequests :: Lens.Lens' BookingOptions (Lude.Maybe Lude.Bool)
boAutoAcceptRequests = Lens.lens (autoAcceptRequests :: BookingOptions -> Lude.Maybe Lude.Bool) (\s a -> s {autoAcceptRequests = a} :: BookingOptions)
{-# DEPRECATED boAutoAcceptRequests "Use generic-lens or generic-optics with 'autoAcceptRequests' instead." #-}

instance Lude.FromJSON BookingOptions where
  parseJSON =
    Lude.withObject
      "BookingOptions"
      ( \x ->
          BookingOptions'
            Lude.<$> (x Lude..:? "AutoDeclineConflictingRequests")
            Lude.<*> (x Lude..:? "AutoDeclineRecurringRequests")
            Lude.<*> (x Lude..:? "AutoAcceptRequests")
      )

instance Lude.ToJSON BookingOptions where
  toJSON BookingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AutoDeclineConflictingRequests" Lude..=)
              Lude.<$> autoDeclineConflictingRequests,
            ("AutoDeclineRecurringRequests" Lude..=)
              Lude.<$> autoDeclineRecurringRequests,
            ("AutoAcceptRequests" Lude..=) Lude.<$> autoAcceptRequests
          ]
      )
