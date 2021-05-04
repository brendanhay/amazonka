{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.BookingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.BookingOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | At least one delegate must be associated to the resource to disable
-- automatic replies from the resource.
--
-- /See:/ 'newBookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { -- | The resource\'s ability to automatically decline any conflicting
    -- requests.
    autoDeclineConflictingRequests :: Prelude.Maybe Prelude.Bool,
    -- | The resource\'s ability to automatically decline any recurring requests.
    autoDeclineRecurringRequests :: Prelude.Maybe Prelude.Bool,
    -- | The resource\'s ability to automatically reply to requests. If disabled,
    -- delegates must be associated to the resource.
    autoAcceptRequests :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BookingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoDeclineConflictingRequests', 'bookingOptions_autoDeclineConflictingRequests' - The resource\'s ability to automatically decline any conflicting
-- requests.
--
-- 'autoDeclineRecurringRequests', 'bookingOptions_autoDeclineRecurringRequests' - The resource\'s ability to automatically decline any recurring requests.
--
-- 'autoAcceptRequests', 'bookingOptions_autoAcceptRequests' - The resource\'s ability to automatically reply to requests. If disabled,
-- delegates must be associated to the resource.
newBookingOptions ::
  BookingOptions
newBookingOptions =
  BookingOptions'
    { autoDeclineConflictingRequests =
        Prelude.Nothing,
      autoDeclineRecurringRequests = Prelude.Nothing,
      autoAcceptRequests = Prelude.Nothing
    }

-- | The resource\'s ability to automatically decline any conflicting
-- requests.
bookingOptions_autoDeclineConflictingRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoDeclineConflictingRequests = Lens.lens (\BookingOptions' {autoDeclineConflictingRequests} -> autoDeclineConflictingRequests) (\s@BookingOptions' {} a -> s {autoDeclineConflictingRequests = a} :: BookingOptions)

-- | The resource\'s ability to automatically decline any recurring requests.
bookingOptions_autoDeclineRecurringRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoDeclineRecurringRequests = Lens.lens (\BookingOptions' {autoDeclineRecurringRequests} -> autoDeclineRecurringRequests) (\s@BookingOptions' {} a -> s {autoDeclineRecurringRequests = a} :: BookingOptions)

-- | The resource\'s ability to automatically reply to requests. If disabled,
-- delegates must be associated to the resource.
bookingOptions_autoAcceptRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoAcceptRequests = Lens.lens (\BookingOptions' {autoAcceptRequests} -> autoAcceptRequests) (\s@BookingOptions' {} a -> s {autoAcceptRequests = a} :: BookingOptions)

instance Prelude.FromJSON BookingOptions where
  parseJSON =
    Prelude.withObject
      "BookingOptions"
      ( \x ->
          BookingOptions'
            Prelude.<$> (x Prelude..:? "AutoDeclineConflictingRequests")
            Prelude.<*> (x Prelude..:? "AutoDeclineRecurringRequests")
            Prelude.<*> (x Prelude..:? "AutoAcceptRequests")
      )

instance Prelude.Hashable BookingOptions

instance Prelude.NFData BookingOptions

instance Prelude.ToJSON BookingOptions where
  toJSON BookingOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AutoDeclineConflictingRequests" Prelude..=)
              Prelude.<$> autoDeclineConflictingRequests,
            ("AutoDeclineRecurringRequests" Prelude..=)
              Prelude.<$> autoDeclineRecurringRequests,
            ("AutoAcceptRequests" Prelude..=)
              Prelude.<$> autoAcceptRequests
          ]
      )
