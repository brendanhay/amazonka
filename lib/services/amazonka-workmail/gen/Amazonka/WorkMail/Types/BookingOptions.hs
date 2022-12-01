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
-- Module      : Amazonka.WorkMail.Types.BookingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.BookingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | At least one delegate must be associated to the resource to disable
-- automatic replies from the resource.
--
-- /See:/ 'newBookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { -- | The resource\'s ability to automatically decline any recurring requests.
    autoDeclineRecurringRequests :: Prelude.Maybe Prelude.Bool,
    -- | The resource\'s ability to automatically decline any conflicting
    -- requests.
    autoDeclineConflictingRequests :: Prelude.Maybe Prelude.Bool,
    -- | The resource\'s ability to automatically reply to requests. If disabled,
    -- delegates must be associated to the resource.
    autoAcceptRequests :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BookingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoDeclineRecurringRequests', 'bookingOptions_autoDeclineRecurringRequests' - The resource\'s ability to automatically decline any recurring requests.
--
-- 'autoDeclineConflictingRequests', 'bookingOptions_autoDeclineConflictingRequests' - The resource\'s ability to automatically decline any conflicting
-- requests.
--
-- 'autoAcceptRequests', 'bookingOptions_autoAcceptRequests' - The resource\'s ability to automatically reply to requests. If disabled,
-- delegates must be associated to the resource.
newBookingOptions ::
  BookingOptions
newBookingOptions =
  BookingOptions'
    { autoDeclineRecurringRequests =
        Prelude.Nothing,
      autoDeclineConflictingRequests = Prelude.Nothing,
      autoAcceptRequests = Prelude.Nothing
    }

-- | The resource\'s ability to automatically decline any recurring requests.
bookingOptions_autoDeclineRecurringRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoDeclineRecurringRequests = Lens.lens (\BookingOptions' {autoDeclineRecurringRequests} -> autoDeclineRecurringRequests) (\s@BookingOptions' {} a -> s {autoDeclineRecurringRequests = a} :: BookingOptions)

-- | The resource\'s ability to automatically decline any conflicting
-- requests.
bookingOptions_autoDeclineConflictingRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoDeclineConflictingRequests = Lens.lens (\BookingOptions' {autoDeclineConflictingRequests} -> autoDeclineConflictingRequests) (\s@BookingOptions' {} a -> s {autoDeclineConflictingRequests = a} :: BookingOptions)

-- | The resource\'s ability to automatically reply to requests. If disabled,
-- delegates must be associated to the resource.
bookingOptions_autoAcceptRequests :: Lens.Lens' BookingOptions (Prelude.Maybe Prelude.Bool)
bookingOptions_autoAcceptRequests = Lens.lens (\BookingOptions' {autoAcceptRequests} -> autoAcceptRequests) (\s@BookingOptions' {} a -> s {autoAcceptRequests = a} :: BookingOptions)

instance Core.FromJSON BookingOptions where
  parseJSON =
    Core.withObject
      "BookingOptions"
      ( \x ->
          BookingOptions'
            Prelude.<$> (x Core..:? "AutoDeclineRecurringRequests")
            Prelude.<*> (x Core..:? "AutoDeclineConflictingRequests")
            Prelude.<*> (x Core..:? "AutoAcceptRequests")
      )

instance Prelude.Hashable BookingOptions where
  hashWithSalt _salt BookingOptions' {..} =
    _salt
      `Prelude.hashWithSalt` autoDeclineRecurringRequests
      `Prelude.hashWithSalt` autoDeclineConflictingRequests
      `Prelude.hashWithSalt` autoAcceptRequests

instance Prelude.NFData BookingOptions where
  rnf BookingOptions' {..} =
    Prelude.rnf autoDeclineRecurringRequests
      `Prelude.seq` Prelude.rnf autoDeclineConflictingRequests
      `Prelude.seq` Prelude.rnf autoAcceptRequests

instance Core.ToJSON BookingOptions where
  toJSON BookingOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AutoDeclineRecurringRequests" Core..=)
              Prelude.<$> autoDeclineRecurringRequests,
            ("AutoDeclineConflictingRequests" Core..=)
              Prelude.<$> autoDeclineConflictingRequests,
            ("AutoAcceptRequests" Core..=)
              Prelude.<$> autoAcceptRequests
          ]
      )
