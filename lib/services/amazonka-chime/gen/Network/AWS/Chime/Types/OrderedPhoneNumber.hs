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
-- Module      : Network.AWS.Chime.Types.OrderedPhoneNumber
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.OrderedPhoneNumber where

import Network.AWS.Chime.Types.OrderedPhoneNumberStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A phone number for which an order has been placed.
--
-- /See:/ 'newOrderedPhoneNumber' smart constructor.
data OrderedPhoneNumber = OrderedPhoneNumber'
  { -- | The phone number status.
    status :: Prelude.Maybe OrderedPhoneNumberStatus,
    -- | The phone number, in E.164 format.
    e164PhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrderedPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'orderedPhoneNumber_status' - The phone number status.
--
-- 'e164PhoneNumber', 'orderedPhoneNumber_e164PhoneNumber' - The phone number, in E.164 format.
newOrderedPhoneNumber ::
  OrderedPhoneNumber
newOrderedPhoneNumber =
  OrderedPhoneNumber'
    { status = Prelude.Nothing,
      e164PhoneNumber = Prelude.Nothing
    }

-- | The phone number status.
orderedPhoneNumber_status :: Lens.Lens' OrderedPhoneNumber (Prelude.Maybe OrderedPhoneNumberStatus)
orderedPhoneNumber_status = Lens.lens (\OrderedPhoneNumber' {status} -> status) (\s@OrderedPhoneNumber' {} a -> s {status = a} :: OrderedPhoneNumber)

-- | The phone number, in E.164 format.
orderedPhoneNumber_e164PhoneNumber :: Lens.Lens' OrderedPhoneNumber (Prelude.Maybe Prelude.Text)
orderedPhoneNumber_e164PhoneNumber = Lens.lens (\OrderedPhoneNumber' {e164PhoneNumber} -> e164PhoneNumber) (\s@OrderedPhoneNumber' {} a -> s {e164PhoneNumber = a} :: OrderedPhoneNumber) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON OrderedPhoneNumber where
  parseJSON =
    Core.withObject
      "OrderedPhoneNumber"
      ( \x ->
          OrderedPhoneNumber'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "E164PhoneNumber")
      )

instance Prelude.Hashable OrderedPhoneNumber

instance Prelude.NFData OrderedPhoneNumber
