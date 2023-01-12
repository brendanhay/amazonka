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
-- Module      : Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumber where

import Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumberStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newOrderedPhoneNumber' smart constructor.
data OrderedPhoneNumber = OrderedPhoneNumber'
  { e164PhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    status :: Prelude.Maybe OrderedPhoneNumberStatus
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
-- 'e164PhoneNumber', 'orderedPhoneNumber_e164PhoneNumber' - Undocumented member.
--
-- 'status', 'orderedPhoneNumber_status' - Undocumented member.
newOrderedPhoneNumber ::
  OrderedPhoneNumber
newOrderedPhoneNumber =
  OrderedPhoneNumber'
    { e164PhoneNumber =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Undocumented member.
orderedPhoneNumber_e164PhoneNumber :: Lens.Lens' OrderedPhoneNumber (Prelude.Maybe Prelude.Text)
orderedPhoneNumber_e164PhoneNumber = Lens.lens (\OrderedPhoneNumber' {e164PhoneNumber} -> e164PhoneNumber) (\s@OrderedPhoneNumber' {} a -> s {e164PhoneNumber = a} :: OrderedPhoneNumber) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
orderedPhoneNumber_status :: Lens.Lens' OrderedPhoneNumber (Prelude.Maybe OrderedPhoneNumberStatus)
orderedPhoneNumber_status = Lens.lens (\OrderedPhoneNumber' {status} -> status) (\s@OrderedPhoneNumber' {} a -> s {status = a} :: OrderedPhoneNumber)

instance Data.FromJSON OrderedPhoneNumber where
  parseJSON =
    Data.withObject
      "OrderedPhoneNumber"
      ( \x ->
          OrderedPhoneNumber'
            Prelude.<$> (x Data..:? "E164PhoneNumber")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable OrderedPhoneNumber where
  hashWithSalt _salt OrderedPhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` e164PhoneNumber
      `Prelude.hashWithSalt` status

instance Prelude.NFData OrderedPhoneNumber where
  rnf OrderedPhoneNumber' {..} =
    Prelude.rnf e164PhoneNumber
      `Prelude.seq` Prelude.rnf status
