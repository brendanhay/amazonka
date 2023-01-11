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
-- Module      : Amazonka.Pinpoint.Types.NumberValidateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.NumberValidateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a phone number to validate and retrieve information about.
--
-- /See:/ 'newNumberValidateRequest' smart constructor.
data NumberValidateRequest = NumberValidateRequest'
  { -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the phone number was originally registered.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The phone number to retrieve information about. The phone number that
    -- you provide should include a valid numeric country code. Otherwise, the
    -- operation might result in an error.
    phoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberValidateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isoCountryCode', 'numberValidateRequest_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
--
-- 'phoneNumber', 'numberValidateRequest_phoneNumber' - The phone number to retrieve information about. The phone number that
-- you provide should include a valid numeric country code. Otherwise, the
-- operation might result in an error.
newNumberValidateRequest ::
  NumberValidateRequest
newNumberValidateRequest =
  NumberValidateRequest'
    { isoCountryCode =
        Prelude.Nothing,
      phoneNumber = Prelude.Nothing
    }

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
numberValidateRequest_isoCountryCode :: Lens.Lens' NumberValidateRequest (Prelude.Maybe Prelude.Text)
numberValidateRequest_isoCountryCode = Lens.lens (\NumberValidateRequest' {isoCountryCode} -> isoCountryCode) (\s@NumberValidateRequest' {} a -> s {isoCountryCode = a} :: NumberValidateRequest)

-- | The phone number to retrieve information about. The phone number that
-- you provide should include a valid numeric country code. Otherwise, the
-- operation might result in an error.
numberValidateRequest_phoneNumber :: Lens.Lens' NumberValidateRequest (Prelude.Maybe Prelude.Text)
numberValidateRequest_phoneNumber = Lens.lens (\NumberValidateRequest' {phoneNumber} -> phoneNumber) (\s@NumberValidateRequest' {} a -> s {phoneNumber = a} :: NumberValidateRequest)

instance Prelude.Hashable NumberValidateRequest where
  hashWithSalt _salt NumberValidateRequest' {..} =
    _salt `Prelude.hashWithSalt` isoCountryCode
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData NumberValidateRequest where
  rnf NumberValidateRequest' {..} =
    Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf phoneNumber

instance Data.ToJSON NumberValidateRequest where
  toJSON NumberValidateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsoCountryCode" Data..=)
              Prelude.<$> isoCountryCode,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber
          ]
      )
