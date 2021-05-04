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
-- Module      : Network.AWS.Pinpoint.Types.NumberValidateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.NumberValidateRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a phone number to validate and retrieve information about.
--
-- /See:/ 'newNumberValidateRequest' smart constructor.
data NumberValidateRequest = NumberValidateRequest'
  { -- | The phone number to retrieve information about. The phone number that
    -- you provide should include a valid numeric country code. Otherwise, the
    -- operation might result in an error.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the phone number was originally registered.
    isoCountryCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NumberValidateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'numberValidateRequest_phoneNumber' - The phone number to retrieve information about. The phone number that
-- you provide should include a valid numeric country code. Otherwise, the
-- operation might result in an error.
--
-- 'isoCountryCode', 'numberValidateRequest_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
newNumberValidateRequest ::
  NumberValidateRequest
newNumberValidateRequest =
  NumberValidateRequest'
    { phoneNumber =
        Prelude.Nothing,
      isoCountryCode = Prelude.Nothing
    }

-- | The phone number to retrieve information about. The phone number that
-- you provide should include a valid numeric country code. Otherwise, the
-- operation might result in an error.
numberValidateRequest_phoneNumber :: Lens.Lens' NumberValidateRequest (Prelude.Maybe Prelude.Text)
numberValidateRequest_phoneNumber = Lens.lens (\NumberValidateRequest' {phoneNumber} -> phoneNumber) (\s@NumberValidateRequest' {} a -> s {phoneNumber = a} :: NumberValidateRequest)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
numberValidateRequest_isoCountryCode :: Lens.Lens' NumberValidateRequest (Prelude.Maybe Prelude.Text)
numberValidateRequest_isoCountryCode = Lens.lens (\NumberValidateRequest' {isoCountryCode} -> isoCountryCode) (\s@NumberValidateRequest' {} a -> s {isoCountryCode = a} :: NumberValidateRequest)

instance Prelude.Hashable NumberValidateRequest

instance Prelude.NFData NumberValidateRequest

instance Prelude.ToJSON NumberValidateRequest where
  toJSON NumberValidateRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PhoneNumber" Prelude..=) Prelude.<$> phoneNumber,
            ("IsoCountryCode" Prelude..=)
              Prelude.<$> isoCountryCode
          ]
      )
