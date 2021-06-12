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
-- Module      : Network.AWS.Connect.Types.PhoneNumberSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberSummary where

import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a phone number for a contact center.
--
-- /See:/ 'newPhoneNumberSummary' smart constructor.
data PhoneNumberSummary = PhoneNumberSummary'
  { -- | The phone number.
    phoneNumber :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the phone number.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the phone number.
    id :: Core.Maybe Core.Text,
    -- | The type of phone number.
    phoneNumberType :: Core.Maybe PhoneNumberType,
    -- | The ISO country code.
    phoneNumberCountryCode :: Core.Maybe PhoneNumberCountryCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PhoneNumberSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'phoneNumberSummary_phoneNumber' - The phone number.
--
-- 'arn', 'phoneNumberSummary_arn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'id', 'phoneNumberSummary_id' - The identifier of the phone number.
--
-- 'phoneNumberType', 'phoneNumberSummary_phoneNumberType' - The type of phone number.
--
-- 'phoneNumberCountryCode', 'phoneNumberSummary_phoneNumberCountryCode' - The ISO country code.
newPhoneNumberSummary ::
  PhoneNumberSummary
newPhoneNumberSummary =
  PhoneNumberSummary'
    { phoneNumber = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      phoneNumberType = Core.Nothing,
      phoneNumberCountryCode = Core.Nothing
    }

-- | The phone number.
phoneNumberSummary_phoneNumber :: Lens.Lens' PhoneNumberSummary (Core.Maybe Core.Text)
phoneNumberSummary_phoneNumber = Lens.lens (\PhoneNumberSummary' {phoneNumber} -> phoneNumber) (\s@PhoneNumberSummary' {} a -> s {phoneNumber = a} :: PhoneNumberSummary)

-- | The Amazon Resource Name (ARN) of the phone number.
phoneNumberSummary_arn :: Lens.Lens' PhoneNumberSummary (Core.Maybe Core.Text)
phoneNumberSummary_arn = Lens.lens (\PhoneNumberSummary' {arn} -> arn) (\s@PhoneNumberSummary' {} a -> s {arn = a} :: PhoneNumberSummary)

-- | The identifier of the phone number.
phoneNumberSummary_id :: Lens.Lens' PhoneNumberSummary (Core.Maybe Core.Text)
phoneNumberSummary_id = Lens.lens (\PhoneNumberSummary' {id} -> id) (\s@PhoneNumberSummary' {} a -> s {id = a} :: PhoneNumberSummary)

-- | The type of phone number.
phoneNumberSummary_phoneNumberType :: Lens.Lens' PhoneNumberSummary (Core.Maybe PhoneNumberType)
phoneNumberSummary_phoneNumberType = Lens.lens (\PhoneNumberSummary' {phoneNumberType} -> phoneNumberType) (\s@PhoneNumberSummary' {} a -> s {phoneNumberType = a} :: PhoneNumberSummary)

-- | The ISO country code.
phoneNumberSummary_phoneNumberCountryCode :: Lens.Lens' PhoneNumberSummary (Core.Maybe PhoneNumberCountryCode)
phoneNumberSummary_phoneNumberCountryCode = Lens.lens (\PhoneNumberSummary' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@PhoneNumberSummary' {} a -> s {phoneNumberCountryCode = a} :: PhoneNumberSummary)

instance Core.FromJSON PhoneNumberSummary where
  parseJSON =
    Core.withObject
      "PhoneNumberSummary"
      ( \x ->
          PhoneNumberSummary'
            Core.<$> (x Core..:? "PhoneNumber")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "PhoneNumberType")
            Core.<*> (x Core..:? "PhoneNumberCountryCode")
      )

instance Core.Hashable PhoneNumberSummary

instance Core.NFData PhoneNumberSummary
