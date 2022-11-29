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
-- Module      : Amazonka.Connect.Types.ListPhoneNumbersSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ListPhoneNumbersSummary where

import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about phone numbers that have been claimed to your Amazon
-- Connect instance or traffic distribution group.
--
-- /See:/ 'newListPhoneNumbersSummary' smart constructor.
data ListPhoneNumbersSummary = ListPhoneNumbersSummary'
  { -- | The ISO country code.
    phoneNumberCountryCode :: Prelude.Maybe PhoneNumberCountryCode,
    -- | The Amazon Resource Name (ARN) of the phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The type of phone number.
    phoneNumberType :: Prelude.Maybe PhoneNumberType,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The phone number. Phone numbers are formatted
    -- @[+] [country code] [subscriber number including area code]@.
    phoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPhoneNumbersSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberCountryCode', 'listPhoneNumbersSummary_phoneNumberCountryCode' - The ISO country code.
--
-- 'phoneNumberArn', 'listPhoneNumbersSummary_phoneNumberArn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'phoneNumberType', 'listPhoneNumbersSummary_phoneNumberType' - The type of phone number.
--
-- 'targetArn', 'listPhoneNumbersSummary_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
--
-- 'phoneNumberId', 'listPhoneNumbersSummary_phoneNumberId' - A unique identifier for the phone number.
--
-- 'phoneNumber', 'listPhoneNumbersSummary_phoneNumber' - The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
newListPhoneNumbersSummary ::
  ListPhoneNumbersSummary
newListPhoneNumbersSummary =
  ListPhoneNumbersSummary'
    { phoneNumberCountryCode =
        Prelude.Nothing,
      phoneNumberArn = Prelude.Nothing,
      phoneNumberType = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      phoneNumber = Prelude.Nothing
    }

-- | The ISO country code.
listPhoneNumbersSummary_phoneNumberCountryCode :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe PhoneNumberCountryCode)
listPhoneNumbersSummary_phoneNumberCountryCode = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberCountryCode = a} :: ListPhoneNumbersSummary)

-- | The Amazon Resource Name (ARN) of the phone number.
listPhoneNumbersSummary_phoneNumberArn :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumberArn = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberArn} -> phoneNumberArn) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberArn = a} :: ListPhoneNumbersSummary)

-- | The type of phone number.
listPhoneNumbersSummary_phoneNumberType :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe PhoneNumberType)
listPhoneNumbersSummary_phoneNumberType = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberType} -> phoneNumberType) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberType = a} :: ListPhoneNumbersSummary)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
listPhoneNumbersSummary_targetArn :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_targetArn = Lens.lens (\ListPhoneNumbersSummary' {targetArn} -> targetArn) (\s@ListPhoneNumbersSummary' {} a -> s {targetArn = a} :: ListPhoneNumbersSummary)

-- | A unique identifier for the phone number.
listPhoneNumbersSummary_phoneNumberId :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumberId = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberId} -> phoneNumberId) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberId = a} :: ListPhoneNumbersSummary)

-- | The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
listPhoneNumbersSummary_phoneNumber :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumber = Lens.lens (\ListPhoneNumbersSummary' {phoneNumber} -> phoneNumber) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumber = a} :: ListPhoneNumbersSummary)

instance Core.FromJSON ListPhoneNumbersSummary where
  parseJSON =
    Core.withObject
      "ListPhoneNumbersSummary"
      ( \x ->
          ListPhoneNumbersSummary'
            Prelude.<$> (x Core..:? "PhoneNumberCountryCode")
            Prelude.<*> (x Core..:? "PhoneNumberArn")
            Prelude.<*> (x Core..:? "PhoneNumberType")
            Prelude.<*> (x Core..:? "TargetArn")
            Prelude.<*> (x Core..:? "PhoneNumberId")
            Prelude.<*> (x Core..:? "PhoneNumber")
      )

instance Prelude.Hashable ListPhoneNumbersSummary where
  hashWithSalt _salt ListPhoneNumbersSummary' {..} =
    _salt `Prelude.hashWithSalt` phoneNumberCountryCode
      `Prelude.hashWithSalt` phoneNumberArn
      `Prelude.hashWithSalt` phoneNumberType
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData ListPhoneNumbersSummary where
  rnf ListPhoneNumbersSummary' {..} =
    Prelude.rnf phoneNumberCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberType
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf phoneNumber
