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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ListPhoneNumbersSummary where

import Amazonka.Connect.Types.PhoneNumberCountryCode
import Amazonka.Connect.Types.PhoneNumberType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about phone numbers that have been claimed to your Amazon
-- Connect instance or traffic distribution group.
--
-- /See:/ 'newListPhoneNumbersSummary' smart constructor.
data ListPhoneNumbersSummary = ListPhoneNumbersSummary'
  { -- | The phone number. Phone numbers are formatted
    -- @[+] [country code] [subscriber number including area code]@.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | The ISO country code.
    phoneNumberCountryCode :: Prelude.Maybe PhoneNumberCountryCode,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The type of phone number.
    phoneNumberType :: Prelude.Maybe PhoneNumberType,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Maybe Prelude.Text
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
-- 'phoneNumber', 'listPhoneNumbersSummary_phoneNumber' - The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
--
-- 'phoneNumberArn', 'listPhoneNumbersSummary_phoneNumberArn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'phoneNumberCountryCode', 'listPhoneNumbersSummary_phoneNumberCountryCode' - The ISO country code.
--
-- 'phoneNumberId', 'listPhoneNumbersSummary_phoneNumberId' - A unique identifier for the phone number.
--
-- 'phoneNumberType', 'listPhoneNumbersSummary_phoneNumberType' - The type of phone number.
--
-- 'targetArn', 'listPhoneNumbersSummary_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
newListPhoneNumbersSummary ::
  ListPhoneNumbersSummary
newListPhoneNumbersSummary =
  ListPhoneNumbersSummary'
    { phoneNumber =
        Prelude.Nothing,
      phoneNumberArn = Prelude.Nothing,
      phoneNumberCountryCode = Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      phoneNumberType = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

-- | The phone number. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
listPhoneNumbersSummary_phoneNumber :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumber = Lens.lens (\ListPhoneNumbersSummary' {phoneNumber} -> phoneNumber) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumber = a} :: ListPhoneNumbersSummary)

-- | The Amazon Resource Name (ARN) of the phone number.
listPhoneNumbersSummary_phoneNumberArn :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumberArn = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberArn} -> phoneNumberArn) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberArn = a} :: ListPhoneNumbersSummary)

-- | The ISO country code.
listPhoneNumbersSummary_phoneNumberCountryCode :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe PhoneNumberCountryCode)
listPhoneNumbersSummary_phoneNumberCountryCode = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberCountryCode} -> phoneNumberCountryCode) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberCountryCode = a} :: ListPhoneNumbersSummary)

-- | A unique identifier for the phone number.
listPhoneNumbersSummary_phoneNumberId :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_phoneNumberId = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberId} -> phoneNumberId) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberId = a} :: ListPhoneNumbersSummary)

-- | The type of phone number.
listPhoneNumbersSummary_phoneNumberType :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe PhoneNumberType)
listPhoneNumbersSummary_phoneNumberType = Lens.lens (\ListPhoneNumbersSummary' {phoneNumberType} -> phoneNumberType) (\s@ListPhoneNumbersSummary' {} a -> s {phoneNumberType = a} :: ListPhoneNumbersSummary)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
listPhoneNumbersSummary_targetArn :: Lens.Lens' ListPhoneNumbersSummary (Prelude.Maybe Prelude.Text)
listPhoneNumbersSummary_targetArn = Lens.lens (\ListPhoneNumbersSummary' {targetArn} -> targetArn) (\s@ListPhoneNumbersSummary' {} a -> s {targetArn = a} :: ListPhoneNumbersSummary)

instance Data.FromJSON ListPhoneNumbersSummary where
  parseJSON =
    Data.withObject
      "ListPhoneNumbersSummary"
      ( \x ->
          ListPhoneNumbersSummary'
            Prelude.<$> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "PhoneNumberArn")
            Prelude.<*> (x Data..:? "PhoneNumberCountryCode")
            Prelude.<*> (x Data..:? "PhoneNumberId")
            Prelude.<*> (x Data..:? "PhoneNumberType")
            Prelude.<*> (x Data..:? "TargetArn")
      )

instance Prelude.Hashable ListPhoneNumbersSummary where
  hashWithSalt _salt ListPhoneNumbersSummary' {..} =
    _salt
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` phoneNumberArn
      `Prelude.hashWithSalt` phoneNumberCountryCode
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` phoneNumberType
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData ListPhoneNumbersSummary where
  rnf ListPhoneNumbersSummary' {..} =
    Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberCountryCode
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf phoneNumberType
      `Prelude.seq` Prelude.rnf targetArn
