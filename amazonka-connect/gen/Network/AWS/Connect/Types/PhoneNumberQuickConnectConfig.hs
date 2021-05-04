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
-- Module      : Network.AWS.Connect.Types.PhoneNumberQuickConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberQuickConnectConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a phone number for a quick connect.
--
-- /See:/ 'newPhoneNumberQuickConnectConfig' smart constructor.
data PhoneNumberQuickConnectConfig = PhoneNumberQuickConnectConfig'
  { -- | The phone number in E.164 format.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberQuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'phoneNumberQuickConnectConfig_phoneNumber' - The phone number in E.164 format.
newPhoneNumberQuickConnectConfig ::
  -- | 'phoneNumber'
  Prelude.Text ->
  PhoneNumberQuickConnectConfig
newPhoneNumberQuickConnectConfig pPhoneNumber_ =
  PhoneNumberQuickConnectConfig'
    { phoneNumber =
        pPhoneNumber_
    }

-- | The phone number in E.164 format.
phoneNumberQuickConnectConfig_phoneNumber :: Lens.Lens' PhoneNumberQuickConnectConfig Prelude.Text
phoneNumberQuickConnectConfig_phoneNumber = Lens.lens (\PhoneNumberQuickConnectConfig' {phoneNumber} -> phoneNumber) (\s@PhoneNumberQuickConnectConfig' {} a -> s {phoneNumber = a} :: PhoneNumberQuickConnectConfig)

instance
  Prelude.FromJSON
    PhoneNumberQuickConnectConfig
  where
  parseJSON =
    Prelude.withObject
      "PhoneNumberQuickConnectConfig"
      ( \x ->
          PhoneNumberQuickConnectConfig'
            Prelude.<$> (x Prelude..: "PhoneNumber")
      )

instance
  Prelude.Hashable
    PhoneNumberQuickConnectConfig

instance Prelude.NFData PhoneNumberQuickConnectConfig

instance Prelude.ToJSON PhoneNumberQuickConnectConfig where
  toJSON PhoneNumberQuickConnectConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PhoneNumber" Prelude..= phoneNumber)
          ]
      )
