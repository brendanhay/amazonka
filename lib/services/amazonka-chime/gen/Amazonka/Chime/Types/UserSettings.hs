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
-- Module      : Amazonka.Chime.Types.UserSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.UserSettings where

import Amazonka.Chime.Types.TelephonySettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings associated with an Amazon Chime user, including inbound and
-- outbound calling and text messaging.
--
-- /See:/ 'newUserSettings' smart constructor.
data UserSettings = UserSettings'
  { -- | The telephony settings associated with the user.
    telephony :: TelephonySettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telephony', 'userSettings_telephony' - The telephony settings associated with the user.
newUserSettings ::
  -- | 'telephony'
  TelephonySettings ->
  UserSettings
newUserSettings pTelephony_ =
  UserSettings' {telephony = pTelephony_}

-- | The telephony settings associated with the user.
userSettings_telephony :: Lens.Lens' UserSettings TelephonySettings
userSettings_telephony = Lens.lens (\UserSettings' {telephony} -> telephony) (\s@UserSettings' {} a -> s {telephony = a} :: UserSettings)

instance Data.FromJSON UserSettings where
  parseJSON =
    Data.withObject
      "UserSettings"
      ( \x ->
          UserSettings' Prelude.<$> (x Data..: "Telephony")
      )

instance Prelude.Hashable UserSettings where
  hashWithSalt _salt UserSettings' {..} =
    _salt `Prelude.hashWithSalt` telephony

instance Prelude.NFData UserSettings where
  rnf UserSettings' {..} = Prelude.rnf telephony

instance Data.ToJSON UserSettings where
  toJSON UserSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Telephony" Data..= telephony)]
      )
