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
-- Module      : Amazonka.Chime.Types.AccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings related to the Amazon Chime account. This includes settings
-- that start or stop remote control of shared screens, or start or stop
-- the dial-out option in the Amazon Chime web application. For more
-- information about these settings, see
-- <https://docs.aws.amazon.com/chime/latest/ag/policies.html Use the Policies Page>
-- in the /Amazon Chime Administration Guide/.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | Setting that stops or starts remote control of shared screens during
    -- meetings.
    disableRemoteControl :: Prelude.Maybe Prelude.Bool,
    -- | Setting that allows meeting participants to choose the __Call me at a
    -- phone number__ option. For more information, see
    -- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
    enableDialOut :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableRemoteControl', 'accountSettings_disableRemoteControl' - Setting that stops or starts remote control of shared screens during
-- meetings.
--
-- 'enableDialOut', 'accountSettings_enableDialOut' - Setting that allows meeting participants to choose the __Call me at a
-- phone number__ option. For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { disableRemoteControl =
        Prelude.Nothing,
      enableDialOut = Prelude.Nothing
    }

-- | Setting that stops or starts remote control of shared screens during
-- meetings.
accountSettings_disableRemoteControl :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_disableRemoteControl = Lens.lens (\AccountSettings' {disableRemoteControl} -> disableRemoteControl) (\s@AccountSettings' {} a -> s {disableRemoteControl = a} :: AccountSettings)

-- | Setting that allows meeting participants to choose the __Call me at a
-- phone number__ option. For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
accountSettings_enableDialOut :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_enableDialOut = Lens.lens (\AccountSettings' {enableDialOut} -> enableDialOut) (\s@AccountSettings' {} a -> s {enableDialOut = a} :: AccountSettings)

instance Data.FromJSON AccountSettings where
  parseJSON =
    Data.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Data..:? "DisableRemoteControl")
            Prelude.<*> (x Data..:? "EnableDialOut")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` disableRemoteControl
      `Prelude.hashWithSalt` enableDialOut

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf disableRemoteControl `Prelude.seq`
      Prelude.rnf enableDialOut

instance Data.ToJSON AccountSettings where
  toJSON AccountSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisableRemoteControl" Data..=)
              Prelude.<$> disableRemoteControl,
            ("EnableDialOut" Data..=) Prelude.<$> enableDialOut
          ]
      )
