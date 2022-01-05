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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | Setting that allows meeting participants to choose the __Call me at a
    -- phone number__ option. For more information, see
    -- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
    enableDialOut :: Prelude.Maybe Prelude.Bool,
    -- | Setting that stops or starts remote control of shared screens during
    -- meetings.
    disableRemoteControl :: Prelude.Maybe Prelude.Bool
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
-- 'enableDialOut', 'accountSettings_enableDialOut' - Setting that allows meeting participants to choose the __Call me at a
-- phone number__ option. For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
--
-- 'disableRemoteControl', 'accountSettings_disableRemoteControl' - Setting that stops or starts remote control of shared screens during
-- meetings.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { enableDialOut = Prelude.Nothing,
      disableRemoteControl = Prelude.Nothing
    }

-- | Setting that allows meeting participants to choose the __Call me at a
-- phone number__ option. For more information, see
-- <https://docs.aws.amazon.com/chime/latest/ug/chime-join-meeting.html Join a Meeting without the Amazon Chime App>.
accountSettings_enableDialOut :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_enableDialOut = Lens.lens (\AccountSettings' {enableDialOut} -> enableDialOut) (\s@AccountSettings' {} a -> s {enableDialOut = a} :: AccountSettings)

-- | Setting that stops or starts remote control of shared screens during
-- meetings.
accountSettings_disableRemoteControl :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Bool)
accountSettings_disableRemoteControl = Lens.lens (\AccountSettings' {disableRemoteControl} -> disableRemoteControl) (\s@AccountSettings' {} a -> s {disableRemoteControl = a} :: AccountSettings)

instance Core.FromJSON AccountSettings where
  parseJSON =
    Core.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Core..:? "EnableDialOut")
            Prelude.<*> (x Core..:? "DisableRemoteControl")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt `Prelude.hashWithSalt` enableDialOut
      `Prelude.hashWithSalt` disableRemoteControl

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf enableDialOut
      `Prelude.seq` Prelude.rnf disableRemoteControl

instance Core.ToJSON AccountSettings where
  toJSON AccountSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableDialOut" Core..=) Prelude.<$> enableDialOut,
            ("DisableRemoteControl" Core..=)
              Prelude.<$> disableRemoteControl
          ]
      )
