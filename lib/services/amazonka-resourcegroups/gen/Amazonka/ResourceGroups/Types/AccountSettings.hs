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
-- Module      : Amazonka.ResourceGroups.Types.AccountSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.GroupLifecycleEventsDesiredStatus
import Amazonka.ResourceGroups.Types.GroupLifecycleEventsStatus

-- | The Resource Groups settings for this Amazon Web Services account.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The desired target status of the group lifecycle events feature. If
    groupLifecycleEventsDesiredStatus :: Prelude.Maybe GroupLifecycleEventsDesiredStatus,
    -- | The current status of the group lifecycle events feature.
    groupLifecycleEventsStatus :: Prelude.Maybe GroupLifecycleEventsStatus,
    -- | The text of any error message occurs during an attempt to turn group
    -- lifecycle events on or off.
    groupLifecycleEventsStatusMessage :: Prelude.Maybe Prelude.Text
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
-- 'groupLifecycleEventsDesiredStatus', 'accountSettings_groupLifecycleEventsDesiredStatus' - The desired target status of the group lifecycle events feature. If
--
-- 'groupLifecycleEventsStatus', 'accountSettings_groupLifecycleEventsStatus' - The current status of the group lifecycle events feature.
--
-- 'groupLifecycleEventsStatusMessage', 'accountSettings_groupLifecycleEventsStatusMessage' - The text of any error message occurs during an attempt to turn group
-- lifecycle events on or off.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { groupLifecycleEventsDesiredStatus =
        Prelude.Nothing,
      groupLifecycleEventsStatus = Prelude.Nothing,
      groupLifecycleEventsStatusMessage = Prelude.Nothing
    }

-- | The desired target status of the group lifecycle events feature. If
accountSettings_groupLifecycleEventsDesiredStatus :: Lens.Lens' AccountSettings (Prelude.Maybe GroupLifecycleEventsDesiredStatus)
accountSettings_groupLifecycleEventsDesiredStatus = Lens.lens (\AccountSettings' {groupLifecycleEventsDesiredStatus} -> groupLifecycleEventsDesiredStatus) (\s@AccountSettings' {} a -> s {groupLifecycleEventsDesiredStatus = a} :: AccountSettings)

-- | The current status of the group lifecycle events feature.
accountSettings_groupLifecycleEventsStatus :: Lens.Lens' AccountSettings (Prelude.Maybe GroupLifecycleEventsStatus)
accountSettings_groupLifecycleEventsStatus = Lens.lens (\AccountSettings' {groupLifecycleEventsStatus} -> groupLifecycleEventsStatus) (\s@AccountSettings' {} a -> s {groupLifecycleEventsStatus = a} :: AccountSettings)

-- | The text of any error message occurs during an attempt to turn group
-- lifecycle events on or off.
accountSettings_groupLifecycleEventsStatusMessage :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_groupLifecycleEventsStatusMessage = Lens.lens (\AccountSettings' {groupLifecycleEventsStatusMessage} -> groupLifecycleEventsStatusMessage) (\s@AccountSettings' {} a -> s {groupLifecycleEventsStatusMessage = a} :: AccountSettings)

instance Data.FromJSON AccountSettings where
  parseJSON =
    Data.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Data..:? "GroupLifecycleEventsDesiredStatus")
            Prelude.<*> (x Data..:? "GroupLifecycleEventsStatus")
            Prelude.<*> (x Data..:? "GroupLifecycleEventsStatusMessage")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` groupLifecycleEventsDesiredStatus
      `Prelude.hashWithSalt` groupLifecycleEventsStatus
      `Prelude.hashWithSalt` groupLifecycleEventsStatusMessage

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf groupLifecycleEventsDesiredStatus
      `Prelude.seq` Prelude.rnf groupLifecycleEventsStatus
      `Prelude.seq` Prelude.rnf groupLifecycleEventsStatusMessage
