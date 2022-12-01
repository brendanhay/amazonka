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
-- Module      : Amazonka.WellArchitected.Types.NotificationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.NotificationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.LensUpgradeSummary
import Amazonka.WellArchitected.Types.NotificationType

-- | A notification summary return object.
--
-- /See:/ 'newNotificationSummary' smart constructor.
data NotificationSummary = NotificationSummary'
  { -- | The type of notification.
    type' :: Prelude.Maybe NotificationType,
    -- | Summary of lens upgrade.
    lensUpgradeSummary :: Prelude.Maybe LensUpgradeSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'notificationSummary_type' - The type of notification.
--
-- 'lensUpgradeSummary', 'notificationSummary_lensUpgradeSummary' - Summary of lens upgrade.
newNotificationSummary ::
  NotificationSummary
newNotificationSummary =
  NotificationSummary'
    { type' = Prelude.Nothing,
      lensUpgradeSummary = Prelude.Nothing
    }

-- | The type of notification.
notificationSummary_type :: Lens.Lens' NotificationSummary (Prelude.Maybe NotificationType)
notificationSummary_type = Lens.lens (\NotificationSummary' {type'} -> type') (\s@NotificationSummary' {} a -> s {type' = a} :: NotificationSummary)

-- | Summary of lens upgrade.
notificationSummary_lensUpgradeSummary :: Lens.Lens' NotificationSummary (Prelude.Maybe LensUpgradeSummary)
notificationSummary_lensUpgradeSummary = Lens.lens (\NotificationSummary' {lensUpgradeSummary} -> lensUpgradeSummary) (\s@NotificationSummary' {} a -> s {lensUpgradeSummary = a} :: NotificationSummary)

instance Core.FromJSON NotificationSummary where
  parseJSON =
    Core.withObject
      "NotificationSummary"
      ( \x ->
          NotificationSummary'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "LensUpgradeSummary")
      )

instance Prelude.Hashable NotificationSummary where
  hashWithSalt _salt NotificationSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` lensUpgradeSummary

instance Prelude.NFData NotificationSummary where
  rnf NotificationSummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf lensUpgradeSummary
