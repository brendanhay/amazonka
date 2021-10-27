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
-- Module      : Network.AWS.WellArchitected.Types.NotificationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.NotificationSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.LensUpgradeSummary
import Network.AWS.WellArchitected.Types.NotificationType

-- | A notification summary return object.
--
-- /See:/ 'newNotificationSummary' smart constructor.
data NotificationSummary = NotificationSummary'
  { -- | Summary of lens upgrade.
    lensUpgradeSummary :: Prelude.Maybe LensUpgradeSummary,
    -- | The type of notification.
    type' :: Prelude.Maybe NotificationType
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
-- 'lensUpgradeSummary', 'notificationSummary_lensUpgradeSummary' - Summary of lens upgrade.
--
-- 'type'', 'notificationSummary_type' - The type of notification.
newNotificationSummary ::
  NotificationSummary
newNotificationSummary =
  NotificationSummary'
    { lensUpgradeSummary =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Summary of lens upgrade.
notificationSummary_lensUpgradeSummary :: Lens.Lens' NotificationSummary (Prelude.Maybe LensUpgradeSummary)
notificationSummary_lensUpgradeSummary = Lens.lens (\NotificationSummary' {lensUpgradeSummary} -> lensUpgradeSummary) (\s@NotificationSummary' {} a -> s {lensUpgradeSummary = a} :: NotificationSummary)

-- | The type of notification.
notificationSummary_type :: Lens.Lens' NotificationSummary (Prelude.Maybe NotificationType)
notificationSummary_type = Lens.lens (\NotificationSummary' {type'} -> type') (\s@NotificationSummary' {} a -> s {type' = a} :: NotificationSummary)

instance Core.FromJSON NotificationSummary where
  parseJSON =
    Core.withObject
      "NotificationSummary"
      ( \x ->
          NotificationSummary'
            Prelude.<$> (x Core..:? "LensUpgradeSummary")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable NotificationSummary

instance Prelude.NFData NotificationSummary
