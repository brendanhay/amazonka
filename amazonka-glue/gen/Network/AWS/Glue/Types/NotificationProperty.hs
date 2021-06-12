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
-- Module      : Network.AWS.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NotificationProperty where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies configuration properties of a notification.
--
-- /See:/ 'newNotificationProperty' smart constructor.
data NotificationProperty = NotificationProperty'
  { -- | After a job run starts, the number of minutes to wait before sending a
    -- job run delay notification.
    notifyDelayAfter :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotificationProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notifyDelayAfter', 'notificationProperty_notifyDelayAfter' - After a job run starts, the number of minutes to wait before sending a
-- job run delay notification.
newNotificationProperty ::
  NotificationProperty
newNotificationProperty =
  NotificationProperty'
    { notifyDelayAfter =
        Core.Nothing
    }

-- | After a job run starts, the number of minutes to wait before sending a
-- job run delay notification.
notificationProperty_notifyDelayAfter :: Lens.Lens' NotificationProperty (Core.Maybe Core.Natural)
notificationProperty_notifyDelayAfter = Lens.lens (\NotificationProperty' {notifyDelayAfter} -> notifyDelayAfter) (\s@NotificationProperty' {} a -> s {notifyDelayAfter = a} :: NotificationProperty)

instance Core.FromJSON NotificationProperty where
  parseJSON =
    Core.withObject
      "NotificationProperty"
      ( \x ->
          NotificationProperty'
            Core.<$> (x Core..:? "NotifyDelayAfter")
      )

instance Core.Hashable NotificationProperty

instance Core.NFData NotificationProperty

instance Core.ToJSON NotificationProperty where
  toJSON NotificationProperty' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotifyDelayAfter" Core..=)
              Core.<$> notifyDelayAfter
          ]
      )
