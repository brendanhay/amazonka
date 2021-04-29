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
-- Module      : Network.AWS.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NotificationProperty where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies configuration properties of a notification.
--
-- /See:/ 'newNotificationProperty' smart constructor.
data NotificationProperty = NotificationProperty'
  { -- | After a job run starts, the number of minutes to wait before sending a
    -- job run delay notification.
    notifyDelayAfter :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | After a job run starts, the number of minutes to wait before sending a
-- job run delay notification.
notificationProperty_notifyDelayAfter :: Lens.Lens' NotificationProperty (Prelude.Maybe Prelude.Natural)
notificationProperty_notifyDelayAfter = Lens.lens (\NotificationProperty' {notifyDelayAfter} -> notifyDelayAfter) (\s@NotificationProperty' {} a -> s {notifyDelayAfter = a} :: NotificationProperty)

instance Prelude.FromJSON NotificationProperty where
  parseJSON =
    Prelude.withObject
      "NotificationProperty"
      ( \x ->
          NotificationProperty'
            Prelude.<$> (x Prelude..:? "NotifyDelayAfter")
      )

instance Prelude.Hashable NotificationProperty

instance Prelude.NFData NotificationProperty

instance Prelude.ToJSON NotificationProperty where
  toJSON NotificationProperty' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotifyDelayAfter" Prelude..=)
              Prelude.<$> notifyDelayAfter
          ]
      )
