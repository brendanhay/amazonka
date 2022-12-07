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
-- Module      : Amazonka.Glue.Types.NotificationProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.NotificationProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration properties of a notification.
--
-- /See:/ 'newNotificationProperty' smart constructor.
data NotificationProperty = NotificationProperty'
  { -- | After a job run starts, the number of minutes to wait before sending a
    -- job run delay notification.
    notifyDelayAfter :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON NotificationProperty where
  parseJSON =
    Data.withObject
      "NotificationProperty"
      ( \x ->
          NotificationProperty'
            Prelude.<$> (x Data..:? "NotifyDelayAfter")
      )

instance Prelude.Hashable NotificationProperty where
  hashWithSalt _salt NotificationProperty' {..} =
    _salt `Prelude.hashWithSalt` notifyDelayAfter

instance Prelude.NFData NotificationProperty where
  rnf NotificationProperty' {..} =
    Prelude.rnf notifyDelayAfter

instance Data.ToJSON NotificationProperty where
  toJSON NotificationProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotifyDelayAfter" Data..=)
              Prelude.<$> notifyDelayAfter
          ]
      )
