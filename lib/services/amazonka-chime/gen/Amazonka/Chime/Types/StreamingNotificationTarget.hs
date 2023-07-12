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
-- Module      : Amazonka.Chime.Types.StreamingNotificationTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.StreamingNotificationTarget where

import Amazonka.Chime.Types.NotificationTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The targeted recipient for a streaming configuration notification.
--
-- /See:/ 'newStreamingNotificationTarget' smart constructor.
data StreamingNotificationTarget = StreamingNotificationTarget'
  { -- | The streaming notification target.
    notificationTarget :: NotificationTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamingNotificationTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationTarget', 'streamingNotificationTarget_notificationTarget' - The streaming notification target.
newStreamingNotificationTarget ::
  -- | 'notificationTarget'
  NotificationTarget ->
  StreamingNotificationTarget
newStreamingNotificationTarget pNotificationTarget_ =
  StreamingNotificationTarget'
    { notificationTarget =
        pNotificationTarget_
    }

-- | The streaming notification target.
streamingNotificationTarget_notificationTarget :: Lens.Lens' StreamingNotificationTarget NotificationTarget
streamingNotificationTarget_notificationTarget = Lens.lens (\StreamingNotificationTarget' {notificationTarget} -> notificationTarget) (\s@StreamingNotificationTarget' {} a -> s {notificationTarget = a} :: StreamingNotificationTarget)

instance Data.FromJSON StreamingNotificationTarget where
  parseJSON =
    Data.withObject
      "StreamingNotificationTarget"
      ( \x ->
          StreamingNotificationTarget'
            Prelude.<$> (x Data..: "NotificationTarget")
      )

instance Prelude.Hashable StreamingNotificationTarget where
  hashWithSalt _salt StreamingNotificationTarget' {..} =
    _salt `Prelude.hashWithSalt` notificationTarget

instance Prelude.NFData StreamingNotificationTarget where
  rnf StreamingNotificationTarget' {..} =
    Prelude.rnf notificationTarget

instance Data.ToJSON StreamingNotificationTarget where
  toJSON StreamingNotificationTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NotificationTarget" Data..= notificationTarget)
          ]
      )
