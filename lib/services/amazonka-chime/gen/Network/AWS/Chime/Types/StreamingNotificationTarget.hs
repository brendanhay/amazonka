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
-- Module      : Network.AWS.Chime.Types.StreamingNotificationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.StreamingNotificationTarget where

import Network.AWS.Chime.Types.NotificationTarget
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON StreamingNotificationTarget where
  parseJSON =
    Core.withObject
      "StreamingNotificationTarget"
      ( \x ->
          StreamingNotificationTarget'
            Prelude.<$> (x Core..: "NotificationTarget")
      )

instance Prelude.Hashable StreamingNotificationTarget

instance Prelude.NFData StreamingNotificationTarget

instance Core.ToJSON StreamingNotificationTarget where
  toJSON StreamingNotificationTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NotificationTarget" Core..= notificationTarget)
          ]
      )
