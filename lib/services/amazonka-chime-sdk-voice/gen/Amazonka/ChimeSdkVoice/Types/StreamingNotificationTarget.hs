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
-- Module      : Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget where

import Amazonka.ChimeSdkVoice.Types.NotificationTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newStreamingNotificationTarget' smart constructor.
data StreamingNotificationTarget = StreamingNotificationTarget'
  { notificationTarget :: Prelude.Maybe NotificationTarget
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
-- 'notificationTarget', 'streamingNotificationTarget_notificationTarget' - Undocumented member.
newStreamingNotificationTarget ::
  StreamingNotificationTarget
newStreamingNotificationTarget =
  StreamingNotificationTarget'
    { notificationTarget =
        Prelude.Nothing
    }

-- | Undocumented member.
streamingNotificationTarget_notificationTarget :: Lens.Lens' StreamingNotificationTarget (Prelude.Maybe NotificationTarget)
streamingNotificationTarget_notificationTarget = Lens.lens (\StreamingNotificationTarget' {notificationTarget} -> notificationTarget) (\s@StreamingNotificationTarget' {} a -> s {notificationTarget = a} :: StreamingNotificationTarget)

instance Data.FromJSON StreamingNotificationTarget where
  parseJSON =
    Data.withObject
      "StreamingNotificationTarget"
      ( \x ->
          StreamingNotificationTarget'
            Prelude.<$> (x Data..:? "NotificationTarget")
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
          [ ("NotificationTarget" Data..=)
              Prelude.<$> notificationTarget
          ]
      )
