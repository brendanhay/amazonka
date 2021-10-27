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
-- Module      : Network.AWS.CodeGuruProfiler.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeGuruProfiler.Types.NotificationConfiguration where

import Network.AWS.CodeGuruProfiler.Types.Channel
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration for notifications stored for each profiling group.
-- This includes up to to two channels and a list of event publishers
-- associated with each channel.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | List of up to two channels to be used for sending notifications for
    -- events detected from the application profile.
    channels :: Prelude.Maybe (Prelude.NonEmpty Channel)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channels', 'notificationConfiguration_channels' - List of up to two channels to be used for sending notifications for
-- events detected from the application profile.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { channels =
        Prelude.Nothing
    }

-- | List of up to two channels to be used for sending notifications for
-- events detected from the application profile.
notificationConfiguration_channels :: Lens.Lens' NotificationConfiguration (Prelude.Maybe (Prelude.NonEmpty Channel))
notificationConfiguration_channels = Lens.lens (\NotificationConfiguration' {channels} -> channels) (\s@NotificationConfiguration' {} a -> s {channels = a} :: NotificationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON NotificationConfiguration where
  parseJSON =
    Core.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Prelude.<$> (x Core..:? "channels")
      )

instance Prelude.Hashable NotificationConfiguration

instance Prelude.NFData NotificationConfiguration
