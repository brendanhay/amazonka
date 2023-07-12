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
-- Module      : Amazonka.CodeGuruProfiler.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.NotificationConfiguration where

import Amazonka.CodeGuruProfiler.Types.Channel
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON NotificationConfiguration where
  parseJSON =
    Data.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Prelude.<$> (x Data..:? "channels")
      )

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` channels

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf channels
