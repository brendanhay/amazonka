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
-- Module      : Amazonka.KinesisVideo.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ConfigurationStatus
import Amazonka.KinesisVideo.Types.NotificationDestinationConfig
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains the notification information for the KVS
-- images delivery. If this parameter is null, the configuration will be
-- deleted from the stream.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | Indicates if a notification configuration is enabled or disabled.
    status :: ConfigurationStatus,
    -- | The destination information required to deliver a notification to a
    -- customer.
    destinationConfig :: NotificationDestinationConfig
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
-- 'status', 'notificationConfiguration_status' - Indicates if a notification configuration is enabled or disabled.
--
-- 'destinationConfig', 'notificationConfiguration_destinationConfig' - The destination information required to deliver a notification to a
-- customer.
newNotificationConfiguration ::
  -- | 'status'
  ConfigurationStatus ->
  -- | 'destinationConfig'
  NotificationDestinationConfig ->
  NotificationConfiguration
newNotificationConfiguration
  pStatus_
  pDestinationConfig_ =
    NotificationConfiguration'
      { status = pStatus_,
        destinationConfig = pDestinationConfig_
      }

-- | Indicates if a notification configuration is enabled or disabled.
notificationConfiguration_status :: Lens.Lens' NotificationConfiguration ConfigurationStatus
notificationConfiguration_status = Lens.lens (\NotificationConfiguration' {status} -> status) (\s@NotificationConfiguration' {} a -> s {status = a} :: NotificationConfiguration)

-- | The destination information required to deliver a notification to a
-- customer.
notificationConfiguration_destinationConfig :: Lens.Lens' NotificationConfiguration NotificationDestinationConfig
notificationConfiguration_destinationConfig = Lens.lens (\NotificationConfiguration' {destinationConfig} -> destinationConfig) (\s@NotificationConfiguration' {} a -> s {destinationConfig = a} :: NotificationConfiguration)

instance Data.FromJSON NotificationConfiguration where
  parseJSON =
    Data.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Prelude.<$> (x Data..: "Status")
            Prelude.<*> (x Data..: "DestinationConfig")
      )

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` destinationConfig

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf destinationConfig

instance Data.ToJSON NotificationConfiguration where
  toJSON NotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Status" Data..= status),
            Prelude.Just
              ("DestinationConfig" Data..= destinationConfig)
          ]
      )
