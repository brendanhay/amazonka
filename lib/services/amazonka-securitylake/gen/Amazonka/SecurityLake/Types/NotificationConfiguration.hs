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
-- Module      : Amazonka.SecurityLake.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.HttpsNotificationConfiguration
import Amazonka.SecurityLake.Types.SqsNotificationConfiguration

-- | Specify the configurations you want to use for subscriber notification
-- to notify the subscriber when new data is written to the data lake for
-- sources that the subscriber consumes in Security Lake.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | The configurations for HTTPS subscriber notification.
    httpsNotificationConfiguration :: Prelude.Maybe HttpsNotificationConfiguration,
    -- | The configurations for SQS subscriber notification.
    sqsNotificationConfiguration :: Prelude.Maybe SqsNotificationConfiguration
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
-- 'httpsNotificationConfiguration', 'notificationConfiguration_httpsNotificationConfiguration' - The configurations for HTTPS subscriber notification.
--
-- 'sqsNotificationConfiguration', 'notificationConfiguration_sqsNotificationConfiguration' - The configurations for SQS subscriber notification.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { httpsNotificationConfiguration =
        Prelude.Nothing,
      sqsNotificationConfiguration = Prelude.Nothing
    }

-- | The configurations for HTTPS subscriber notification.
notificationConfiguration_httpsNotificationConfiguration :: Lens.Lens' NotificationConfiguration (Prelude.Maybe HttpsNotificationConfiguration)
notificationConfiguration_httpsNotificationConfiguration = Lens.lens (\NotificationConfiguration' {httpsNotificationConfiguration} -> httpsNotificationConfiguration) (\s@NotificationConfiguration' {} a -> s {httpsNotificationConfiguration = a} :: NotificationConfiguration)

-- | The configurations for SQS subscriber notification.
notificationConfiguration_sqsNotificationConfiguration :: Lens.Lens' NotificationConfiguration (Prelude.Maybe SqsNotificationConfiguration)
notificationConfiguration_sqsNotificationConfiguration = Lens.lens (\NotificationConfiguration' {sqsNotificationConfiguration} -> sqsNotificationConfiguration) (\s@NotificationConfiguration' {} a -> s {sqsNotificationConfiguration = a} :: NotificationConfiguration)

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` httpsNotificationConfiguration
      `Prelude.hashWithSalt` sqsNotificationConfiguration

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf httpsNotificationConfiguration
      `Prelude.seq` Prelude.rnf sqsNotificationConfiguration

instance Data.ToJSON NotificationConfiguration where
  toJSON NotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("httpsNotificationConfiguration" Data..=)
              Prelude.<$> httpsNotificationConfiguration,
            ("sqsNotificationConfiguration" Data..=)
              Prelude.<$> sqsNotificationConfiguration
          ]
      )
