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
-- Module      : Amazonka.TimeStreamQuery.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.SnsConfiguration

-- | Notification configuration for a scheduled query. A notification is sent
-- by Timestream when a scheduled query is created, its state is updated or
-- when it is deleted.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | Details on SNS configuration.
    snsConfiguration :: SnsConfiguration
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
-- 'snsConfiguration', 'notificationConfiguration_snsConfiguration' - Details on SNS configuration.
newNotificationConfiguration ::
  -- | 'snsConfiguration'
  SnsConfiguration ->
  NotificationConfiguration
newNotificationConfiguration pSnsConfiguration_ =
  NotificationConfiguration'
    { snsConfiguration =
        pSnsConfiguration_
    }

-- | Details on SNS configuration.
notificationConfiguration_snsConfiguration :: Lens.Lens' NotificationConfiguration SnsConfiguration
notificationConfiguration_snsConfiguration = Lens.lens (\NotificationConfiguration' {snsConfiguration} -> snsConfiguration) (\s@NotificationConfiguration' {} a -> s {snsConfiguration = a} :: NotificationConfiguration)

instance Data.FromJSON NotificationConfiguration where
  parseJSON =
    Data.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Prelude.<$> (x Data..: "SnsConfiguration")
      )

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` snsConfiguration

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf snsConfiguration

instance Data.ToJSON NotificationConfiguration where
  toJSON NotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SnsConfiguration" Data..= snsConfiguration)
          ]
      )
