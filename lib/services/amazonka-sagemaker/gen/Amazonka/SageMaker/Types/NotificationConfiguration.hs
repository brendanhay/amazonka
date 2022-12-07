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
-- Module      : Amazonka.SageMaker.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NotificationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures Amazon SNS notifications of available or expiring work items
-- for work teams.
--
-- /See:/ 'newNotificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { -- | The ARN for the Amazon SNS topic to which notifications should be
    -- published.
    notificationTopicArn :: Prelude.Maybe Prelude.Text
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
-- 'notificationTopicArn', 'notificationConfiguration_notificationTopicArn' - The ARN for the Amazon SNS topic to which notifications should be
-- published.
newNotificationConfiguration ::
  NotificationConfiguration
newNotificationConfiguration =
  NotificationConfiguration'
    { notificationTopicArn =
        Prelude.Nothing
    }

-- | The ARN for the Amazon SNS topic to which notifications should be
-- published.
notificationConfiguration_notificationTopicArn :: Lens.Lens' NotificationConfiguration (Prelude.Maybe Prelude.Text)
notificationConfiguration_notificationTopicArn = Lens.lens (\NotificationConfiguration' {notificationTopicArn} -> notificationTopicArn) (\s@NotificationConfiguration' {} a -> s {notificationTopicArn = a} :: NotificationConfiguration)

instance Data.FromJSON NotificationConfiguration where
  parseJSON =
    Data.withObject
      "NotificationConfiguration"
      ( \x ->
          NotificationConfiguration'
            Prelude.<$> (x Data..:? "NotificationTopicArn")
      )

instance Prelude.Hashable NotificationConfiguration where
  hashWithSalt _salt NotificationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` notificationTopicArn

instance Prelude.NFData NotificationConfiguration where
  rnf NotificationConfiguration' {..} =
    Prelude.rnf notificationTopicArn

instance Data.ToJSON NotificationConfiguration where
  toJSON NotificationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotificationTopicArn" Data..=)
              Prelude.<$> notificationTopicArn
          ]
      )
