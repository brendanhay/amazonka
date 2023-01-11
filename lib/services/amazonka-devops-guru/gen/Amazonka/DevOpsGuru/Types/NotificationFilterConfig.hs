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
-- Module      : Amazonka.DevOpsGuru.Types.NotificationFilterConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.NotificationFilterConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightSeverity
import Amazonka.DevOpsGuru.Types.NotificationMessageType
import qualified Amazonka.Prelude as Prelude

-- | The filter configurations for the Amazon SNS notification topic you use
-- with DevOps Guru. You can choose to specify which events or message
-- types to receive notifications for. You can also choose to specify which
-- severity levels to receive notifications for.
--
-- /See:/ 'newNotificationFilterConfig' smart constructor.
data NotificationFilterConfig = NotificationFilterConfig'
  { -- | The events that you want to receive notifications for. For example, you
    -- can choose to receive notifications only when the severity level is
    -- upgraded or a new insight is created.
    messageTypes :: Prelude.Maybe [NotificationMessageType],
    -- | The severity levels that you want to receive notifications for. For
    -- example, you can choose to receive notifications only for insights with
    -- @HIGH@ and @MEDIUM@ severity levels. For more information, see
    -- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>.
    severities :: Prelude.Maybe [InsightSeverity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotificationFilterConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageTypes', 'notificationFilterConfig_messageTypes' - The events that you want to receive notifications for. For example, you
-- can choose to receive notifications only when the severity level is
-- upgraded or a new insight is created.
--
-- 'severities', 'notificationFilterConfig_severities' - The severity levels that you want to receive notifications for. For
-- example, you can choose to receive notifications only for insights with
-- @HIGH@ and @MEDIUM@ severity levels. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>.
newNotificationFilterConfig ::
  NotificationFilterConfig
newNotificationFilterConfig =
  NotificationFilterConfig'
    { messageTypes =
        Prelude.Nothing,
      severities = Prelude.Nothing
    }

-- | The events that you want to receive notifications for. For example, you
-- can choose to receive notifications only when the severity level is
-- upgraded or a new insight is created.
notificationFilterConfig_messageTypes :: Lens.Lens' NotificationFilterConfig (Prelude.Maybe [NotificationMessageType])
notificationFilterConfig_messageTypes = Lens.lens (\NotificationFilterConfig' {messageTypes} -> messageTypes) (\s@NotificationFilterConfig' {} a -> s {messageTypes = a} :: NotificationFilterConfig) Prelude.. Lens.mapping Lens.coerced

-- | The severity levels that you want to receive notifications for. For
-- example, you can choose to receive notifications only for insights with
-- @HIGH@ and @MEDIUM@ severity levels. For more information, see
-- <https://docs.aws.amazon.com/devops-guru/latest/userguide/working-with-insights.html#understanding-insights-severities Understanding insight severities>.
notificationFilterConfig_severities :: Lens.Lens' NotificationFilterConfig (Prelude.Maybe [InsightSeverity])
notificationFilterConfig_severities = Lens.lens (\NotificationFilterConfig' {severities} -> severities) (\s@NotificationFilterConfig' {} a -> s {severities = a} :: NotificationFilterConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NotificationFilterConfig where
  parseJSON =
    Data.withObject
      "NotificationFilterConfig"
      ( \x ->
          NotificationFilterConfig'
            Prelude.<$> (x Data..:? "MessageTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Severities" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable NotificationFilterConfig where
  hashWithSalt _salt NotificationFilterConfig' {..} =
    _salt `Prelude.hashWithSalt` messageTypes
      `Prelude.hashWithSalt` severities

instance Prelude.NFData NotificationFilterConfig where
  rnf NotificationFilterConfig' {..} =
    Prelude.rnf messageTypes
      `Prelude.seq` Prelude.rnf severities

instance Data.ToJSON NotificationFilterConfig where
  toJSON NotificationFilterConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageTypes" Data..=) Prelude.<$> messageTypes,
            ("Severities" Data..=) Prelude.<$> severities
          ]
      )
