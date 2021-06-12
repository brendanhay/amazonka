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
-- Module      : Network.AWS.Glacier.Types.VaultNotificationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.VaultNotificationConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a vault\'s notification configuration.
--
-- /See:/ 'newVaultNotificationConfig' smart constructor.
data VaultNotificationConfig = VaultNotificationConfig'
  { -- | A list of one or more events for which Amazon S3 Glacier will send a
    -- notification to the specified Amazon SNS topic.
    events :: Core.Maybe [Core.Text],
    -- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon
    -- Resource Name (ARN).
    sNSTopic :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VaultNotificationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'vaultNotificationConfig_events' - A list of one or more events for which Amazon S3 Glacier will send a
-- notification to the specified Amazon SNS topic.
--
-- 'sNSTopic', 'vaultNotificationConfig_sNSTopic' - The Amazon Simple Notification Service (Amazon SNS) topic Amazon
-- Resource Name (ARN).
newVaultNotificationConfig ::
  VaultNotificationConfig
newVaultNotificationConfig =
  VaultNotificationConfig'
    { events = Core.Nothing,
      sNSTopic = Core.Nothing
    }

-- | A list of one or more events for which Amazon S3 Glacier will send a
-- notification to the specified Amazon SNS topic.
vaultNotificationConfig_events :: Lens.Lens' VaultNotificationConfig (Core.Maybe [Core.Text])
vaultNotificationConfig_events = Lens.lens (\VaultNotificationConfig' {events} -> events) (\s@VaultNotificationConfig' {} a -> s {events = a} :: VaultNotificationConfig) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Simple Notification Service (Amazon SNS) topic Amazon
-- Resource Name (ARN).
vaultNotificationConfig_sNSTopic :: Lens.Lens' VaultNotificationConfig (Core.Maybe Core.Text)
vaultNotificationConfig_sNSTopic = Lens.lens (\VaultNotificationConfig' {sNSTopic} -> sNSTopic) (\s@VaultNotificationConfig' {} a -> s {sNSTopic = a} :: VaultNotificationConfig)

instance Core.FromJSON VaultNotificationConfig where
  parseJSON =
    Core.withObject
      "VaultNotificationConfig"
      ( \x ->
          VaultNotificationConfig'
            Core.<$> (x Core..:? "Events" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SNSTopic")
      )

instance Core.Hashable VaultNotificationConfig

instance Core.NFData VaultNotificationConfig

instance Core.ToJSON VaultNotificationConfig where
  toJSON VaultNotificationConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Events" Core..=) Core.<$> events,
            ("SNSTopic" Core..=) Core.<$> sNSTopic
          ]
      )
