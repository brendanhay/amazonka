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
-- Module      : Network.AWS.Connect.Types.QuickConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QuickConnectConfig where

import Network.AWS.Connect.Types.PhoneNumberQuickConnectConfig
import Network.AWS.Connect.Types.QueueQuickConnectConfig
import Network.AWS.Connect.Types.QuickConnectType
import Network.AWS.Connect.Types.UserQuickConnectConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains configuration settings for a quick connect.
--
-- /See:/ 'newQuickConnectConfig' smart constructor.
data QuickConnectConfig = QuickConnectConfig'
  { -- | The user configuration. This is required only if QuickConnectType is
    -- USER.
    userConfig :: Core.Maybe UserQuickConnectConfig,
    -- | The phone configuration. This is required only if QuickConnectType is
    -- PHONE_NUMBER.
    phoneConfig :: Core.Maybe PhoneNumberQuickConnectConfig,
    -- | The queue configuration. This is required only if QuickConnectType is
    -- QUEUE.
    queueConfig :: Core.Maybe QueueQuickConnectConfig,
    -- | The type of quick connect. In the Amazon Connect console, when you
    -- create a quick connect, you are prompted to assign one of the following
    -- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
    quickConnectType :: QuickConnectType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userConfig', 'quickConnectConfig_userConfig' - The user configuration. This is required only if QuickConnectType is
-- USER.
--
-- 'phoneConfig', 'quickConnectConfig_phoneConfig' - The phone configuration. This is required only if QuickConnectType is
-- PHONE_NUMBER.
--
-- 'queueConfig', 'quickConnectConfig_queueConfig' - The queue configuration. This is required only if QuickConnectType is
-- QUEUE.
--
-- 'quickConnectType', 'quickConnectConfig_quickConnectType' - The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
newQuickConnectConfig ::
  -- | 'quickConnectType'
  QuickConnectType ->
  QuickConnectConfig
newQuickConnectConfig pQuickConnectType_ =
  QuickConnectConfig'
    { userConfig = Core.Nothing,
      phoneConfig = Core.Nothing,
      queueConfig = Core.Nothing,
      quickConnectType = pQuickConnectType_
    }

-- | The user configuration. This is required only if QuickConnectType is
-- USER.
quickConnectConfig_userConfig :: Lens.Lens' QuickConnectConfig (Core.Maybe UserQuickConnectConfig)
quickConnectConfig_userConfig = Lens.lens (\QuickConnectConfig' {userConfig} -> userConfig) (\s@QuickConnectConfig' {} a -> s {userConfig = a} :: QuickConnectConfig)

-- | The phone configuration. This is required only if QuickConnectType is
-- PHONE_NUMBER.
quickConnectConfig_phoneConfig :: Lens.Lens' QuickConnectConfig (Core.Maybe PhoneNumberQuickConnectConfig)
quickConnectConfig_phoneConfig = Lens.lens (\QuickConnectConfig' {phoneConfig} -> phoneConfig) (\s@QuickConnectConfig' {} a -> s {phoneConfig = a} :: QuickConnectConfig)

-- | The queue configuration. This is required only if QuickConnectType is
-- QUEUE.
quickConnectConfig_queueConfig :: Lens.Lens' QuickConnectConfig (Core.Maybe QueueQuickConnectConfig)
quickConnectConfig_queueConfig = Lens.lens (\QuickConnectConfig' {queueConfig} -> queueConfig) (\s@QuickConnectConfig' {} a -> s {queueConfig = a} :: QuickConnectConfig)

-- | The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
quickConnectConfig_quickConnectType :: Lens.Lens' QuickConnectConfig QuickConnectType
quickConnectConfig_quickConnectType = Lens.lens (\QuickConnectConfig' {quickConnectType} -> quickConnectType) (\s@QuickConnectConfig' {} a -> s {quickConnectType = a} :: QuickConnectConfig)

instance Core.FromJSON QuickConnectConfig where
  parseJSON =
    Core.withObject
      "QuickConnectConfig"
      ( \x ->
          QuickConnectConfig'
            Core.<$> (x Core..:? "UserConfig")
            Core.<*> (x Core..:? "PhoneConfig")
            Core.<*> (x Core..:? "QueueConfig")
            Core.<*> (x Core..: "QuickConnectType")
      )

instance Core.Hashable QuickConnectConfig

instance Core.NFData QuickConnectConfig

instance Core.ToJSON QuickConnectConfig where
  toJSON QuickConnectConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserConfig" Core..=) Core.<$> userConfig,
            ("PhoneConfig" Core..=) Core.<$> phoneConfig,
            ("QueueConfig" Core..=) Core.<$> queueConfig,
            Core.Just
              ("QuickConnectType" Core..= quickConnectType)
          ]
      )
