{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains configuration settings for a quick connect.
--
-- /See:/ 'newQuickConnectConfig' smart constructor.
data QuickConnectConfig = QuickConnectConfig'
  { -- | The user configuration. This is required only if QuickConnectType is
    -- USER.
    userConfig :: Prelude.Maybe UserQuickConnectConfig,
    -- | The phone configuration. This is required only if QuickConnectType is
    -- PHONE_NUMBER.
    phoneConfig :: Prelude.Maybe PhoneNumberQuickConnectConfig,
    -- | The queue configuration. This is required only if QuickConnectType is
    -- QUEUE.
    queueConfig :: Prelude.Maybe QueueQuickConnectConfig,
    -- | The type of quick connect. In the Amazon Connect console, when you
    -- create a quick connect, you are prompted to assign one of the following
    -- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
    quickConnectType :: QuickConnectType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { userConfig = Prelude.Nothing,
      phoneConfig = Prelude.Nothing,
      queueConfig = Prelude.Nothing,
      quickConnectType = pQuickConnectType_
    }

-- | The user configuration. This is required only if QuickConnectType is
-- USER.
quickConnectConfig_userConfig :: Lens.Lens' QuickConnectConfig (Prelude.Maybe UserQuickConnectConfig)
quickConnectConfig_userConfig = Lens.lens (\QuickConnectConfig' {userConfig} -> userConfig) (\s@QuickConnectConfig' {} a -> s {userConfig = a} :: QuickConnectConfig)

-- | The phone configuration. This is required only if QuickConnectType is
-- PHONE_NUMBER.
quickConnectConfig_phoneConfig :: Lens.Lens' QuickConnectConfig (Prelude.Maybe PhoneNumberQuickConnectConfig)
quickConnectConfig_phoneConfig = Lens.lens (\QuickConnectConfig' {phoneConfig} -> phoneConfig) (\s@QuickConnectConfig' {} a -> s {phoneConfig = a} :: QuickConnectConfig)

-- | The queue configuration. This is required only if QuickConnectType is
-- QUEUE.
quickConnectConfig_queueConfig :: Lens.Lens' QuickConnectConfig (Prelude.Maybe QueueQuickConnectConfig)
quickConnectConfig_queueConfig = Lens.lens (\QuickConnectConfig' {queueConfig} -> queueConfig) (\s@QuickConnectConfig' {} a -> s {queueConfig = a} :: QuickConnectConfig)

-- | The type of quick connect. In the Amazon Connect console, when you
-- create a quick connect, you are prompted to assign one of the following
-- types: Agent (USER), External (PHONE_NUMBER), or Queue (QUEUE).
quickConnectConfig_quickConnectType :: Lens.Lens' QuickConnectConfig QuickConnectType
quickConnectConfig_quickConnectType = Lens.lens (\QuickConnectConfig' {quickConnectType} -> quickConnectType) (\s@QuickConnectConfig' {} a -> s {quickConnectType = a} :: QuickConnectConfig)

instance Prelude.FromJSON QuickConnectConfig where
  parseJSON =
    Prelude.withObject
      "QuickConnectConfig"
      ( \x ->
          QuickConnectConfig'
            Prelude.<$> (x Prelude..:? "UserConfig")
            Prelude.<*> (x Prelude..:? "PhoneConfig")
            Prelude.<*> (x Prelude..:? "QueueConfig")
            Prelude.<*> (x Prelude..: "QuickConnectType")
      )

instance Prelude.Hashable QuickConnectConfig

instance Prelude.NFData QuickConnectConfig

instance Prelude.ToJSON QuickConnectConfig where
  toJSON QuickConnectConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UserConfig" Prelude..=) Prelude.<$> userConfig,
            ("PhoneConfig" Prelude..=) Prelude.<$> phoneConfig,
            ("QueueConfig" Prelude..=) Prelude.<$> queueConfig,
            Prelude.Just
              ("QuickConnectType" Prelude..= quickConnectType)
          ]
      )
