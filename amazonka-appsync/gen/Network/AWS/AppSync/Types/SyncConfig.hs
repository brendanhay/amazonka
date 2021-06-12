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
-- Module      : Network.AWS.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.SyncConfig where

import Network.AWS.AppSync.Types.ConflictDetectionType
import Network.AWS.AppSync.Types.ConflictHandlerType
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a Sync configuration for a resolver.
--
-- Contains information on which Conflict Detection as well as Resolution
-- strategy should be performed when the resolver is invoked.
--
-- /See:/ 'newSyncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { -- | The Conflict Resolution strategy to perform in the event of a conflict.
    --
    -- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
    --     when versions do not match the latest version at the server.
    --
    -- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
    --     resolution strategy.
    --
    -- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
    --     LambdaConflictHandlerConfig.
    conflictHandler :: Core.Maybe ConflictHandlerType,
    -- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
    -- Conflict Handler.
    lambdaConflictHandlerConfig :: Core.Maybe LambdaConflictHandlerConfig,
    -- | The Conflict Detection strategy to use.
    --
    -- -   __VERSION__: Detect conflicts based on object versions for this
    --     resolver.
    --
    -- -   __NONE__: Do not detect conflicts when executing this resolver.
    conflictDetection :: Core.Maybe ConflictDetectionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictHandler', 'syncConfig_conflictHandler' - The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions do not match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
--     LambdaConflictHandlerConfig.
--
-- 'lambdaConflictHandlerConfig', 'syncConfig_lambdaConflictHandlerConfig' - The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
-- Conflict Handler.
--
-- 'conflictDetection', 'syncConfig_conflictDetection' - The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when executing this resolver.
newSyncConfig ::
  SyncConfig
newSyncConfig =
  SyncConfig'
    { conflictHandler = Core.Nothing,
      lambdaConflictHandlerConfig = Core.Nothing,
      conflictDetection = Core.Nothing
    }

-- | The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions do not match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with a Lambda function supplied in the
--     LambdaConflictHandlerConfig.
syncConfig_conflictHandler :: Lens.Lens' SyncConfig (Core.Maybe ConflictHandlerType)
syncConfig_conflictHandler = Lens.lens (\SyncConfig' {conflictHandler} -> conflictHandler) (\s@SyncConfig' {} a -> s {conflictHandler = a} :: SyncConfig)

-- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the
-- Conflict Handler.
syncConfig_lambdaConflictHandlerConfig :: Lens.Lens' SyncConfig (Core.Maybe LambdaConflictHandlerConfig)
syncConfig_lambdaConflictHandlerConfig = Lens.lens (\SyncConfig' {lambdaConflictHandlerConfig} -> lambdaConflictHandlerConfig) (\s@SyncConfig' {} a -> s {lambdaConflictHandlerConfig = a} :: SyncConfig)

-- | The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when executing this resolver.
syncConfig_conflictDetection :: Lens.Lens' SyncConfig (Core.Maybe ConflictDetectionType)
syncConfig_conflictDetection = Lens.lens (\SyncConfig' {conflictDetection} -> conflictDetection) (\s@SyncConfig' {} a -> s {conflictDetection = a} :: SyncConfig)

instance Core.FromJSON SyncConfig where
  parseJSON =
    Core.withObject
      "SyncConfig"
      ( \x ->
          SyncConfig'
            Core.<$> (x Core..:? "conflictHandler")
            Core.<*> (x Core..:? "lambdaConflictHandlerConfig")
            Core.<*> (x Core..:? "conflictDetection")
      )

instance Core.Hashable SyncConfig

instance Core.NFData SyncConfig

instance Core.ToJSON SyncConfig where
  toJSON SyncConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("conflictHandler" Core..=)
              Core.<$> conflictHandler,
            ("lambdaConflictHandlerConfig" Core..=)
              Core.<$> lambdaConflictHandlerConfig,
            ("conflictDetection" Core..=)
              Core.<$> conflictDetection
          ]
      )
