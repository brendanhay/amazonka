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
-- Module      : Amazonka.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.SyncConfig where

import Amazonka.AppSync.Types.ConflictDetectionType
import Amazonka.AppSync.Types.ConflictHandlerType
import Amazonka.AppSync.Types.LambdaConflictHandlerConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a Sync configuration for a resolver.
--
-- Specifies which Conflict Detection strategy and Resolution strategy to
-- use when the resolver is invoked.
--
-- /See:/ 'newSyncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { -- | The Conflict Detection strategy to use.
    --
    -- -   __VERSION__: Detect conflicts based on object versions for this
    --     resolver.
    --
    -- -   __NONE__: Do not detect conflicts when invoking this resolver.
    conflictDetection :: Prelude.Maybe ConflictDetectionType,
    -- | The Conflict Resolution strategy to perform in the event of a conflict.
    --
    -- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
    --     when versions don\'t match the latest version at the server.
    --
    -- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
    --     resolution strategy.
    --
    -- -   __LAMBDA__: Resolve conflicts with an Lambda function supplied in
    --     the @LambdaConflictHandlerConfig@.
    conflictHandler :: Prelude.Maybe ConflictHandlerType,
    -- | The @LambdaConflictHandlerConfig@ when configuring @LAMBDA@ as the
    -- Conflict Handler.
    lambdaConflictHandlerConfig :: Prelude.Maybe LambdaConflictHandlerConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictDetection', 'syncConfig_conflictDetection' - The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when invoking this resolver.
--
-- 'conflictHandler', 'syncConfig_conflictHandler' - The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions don\'t match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with an Lambda function supplied in
--     the @LambdaConflictHandlerConfig@.
--
-- 'lambdaConflictHandlerConfig', 'syncConfig_lambdaConflictHandlerConfig' - The @LambdaConflictHandlerConfig@ when configuring @LAMBDA@ as the
-- Conflict Handler.
newSyncConfig ::
  SyncConfig
newSyncConfig =
  SyncConfig'
    { conflictDetection = Prelude.Nothing,
      conflictHandler = Prelude.Nothing,
      lambdaConflictHandlerConfig = Prelude.Nothing
    }

-- | The Conflict Detection strategy to use.
--
-- -   __VERSION__: Detect conflicts based on object versions for this
--     resolver.
--
-- -   __NONE__: Do not detect conflicts when invoking this resolver.
syncConfig_conflictDetection :: Lens.Lens' SyncConfig (Prelude.Maybe ConflictDetectionType)
syncConfig_conflictDetection = Lens.lens (\SyncConfig' {conflictDetection} -> conflictDetection) (\s@SyncConfig' {} a -> s {conflictDetection = a} :: SyncConfig)

-- | The Conflict Resolution strategy to perform in the event of a conflict.
--
-- -   __OPTIMISTIC_CONCURRENCY__: Resolve conflicts by rejecting mutations
--     when versions don\'t match the latest version at the server.
--
-- -   __AUTOMERGE__: Resolve conflicts with the Automerge conflict
--     resolution strategy.
--
-- -   __LAMBDA__: Resolve conflicts with an Lambda function supplied in
--     the @LambdaConflictHandlerConfig@.
syncConfig_conflictHandler :: Lens.Lens' SyncConfig (Prelude.Maybe ConflictHandlerType)
syncConfig_conflictHandler = Lens.lens (\SyncConfig' {conflictHandler} -> conflictHandler) (\s@SyncConfig' {} a -> s {conflictHandler = a} :: SyncConfig)

-- | The @LambdaConflictHandlerConfig@ when configuring @LAMBDA@ as the
-- Conflict Handler.
syncConfig_lambdaConflictHandlerConfig :: Lens.Lens' SyncConfig (Prelude.Maybe LambdaConflictHandlerConfig)
syncConfig_lambdaConflictHandlerConfig = Lens.lens (\SyncConfig' {lambdaConflictHandlerConfig} -> lambdaConflictHandlerConfig) (\s@SyncConfig' {} a -> s {lambdaConflictHandlerConfig = a} :: SyncConfig)

instance Data.FromJSON SyncConfig where
  parseJSON =
    Data.withObject
      "SyncConfig"
      ( \x ->
          SyncConfig'
            Prelude.<$> (x Data..:? "conflictDetection")
            Prelude.<*> (x Data..:? "conflictHandler")
            Prelude.<*> (x Data..:? "lambdaConflictHandlerConfig")
      )

instance Prelude.Hashable SyncConfig where
  hashWithSalt _salt SyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` conflictDetection
      `Prelude.hashWithSalt` conflictHandler
      `Prelude.hashWithSalt` lambdaConflictHandlerConfig

instance Prelude.NFData SyncConfig where
  rnf SyncConfig' {..} =
    Prelude.rnf conflictDetection `Prelude.seq`
      Prelude.rnf conflictHandler `Prelude.seq`
        Prelude.rnf lambdaConflictHandlerConfig

instance Data.ToJSON SyncConfig where
  toJSON SyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("conflictDetection" Data..=)
              Prelude.<$> conflictDetection,
            ("conflictHandler" Data..=)
              Prelude.<$> conflictHandler,
            ("lambdaConflictHandlerConfig" Data..=)
              Prelude.<$> lambdaConflictHandlerConfig
          ]
      )
