{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.SyncConfig
  ( SyncConfig (..),

    -- * Smart constructor
    mkSyncConfig,

    -- * Lenses
    scConflictHandler,
    scConflictDetection,
    scLambdaConflictHandlerConfig,
  )
where

import Network.AWS.AppSync.Types.ConflictDetectionType
import Network.AWS.AppSync.Types.ConflictHandlerType
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Sync configuration for a resolver.
--
-- Contains information on which Conflict Detection as well as Resolution strategy should be performed when the resolver is invoked.
--
-- /See:/ 'mkSyncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { conflictHandler ::
      Lude.Maybe ConflictHandlerType,
    conflictDetection :: Lude.Maybe ConflictDetectionType,
    lambdaConflictHandlerConfig ::
      Lude.Maybe LambdaConflictHandlerConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SyncConfig' with the minimum fields required to make a request.
--
-- * 'conflictDetection' - The Conflict Detection strategy to use.
--
--
--     * __VERSION__ : Detect conflicts based on object versions for this resolver.
--
--
--     * __NONE__ : Do not detect conflicts when executing this resolver.
--
--
-- * 'conflictHandler' - The Conflict Resolution strategy to perform in the event of a conflict.
--
--
--     * __OPTIMISTIC_CONCURRENCY__ : Resolve conflicts by rejecting mutations when versions do not match the latest version at the server.
--
--
--     * __AUTOMERGE__ : Resolve conflicts with the Automerge conflict resolution strategy.
--
--
--     * __LAMBDA__ : Resolve conflicts with a Lambda function supplied in the LambdaConflictHandlerConfig.
--
--
-- * 'lambdaConflictHandlerConfig' - The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
mkSyncConfig ::
  SyncConfig
mkSyncConfig =
  SyncConfig'
    { conflictHandler = Lude.Nothing,
      conflictDetection = Lude.Nothing,
      lambdaConflictHandlerConfig = Lude.Nothing
    }

-- | The Conflict Resolution strategy to perform in the event of a conflict.
--
--
--     * __OPTIMISTIC_CONCURRENCY__ : Resolve conflicts by rejecting mutations when versions do not match the latest version at the server.
--
--
--     * __AUTOMERGE__ : Resolve conflicts with the Automerge conflict resolution strategy.
--
--
--     * __LAMBDA__ : Resolve conflicts with a Lambda function supplied in the LambdaConflictHandlerConfig.
--
--
--
-- /Note:/ Consider using 'conflictHandler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scConflictHandler :: Lens.Lens' SyncConfig (Lude.Maybe ConflictHandlerType)
scConflictHandler = Lens.lens (conflictHandler :: SyncConfig -> Lude.Maybe ConflictHandlerType) (\s a -> s {conflictHandler = a} :: SyncConfig)
{-# DEPRECATED scConflictHandler "Use generic-lens or generic-optics with 'conflictHandler' instead." #-}

-- | The Conflict Detection strategy to use.
--
--
--     * __VERSION__ : Detect conflicts based on object versions for this resolver.
--
--
--     * __NONE__ : Do not detect conflicts when executing this resolver.
--
--
--
-- /Note:/ Consider using 'conflictDetection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scConflictDetection :: Lens.Lens' SyncConfig (Lude.Maybe ConflictDetectionType)
scConflictDetection = Lens.lens (conflictDetection :: SyncConfig -> Lude.Maybe ConflictDetectionType) (\s a -> s {conflictDetection = a} :: SyncConfig)
{-# DEPRECATED scConflictDetection "Use generic-lens or generic-optics with 'conflictDetection' instead." #-}

-- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
--
-- /Note:/ Consider using 'lambdaConflictHandlerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scLambdaConflictHandlerConfig :: Lens.Lens' SyncConfig (Lude.Maybe LambdaConflictHandlerConfig)
scLambdaConflictHandlerConfig = Lens.lens (lambdaConflictHandlerConfig :: SyncConfig -> Lude.Maybe LambdaConflictHandlerConfig) (\s a -> s {lambdaConflictHandlerConfig = a} :: SyncConfig)
{-# DEPRECATED scLambdaConflictHandlerConfig "Use generic-lens or generic-optics with 'lambdaConflictHandlerConfig' instead." #-}

instance Lude.FromJSON SyncConfig where
  parseJSON =
    Lude.withObject
      "SyncConfig"
      ( \x ->
          SyncConfig'
            Lude.<$> (x Lude..:? "conflictHandler")
            Lude.<*> (x Lude..:? "conflictDetection")
            Lude.<*> (x Lude..:? "lambdaConflictHandlerConfig")
      )

instance Lude.ToJSON SyncConfig where
  toJSON SyncConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("conflictHandler" Lude..=) Lude.<$> conflictHandler,
            ("conflictDetection" Lude..=) Lude.<$> conflictDetection,
            ("lambdaConflictHandlerConfig" Lude..=)
              Lude.<$> lambdaConflictHandlerConfig
          ]
      )
