{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.SyncConfig
  ( SyncConfig (..)
  -- * Smart constructor
  , mkSyncConfig
  -- * Lenses
  , scConflictDetection
  , scConflictHandler
  , scLambdaConflictHandlerConfig
  ) where

import qualified Network.AWS.AppSync.Types.ConflictDetectionType as Types
import qualified Network.AWS.AppSync.Types.ConflictHandlerType as Types
import qualified Network.AWS.AppSync.Types.LambdaConflictHandlerConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Sync configuration for a resolver.
--
-- Contains information on which Conflict Detection as well as Resolution strategy should be performed when the resolver is invoked.
--
-- /See:/ 'mkSyncConfig' smart constructor.
data SyncConfig = SyncConfig'
  { conflictDetection :: Core.Maybe Types.ConflictDetectionType
    -- ^ The Conflict Detection strategy to use.
--
--
--     * __VERSION__ : Detect conflicts based on object versions for this resolver.
--
--
--     * __NONE__ : Do not detect conflicts when executing this resolver.
--
--
  , conflictHandler :: Core.Maybe Types.ConflictHandlerType
    -- ^ The Conflict Resolution strategy to perform in the event of a conflict.
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
  , lambdaConflictHandlerConfig :: Core.Maybe Types.LambdaConflictHandlerConfig
    -- ^ The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SyncConfig' value with any optional fields omitted.
mkSyncConfig
    :: SyncConfig
mkSyncConfig
  = SyncConfig'{conflictDetection = Core.Nothing,
                conflictHandler = Core.Nothing,
                lambdaConflictHandlerConfig = Core.Nothing}

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
scConflictDetection :: Lens.Lens' SyncConfig (Core.Maybe Types.ConflictDetectionType)
scConflictDetection = Lens.field @"conflictDetection"
{-# INLINEABLE scConflictDetection #-}
{-# DEPRECATED conflictDetection "Use generic-lens or generic-optics with 'conflictDetection' instead"  #-}

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
scConflictHandler :: Lens.Lens' SyncConfig (Core.Maybe Types.ConflictHandlerType)
scConflictHandler = Lens.field @"conflictHandler"
{-# INLINEABLE scConflictHandler #-}
{-# DEPRECATED conflictHandler "Use generic-lens or generic-optics with 'conflictHandler' instead"  #-}

-- | The @LambdaConflictHandlerConfig@ when configuring LAMBDA as the Conflict Handler.
--
-- /Note:/ Consider using 'lambdaConflictHandlerConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scLambdaConflictHandlerConfig :: Lens.Lens' SyncConfig (Core.Maybe Types.LambdaConflictHandlerConfig)
scLambdaConflictHandlerConfig = Lens.field @"lambdaConflictHandlerConfig"
{-# INLINEABLE scLambdaConflictHandlerConfig #-}
{-# DEPRECATED lambdaConflictHandlerConfig "Use generic-lens or generic-optics with 'lambdaConflictHandlerConfig' instead"  #-}

instance Core.FromJSON SyncConfig where
        toJSON SyncConfig{..}
          = Core.object
              (Core.catMaybes
                 [("conflictDetection" Core..=) Core.<$> conflictDetection,
                  ("conflictHandler" Core..=) Core.<$> conflictHandler,
                  ("lambdaConflictHandlerConfig" Core..=) Core.<$>
                    lambdaConflictHandlerConfig])

instance Core.FromJSON SyncConfig where
        parseJSON
          = Core.withObject "SyncConfig" Core.$
              \ x ->
                SyncConfig' Core.<$>
                  (x Core..:? "conflictDetection") Core.<*>
                    x Core..:? "conflictHandler"
                    Core.<*> x Core..:? "lambdaConflictHandlerConfig"
