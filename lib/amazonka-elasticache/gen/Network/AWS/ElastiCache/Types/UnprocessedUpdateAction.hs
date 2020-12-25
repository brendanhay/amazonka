{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
  ( UnprocessedUpdateAction (..),

    -- * Smart constructor
    mkUnprocessedUpdateAction,

    -- * Lenses
    uuaCacheClusterId,
    uuaErrorMessage,
    uuaErrorType,
    uuaReplicationGroupId,
    uuaServiceUpdateName,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Update action that has failed to be processed for the corresponding apply/stop request
--
-- /See:/ 'mkUnprocessedUpdateAction' smart constructor.
data UnprocessedUpdateAction = UnprocessedUpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Core.Maybe Types.String,
    -- | The error message that describes the reason the request was not processed
    errorMessage :: Core.Maybe Types.String,
    -- | The error type for requests that are not processed
    errorType :: Core.Maybe Types.String,
    -- | The replication group ID
    replicationGroupId :: Core.Maybe Types.String,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedUpdateAction' value with any optional fields omitted.
mkUnprocessedUpdateAction ::
  UnprocessedUpdateAction
mkUnprocessedUpdateAction =
  UnprocessedUpdateAction'
    { cacheClusterId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorType = Core.Nothing,
      replicationGroupId = Core.Nothing,
      serviceUpdateName = Core.Nothing
    }

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaCacheClusterId :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Types.String)
uuaCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED uuaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The error message that describes the reason the request was not processed
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorMessage :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Types.String)
uuaErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED uuaErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The error type for requests that are not processed
--
-- /Note:/ Consider using 'errorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorType :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Types.String)
uuaErrorType = Lens.field @"errorType"
{-# DEPRECATED uuaErrorType "Use generic-lens or generic-optics with 'errorType' instead." #-}

-- | The replication group ID
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaReplicationGroupId :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Types.String)
uuaReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED uuaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaServiceUpdateName :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Types.String)
uuaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# DEPRECATED uuaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

instance Core.FromXML UnprocessedUpdateAction where
  parseXML x =
    UnprocessedUpdateAction'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "ErrorMessage")
      Core.<*> (x Core..@? "ErrorType")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "ServiceUpdateName")
