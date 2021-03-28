{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
  ( UnprocessedUpdateAction (..)
  -- * Smart constructor
  , mkUnprocessedUpdateAction
  -- * Lenses
  , uuaCacheClusterId
  , uuaErrorMessage
  , uuaErrorType
  , uuaReplicationGroupId
  , uuaServiceUpdateName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Update action that has failed to be processed for the corresponding apply/stop request
--
-- /See:/ 'mkUnprocessedUpdateAction' smart constructor.
data UnprocessedUpdateAction = UnprocessedUpdateAction'
  { cacheClusterId :: Core.Maybe Core.Text
    -- ^ The ID of the cache cluster
  , errorMessage :: Core.Maybe Core.Text
    -- ^ The error message that describes the reason the request was not processed
  , errorType :: Core.Maybe Core.Text
    -- ^ The error type for requests that are not processed
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The replication group ID
  , serviceUpdateName :: Core.Maybe Core.Text
    -- ^ The unique ID of the service update
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedUpdateAction' value with any optional fields omitted.
mkUnprocessedUpdateAction
    :: UnprocessedUpdateAction
mkUnprocessedUpdateAction
  = UnprocessedUpdateAction'{cacheClusterId = Core.Nothing,
                             errorMessage = Core.Nothing, errorType = Core.Nothing,
                             replicationGroupId = Core.Nothing,
                             serviceUpdateName = Core.Nothing}

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaCacheClusterId :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Core.Text)
uuaCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE uuaCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The error message that describes the reason the request was not processed
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorMessage :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Core.Text)
uuaErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE uuaErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The error type for requests that are not processed
--
-- /Note:/ Consider using 'errorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorType :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Core.Text)
uuaErrorType = Lens.field @"errorType"
{-# INLINEABLE uuaErrorType #-}
{-# DEPRECATED errorType "Use generic-lens or generic-optics with 'errorType' instead"  #-}

-- | The replication group ID
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaReplicationGroupId :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Core.Text)
uuaReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE uuaReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaServiceUpdateName :: Lens.Lens' UnprocessedUpdateAction (Core.Maybe Core.Text)
uuaServiceUpdateName = Lens.field @"serviceUpdateName"
{-# INLINEABLE uuaServiceUpdateName #-}
{-# DEPRECATED serviceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead"  #-}

instance Core.FromXML UnprocessedUpdateAction where
        parseXML x
          = UnprocessedUpdateAction' Core.<$>
              (x Core..@? "CacheClusterId") Core.<*> x Core..@? "ErrorMessage"
                Core.<*> x Core..@? "ErrorType"
                Core.<*> x Core..@? "ReplicationGroupId"
                Core.<*> x Core..@? "ServiceUpdateName"
