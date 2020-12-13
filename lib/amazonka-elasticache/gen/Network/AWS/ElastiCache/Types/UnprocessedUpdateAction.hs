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
    uuaServiceUpdateName,
    uuaErrorType,
    uuaErrorMessage,
    uuaReplicationGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Update action that has failed to be processed for the corresponding apply/stop request
--
-- /See:/ 'mkUnprocessedUpdateAction' smart constructor.
data UnprocessedUpdateAction = UnprocessedUpdateAction'
  { -- | The ID of the cache cluster
    cacheClusterId :: Lude.Maybe Lude.Text,
    -- | The unique ID of the service update
    serviceUpdateName :: Lude.Maybe Lude.Text,
    -- | The error type for requests that are not processed
    errorType :: Lude.Maybe Lude.Text,
    -- | The error message that describes the reason the request was not processed
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The replication group ID
    replicationGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedUpdateAction' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The ID of the cache cluster
-- * 'serviceUpdateName' - The unique ID of the service update
-- * 'errorType' - The error type for requests that are not processed
-- * 'errorMessage' - The error message that describes the reason the request was not processed
-- * 'replicationGroupId' - The replication group ID
mkUnprocessedUpdateAction ::
  UnprocessedUpdateAction
mkUnprocessedUpdateAction =
  UnprocessedUpdateAction'
    { cacheClusterId = Lude.Nothing,
      serviceUpdateName = Lude.Nothing,
      errorType = Lude.Nothing,
      errorMessage = Lude.Nothing,
      replicationGroupId = Lude.Nothing
    }

-- | The ID of the cache cluster
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaCacheClusterId :: Lens.Lens' UnprocessedUpdateAction (Lude.Maybe Lude.Text)
uuaCacheClusterId = Lens.lens (cacheClusterId :: UnprocessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: UnprocessedUpdateAction)
{-# DEPRECATED uuaCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The unique ID of the service update
--
-- /Note:/ Consider using 'serviceUpdateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaServiceUpdateName :: Lens.Lens' UnprocessedUpdateAction (Lude.Maybe Lude.Text)
uuaServiceUpdateName = Lens.lens (serviceUpdateName :: UnprocessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {serviceUpdateName = a} :: UnprocessedUpdateAction)
{-# DEPRECATED uuaServiceUpdateName "Use generic-lens or generic-optics with 'serviceUpdateName' instead." #-}

-- | The error type for requests that are not processed
--
-- /Note:/ Consider using 'errorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorType :: Lens.Lens' UnprocessedUpdateAction (Lude.Maybe Lude.Text)
uuaErrorType = Lens.lens (errorType :: UnprocessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {errorType = a} :: UnprocessedUpdateAction)
{-# DEPRECATED uuaErrorType "Use generic-lens or generic-optics with 'errorType' instead." #-}

-- | The error message that describes the reason the request was not processed
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaErrorMessage :: Lens.Lens' UnprocessedUpdateAction (Lude.Maybe Lude.Text)
uuaErrorMessage = Lens.lens (errorMessage :: UnprocessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: UnprocessedUpdateAction)
{-# DEPRECATED uuaErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The replication group ID
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaReplicationGroupId :: Lens.Lens' UnprocessedUpdateAction (Lude.Maybe Lude.Text)
uuaReplicationGroupId = Lens.lens (replicationGroupId :: UnprocessedUpdateAction -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: UnprocessedUpdateAction)
{-# DEPRECATED uuaReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.FromXML UnprocessedUpdateAction where
  parseXML x =
    UnprocessedUpdateAction'
      Lude.<$> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "ServiceUpdateName")
      Lude.<*> (x Lude..@? "ErrorType")
      Lude.<*> (x Lude..@? "ErrorMessage")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
