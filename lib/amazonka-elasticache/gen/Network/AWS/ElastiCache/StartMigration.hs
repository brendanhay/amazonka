{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.StartMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start the migration of data.
module Network.AWS.ElastiCache.StartMigration
  ( -- * Creating a request
    StartMigration (..),
    mkStartMigration,

    -- ** Request lenses
    smReplicationGroupId,
    smCustomerNodeEndpointList,

    -- * Destructuring the response
    StartMigrationResponse (..),
    mkStartMigrationResponse,

    -- ** Response lenses
    smrsReplicationGroup,
    smrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartMigration' smart constructor.
data StartMigration = StartMigration'
  { replicationGroupId ::
      Lude.Text,
    customerNodeEndpointList :: [CustomerNodeEndpoint]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMigration' with the minimum fields required to make a request.
--
-- * 'customerNodeEndpointList' - List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
-- * 'replicationGroupId' - The ID of the replication group to which data should be migrated.
mkStartMigration ::
  -- | 'replicationGroupId'
  Lude.Text ->
  StartMigration
mkStartMigration pReplicationGroupId_ =
  StartMigration'
    { replicationGroupId = pReplicationGroupId_,
      customerNodeEndpointList = Lude.mempty
    }

-- | The ID of the replication group to which data should be migrated.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smReplicationGroupId :: Lens.Lens' StartMigration Lude.Text
smReplicationGroupId = Lens.lens (replicationGroupId :: StartMigration -> Lude.Text) (\s a -> s {replicationGroupId = a} :: StartMigration)
{-# DEPRECATED smReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | List of endpoints from which data should be migrated. For Redis (cluster mode disabled), list should have only one element.
--
-- /Note:/ Consider using 'customerNodeEndpointList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smCustomerNodeEndpointList :: Lens.Lens' StartMigration [CustomerNodeEndpoint]
smCustomerNodeEndpointList = Lens.lens (customerNodeEndpointList :: StartMigration -> [CustomerNodeEndpoint]) (\s a -> s {customerNodeEndpointList = a} :: StartMigration)
{-# DEPRECATED smCustomerNodeEndpointList "Use generic-lens or generic-optics with 'customerNodeEndpointList' instead." #-}

instance Lude.AWSRequest StartMigration where
  type Rs StartMigration = StartMigrationResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "StartMigrationResult"
      ( \s h x ->
          StartMigrationResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartMigration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath StartMigration where
  toPath = Lude.const "/"

instance Lude.ToQuery StartMigration where
  toQuery StartMigration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("StartMigration" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "CustomerNodeEndpointList"
          Lude.=: Lude.toQueryList "member" customerNodeEndpointList
      ]

-- | /See:/ 'mkStartMigrationResponse' smart constructor.
data StartMigrationResponse = StartMigrationResponse'
  { replicationGroup ::
      Lude.Maybe ReplicationGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartMigrationResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkStartMigrationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartMigrationResponse
mkStartMigrationResponse pResponseStatus_ =
  StartMigrationResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsReplicationGroup :: Lens.Lens' StartMigrationResponse (Lude.Maybe ReplicationGroup)
smrsReplicationGroup = Lens.lens (replicationGroup :: StartMigrationResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: StartMigrationResponse)
{-# DEPRECATED smrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smrsResponseStatus :: Lens.Lens' StartMigrationResponse Lude.Int
smrsResponseStatus = Lens.lens (responseStatus :: StartMigrationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartMigrationResponse)
{-# DEPRECATED smrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
