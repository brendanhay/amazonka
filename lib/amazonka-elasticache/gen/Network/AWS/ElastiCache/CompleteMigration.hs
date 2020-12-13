{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CompleteMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Complete the migration of data.
module Network.AWS.ElastiCache.CompleteMigration
  ( -- * Creating a request
    CompleteMigration (..),
    mkCompleteMigration,

    -- ** Request lenses
    cmForce,
    cmReplicationGroupId,

    -- * Destructuring the response
    CompleteMigrationResponse (..),
    mkCompleteMigrationResponse,

    -- ** Response lenses
    cmrsReplicationGroup,
    cmrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCompleteMigration' smart constructor.
data CompleteMigration = CompleteMigration'
  { -- | Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
    force :: Lude.Maybe Lude.Bool,
    -- | The ID of the replication group to which data is being migrated.
    replicationGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteMigration' with the minimum fields required to make a request.
--
-- * 'force' - Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
-- * 'replicationGroupId' - The ID of the replication group to which data is being migrated.
mkCompleteMigration ::
  -- | 'replicationGroupId'
  Lude.Text ->
  CompleteMigration
mkCompleteMigration pReplicationGroupId_ =
  CompleteMigration'
    { force = Lude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

-- | Forces the migration to stop without ensuring that data is in sync. It is recommended to use this option only to abort the migration and not recommended when application wants to continue migration to ElastiCache.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmForce :: Lens.Lens' CompleteMigration (Lude.Maybe Lude.Bool)
cmForce = Lens.lens (force :: CompleteMigration -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: CompleteMigration)
{-# DEPRECATED cmForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The ID of the replication group to which data is being migrated.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmReplicationGroupId :: Lens.Lens' CompleteMigration Lude.Text
cmReplicationGroupId = Lens.lens (replicationGroupId :: CompleteMigration -> Lude.Text) (\s a -> s {replicationGroupId = a} :: CompleteMigration)
{-# DEPRECATED cmReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest CompleteMigration where
  type Rs CompleteMigration = CompleteMigrationResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CompleteMigrationResult"
      ( \s h x ->
          CompleteMigrationResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CompleteMigration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CompleteMigration where
  toPath = Lude.const "/"

instance Lude.ToQuery CompleteMigration where
  toQuery CompleteMigration' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CompleteMigration" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "Force" Lude.=: force,
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkCompleteMigrationResponse' smart constructor.
data CompleteMigrationResponse = CompleteMigrationResponse'
  { replicationGroup :: Lude.Maybe ReplicationGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompleteMigrationResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' -
-- * 'responseStatus' - The response status code.
mkCompleteMigrationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CompleteMigrationResponse
mkCompleteMigrationResponse pResponseStatus_ =
  CompleteMigrationResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsReplicationGroup :: Lens.Lens' CompleteMigrationResponse (Lude.Maybe ReplicationGroup)
cmrsReplicationGroup = Lens.lens (replicationGroup :: CompleteMigrationResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: CompleteMigrationResponse)
{-# DEPRECATED cmrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrsResponseStatus :: Lens.Lens' CompleteMigrationResponse Lude.Int
cmrsResponseStatus = Lens.lens (responseStatus :: CompleteMigrationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CompleteMigrationResponse)
{-# DEPRECATED cmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
