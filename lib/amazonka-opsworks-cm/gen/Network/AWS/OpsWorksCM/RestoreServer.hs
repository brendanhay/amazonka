{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.RestoreServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a backup to a server that is in a @CONNECTION_LOST@ , @HEALTHY@ , @RUNNING@ , @UNHEALTHY@ , or @TERMINATED@ state. When you run RestoreServer, the server's EC2 instance is deleted, and a new EC2 instance is configured. RestoreServer maintains the existing server endpoint, so configuration management of the server's client devices (nodes) should continue to work.
--
-- Restoring from a backup is performed by creating a new EC2 instance. If restoration is successful, and the server is in a @HEALTHY@ state, AWS OpsWorks CM switches traffic over to the new instance. After restoration is finished, the old EC2 instance is maintained in a @Running@ or @Stopped@ state, but is eventually terminated.
-- This operation is asynchronous.
-- An @InvalidStateException@ is thrown when the server is not in a valid state. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.RestoreServer
  ( -- * Creating a request
    RestoreServer (..),
    mkRestoreServer,

    -- ** Request lenses
    rsBackupId,
    rsServerName,
    rsInstanceType,
    rsKeyPair,

    -- * Destructuring the response
    RestoreServerResponse (..),
    mkRestoreServerResponse,

    -- ** Response lenses
    rsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRestoreServer' smart constructor.
data RestoreServer = RestoreServer'
  { -- | The ID of the backup that you want to use to restore a server.
    backupId :: Types.BackupId,
    -- | The name of the server that you want to restore.
    serverName :: Types.ServerName,
    -- | The type of instance to restore. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m5.large@ . Valid values are @m5.large@ , @r5.xlarge@ , and @r5.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
    instanceType :: Core.Maybe Types.String,
    -- | The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
    keyPair :: Core.Maybe Types.KeyPair
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreServer' value with any optional fields omitted.
mkRestoreServer ::
  -- | 'backupId'
  Types.BackupId ->
  -- | 'serverName'
  Types.ServerName ->
  RestoreServer
mkRestoreServer backupId serverName =
  RestoreServer'
    { backupId,
      serverName,
      instanceType = Core.Nothing,
      keyPair = Core.Nothing
    }

-- | The ID of the backup that you want to use to restore a server.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsBackupId :: Lens.Lens' RestoreServer Types.BackupId
rsBackupId = Lens.field @"backupId"
{-# DEPRECATED rsBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | The name of the server that you want to restore.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsServerName :: Lens.Lens' RestoreServer Types.ServerName
rsServerName = Lens.field @"serverName"
{-# DEPRECATED rsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | The type of instance to restore. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m5.large@ . Valid values are @m5.large@ , @r5.xlarge@ , and @r5.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsInstanceType :: Lens.Lens' RestoreServer (Core.Maybe Types.String)
rsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED rsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsKeyPair :: Lens.Lens' RestoreServer (Core.Maybe Types.KeyPair)
rsKeyPair = Lens.field @"keyPair"
{-# DEPRECATED rsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

instance Core.FromJSON RestoreServer where
  toJSON RestoreServer {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BackupId" Core..= backupId),
            Core.Just ("ServerName" Core..= serverName),
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("KeyPair" Core..=) Core.<$> keyPair
          ]
      )

instance Core.AWSRequest RestoreServer where
  type Rs RestoreServer = RestoreServerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorksCM_V2016_11_01.RestoreServer")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreServerResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreServerResponse' smart constructor.
newtype RestoreServerResponse = RestoreServerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreServerResponse' value with any optional fields omitted.
mkRestoreServerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreServerResponse
mkRestoreServerResponse responseStatus =
  RestoreServerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsResponseStatus :: Lens.Lens' RestoreServerResponse Core.Int
rsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
