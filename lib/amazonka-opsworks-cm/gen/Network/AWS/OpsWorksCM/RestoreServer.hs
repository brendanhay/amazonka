{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    rsKeyPair,
    rsInstanceType,
    rsBackupId,
    rsServerName,

    -- * Destructuring the response
    RestoreServerResponse (..),
    mkRestoreServerResponse,

    -- ** Response lenses
    rsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreServer' smart constructor.
data RestoreServer = RestoreServer'
  { keyPair ::
      Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    backupId :: Lude.Text,
    serverName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreServer' with the minimum fields required to make a request.
--
-- * 'backupId' - The ID of the backup that you want to use to restore a server.
-- * 'instanceType' - The type of instance to restore. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m5.large@ . Valid values are @m5.large@ , @r5.xlarge@ , and @r5.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
-- * 'keyPair' - The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
-- * 'serverName' - The name of the server that you want to restore.
mkRestoreServer ::
  -- | 'backupId'
  Lude.Text ->
  -- | 'serverName'
  Lude.Text ->
  RestoreServer
mkRestoreServer pBackupId_ pServerName_ =
  RestoreServer'
    { keyPair = Lude.Nothing,
      instanceType = Lude.Nothing,
      backupId = pBackupId_,
      serverName = pServerName_
    }

-- | The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsKeyPair :: Lens.Lens' RestoreServer (Lude.Maybe Lude.Text)
rsKeyPair = Lens.lens (keyPair :: RestoreServer -> Lude.Maybe Lude.Text) (\s a -> s {keyPair = a} :: RestoreServer)
{-# DEPRECATED rsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The type of instance to restore. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m5.large@ . Valid values are @m5.large@ , @r5.xlarge@ , and @r5.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsInstanceType :: Lens.Lens' RestoreServer (Lude.Maybe Lude.Text)
rsInstanceType = Lens.lens (instanceType :: RestoreServer -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: RestoreServer)
{-# DEPRECATED rsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the backup that you want to use to restore a server.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsBackupId :: Lens.Lens' RestoreServer Lude.Text
rsBackupId = Lens.lens (backupId :: RestoreServer -> Lude.Text) (\s a -> s {backupId = a} :: RestoreServer)
{-# DEPRECATED rsBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | The name of the server that you want to restore.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsServerName :: Lens.Lens' RestoreServer Lude.Text
rsServerName = Lens.lens (serverName :: RestoreServer -> Lude.Text) (\s a -> s {serverName = a} :: RestoreServer)
{-# DEPRECATED rsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Lude.AWSRequest RestoreServer where
  type Rs RestoreServer = RestoreServerResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RestoreServerResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.RestoreServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreServer where
  toJSON RestoreServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KeyPair" Lude..=) Lude.<$> keyPair,
            ("InstanceType" Lude..=) Lude.<$> instanceType,
            Lude.Just ("BackupId" Lude..= backupId),
            Lude.Just ("ServerName" Lude..= serverName)
          ]
      )

instance Lude.ToPath RestoreServer where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreServerResponse' smart constructor.
newtype RestoreServerResponse = RestoreServerResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreServerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRestoreServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreServerResponse
mkRestoreServerResponse pResponseStatus_ =
  RestoreServerResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsResponseStatus :: Lens.Lens' RestoreServerResponse Lude.Int
rsrsResponseStatus = Lens.lens (responseStatus :: RestoreServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreServerResponse)
{-# DEPRECATED rsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
