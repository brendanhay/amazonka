{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.UpdateServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates settings for a server.
--
-- This operation is synchronous.
module Network.AWS.OpsWorksCM.UpdateServer
  ( -- * Creating a request
    UpdateServer (..),
    mkUpdateServer,

    -- ** Request lenses
    usDisableAutomatedBackup,
    usPreferredMaintenanceWindow,
    usPreferredBackupWindow,
    usBackupRetentionCount,
    usServerName,

    -- * Destructuring the response
    UpdateServerResponse (..),
    mkUpdateServerResponse,

    -- ** Response lenses
    usrsServer,
    usrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateServer' smart constructor.
data UpdateServer = UpdateServer'
  { disableAutomatedBackup ::
      Lude.Maybe Lude.Bool,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    backupRetentionCount :: Lude.Maybe Lude.Int,
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

-- | Creates a value of 'UpdateServer' with the minimum fields required to make a request.
--
-- * 'backupRetentionCount' - Sets the number of automated backups that you want to keep.
-- * 'disableAutomatedBackup' - Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default.
-- * 'preferredBackupWindow' - Undocumented field.
-- * 'preferredMaintenanceWindow' - Undocumented field.
-- * 'serverName' - The name of the server to update.
mkUpdateServer ::
  -- | 'serverName'
  Lude.Text ->
  UpdateServer
mkUpdateServer pServerName_ =
  UpdateServer'
    { disableAutomatedBackup = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      backupRetentionCount = Lude.Nothing,
      serverName = pServerName_
    }

-- | Setting DisableAutomatedBackup to @true@ disables automated or scheduled backups. Automated backups are enabled by default.
--
-- /Note:/ Consider using 'disableAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDisableAutomatedBackup :: Lens.Lens' UpdateServer (Lude.Maybe Lude.Bool)
usDisableAutomatedBackup = Lens.lens (disableAutomatedBackup :: UpdateServer -> Lude.Maybe Lude.Bool) (\s a -> s {disableAutomatedBackup = a} :: UpdateServer)
{-# DEPRECATED usDisableAutomatedBackup "Use generic-lens or generic-optics with 'disableAutomatedBackup' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredMaintenanceWindow :: Lens.Lens' UpdateServer (Lude.Maybe Lude.Text)
usPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: UpdateServer -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: UpdateServer)
{-# DEPRECATED usPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usPreferredBackupWindow :: Lens.Lens' UpdateServer (Lude.Maybe Lude.Text)
usPreferredBackupWindow = Lens.lens (preferredBackupWindow :: UpdateServer -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: UpdateServer)
{-# DEPRECATED usPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | Sets the number of automated backups that you want to keep.
--
-- /Note:/ Consider using 'backupRetentionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usBackupRetentionCount :: Lens.Lens' UpdateServer (Lude.Maybe Lude.Int)
usBackupRetentionCount = Lens.lens (backupRetentionCount :: UpdateServer -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionCount = a} :: UpdateServer)
{-# DEPRECATED usBackupRetentionCount "Use generic-lens or generic-optics with 'backupRetentionCount' instead." #-}

-- | The name of the server to update.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usServerName :: Lens.Lens' UpdateServer Lude.Text
usServerName = Lens.lens (serverName :: UpdateServer -> Lude.Text) (\s a -> s {serverName = a} :: UpdateServer)
{-# DEPRECATED usServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Lude.AWSRequest UpdateServer where
  type Rs UpdateServer = UpdateServerResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateServerResponse'
            Lude.<$> (x Lude..?> "Server") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateServer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.UpdateServer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateServer where
  toJSON UpdateServer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisableAutomatedBackup" Lude..=)
              Lude.<$> disableAutomatedBackup,
            ("PreferredMaintenanceWindow" Lude..=)
              Lude.<$> preferredMaintenanceWindow,
            ("PreferredBackupWindow" Lude..=) Lude.<$> preferredBackupWindow,
            ("BackupRetentionCount" Lude..=) Lude.<$> backupRetentionCount,
            Lude.Just ("ServerName" Lude..= serverName)
          ]
      )

instance Lude.ToPath UpdateServer where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateServer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateServerResponse' smart constructor.
data UpdateServerResponse = UpdateServerResponse'
  { server ::
      Lude.Maybe Server,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateServerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'server' - Contains the response to a @UpdateServer@ request.
mkUpdateServerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateServerResponse
mkUpdateServerResponse pResponseStatus_ =
  UpdateServerResponse'
    { server = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the response to a @UpdateServer@ request.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsServer :: Lens.Lens' UpdateServerResponse (Lude.Maybe Server)
usrsServer = Lens.lens (server :: UpdateServerResponse -> Lude.Maybe Server) (\s a -> s {server = a} :: UpdateServerResponse)
{-# DEPRECATED usrsServer "Use generic-lens or generic-optics with 'server' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrsResponseStatus :: Lens.Lens' UpdateServerResponse Lude.Int
usrsResponseStatus = Lens.lens (responseStatus :: UpdateServerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateServerResponse)
{-# DEPRECATED usrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
