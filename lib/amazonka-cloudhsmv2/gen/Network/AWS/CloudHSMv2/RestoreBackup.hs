{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.RestoreBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a specified AWS CloudHSM backup that is in the @PENDING_DELETION@ state. For mor information on deleting a backup, see 'DeleteBackup' .
module Network.AWS.CloudHSMv2.RestoreBackup
  ( -- * Creating a request
    RestoreBackup (..),
    mkRestoreBackup,

    -- ** Request lenses
    rbBackupId,

    -- * Destructuring the response
    RestoreBackupResponse (..),
    mkRestoreBackupResponse,

    -- ** Response lenses
    rbrsBackup,
    rbrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRestoreBackup' smart constructor.
newtype RestoreBackup = RestoreBackup'
  { -- | The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
    backupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreBackup' with the minimum fields required to make a request.
--
-- * 'backupId' - The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
mkRestoreBackup ::
  -- | 'backupId'
  Lude.Text ->
  RestoreBackup
mkRestoreBackup pBackupId_ = RestoreBackup' {backupId = pBackupId_}

-- | The ID of the backup to be restored. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbBackupId :: Lens.Lens' RestoreBackup Lude.Text
rbBackupId = Lens.lens (backupId :: RestoreBackup -> Lude.Text) (\s a -> s {backupId = a} :: RestoreBackup)
{-# DEPRECATED rbBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Lude.AWSRequest RestoreBackup where
  type Rs RestoreBackup = RestoreBackupResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          RestoreBackupResponse'
            Lude.<$> (x Lude..?> "Backup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.RestoreBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreBackup where
  toJSON RestoreBackup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BackupId" Lude..= backupId)])

instance Lude.ToPath RestoreBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRestoreBackupResponse' smart constructor.
data RestoreBackupResponse = RestoreBackupResponse'
  { -- | Information on the @Backup@ object created.
    backup :: Lude.Maybe Backup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreBackupResponse' with the minimum fields required to make a request.
--
-- * 'backup' - Information on the @Backup@ object created.
-- * 'responseStatus' - The response status code.
mkRestoreBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreBackupResponse
mkRestoreBackupResponse pResponseStatus_ =
  RestoreBackupResponse'
    { backup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information on the @Backup@ object created.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrsBackup :: Lens.Lens' RestoreBackupResponse (Lude.Maybe Backup)
rbrsBackup = Lens.lens (backup :: RestoreBackupResponse -> Lude.Maybe Backup) (\s a -> s {backup = a} :: RestoreBackupResponse)
{-# DEPRECATED rbrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rbrsResponseStatus :: Lens.Lens' RestoreBackupResponse Lude.Int
rbrsResponseStatus = Lens.lens (responseStatus :: RestoreBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreBackupResponse)
{-# DEPRECATED rbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
