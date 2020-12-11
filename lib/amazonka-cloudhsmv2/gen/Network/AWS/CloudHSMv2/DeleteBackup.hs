{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DeleteBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified AWS CloudHSM backup. A backup can be restored up to 7 days after the DeleteBackup request is made. For more information on restoring a backup, see 'RestoreBackup' .
module Network.AWS.CloudHSMv2.DeleteBackup
  ( -- * Creating a request
    DeleteBackup (..),
    mkDeleteBackup,

    -- ** Request lenses
    dbBackupId,

    -- * Destructuring the response
    DeleteBackupResponse (..),
    mkDeleteBackupResponse,

    -- ** Response lenses
    dbrsBackup,
    dbrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup' {backupId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBackup' with the minimum fields required to make a request.
--
-- * 'backupId' - The ID of the backup to be deleted. To find the ID of a backup, use the 'DescribeBackups' operation.
mkDeleteBackup ::
  -- | 'backupId'
  Lude.Text ->
  DeleteBackup
mkDeleteBackup pBackupId_ = DeleteBackup' {backupId = pBackupId_}

-- | The ID of the backup to be deleted. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupId :: Lens.Lens' DeleteBackup Lude.Text
dbBackupId = Lens.lens (backupId :: DeleteBackup -> Lude.Text) (\s a -> s {backupId = a} :: DeleteBackup)
{-# DEPRECATED dbBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Lude.AWSRequest DeleteBackup where
  type Rs DeleteBackup = DeleteBackupResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBackupResponse'
            Lude.<$> (x Lude..?> "Backup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.DeleteBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBackup where
  toJSON DeleteBackup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BackupId" Lude..= backupId)])

instance Lude.ToPath DeleteBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { backup ::
      Lude.Maybe Backup,
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

-- | Creates a value of 'DeleteBackupResponse' with the minimum fields required to make a request.
--
-- * 'backup' - Information on the @Backup@ object deleted.
-- * 'responseStatus' - The response status code.
mkDeleteBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBackupResponse
mkDeleteBackupResponse pResponseStatus_ =
  DeleteBackupResponse'
    { backup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information on the @Backup@ object deleted.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBackup :: Lens.Lens' DeleteBackupResponse (Lude.Maybe Backup)
dbrsBackup = Lens.lens (backup :: DeleteBackupResponse -> Lude.Maybe Backup) (\s a -> s {backup = a} :: DeleteBackupResponse)
{-# DEPRECATED dbrsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DeleteBackupResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DeleteBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBackupResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
