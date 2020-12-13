{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DeleteBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a backup. You can delete both manual and automated backups. This operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when a backup deletion is already in progress. A @ResourceNotFoundException@ is thrown when the backup does not exist. A @ValidationException@ is thrown when parameters of the request are not valid.
module Network.AWS.OpsWorksCM.DeleteBackup
  ( -- * Creating a request
    DeleteBackup (..),
    mkDeleteBackup,

    -- ** Request lenses
    dbBackupId,

    -- * Destructuring the response
    DeleteBackupResponse (..),
    mkDeleteBackupResponse,

    -- ** Response lenses
    dbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup'
  { -- | The ID of the backup to delete. Run the DescribeBackups command to get a list of backup IDs. Backup IDs are in the format @ServerName-yyyyMMddHHmmssSSS@ .
    backupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBackup' with the minimum fields required to make a request.
--
-- * 'backupId' - The ID of the backup to delete. Run the DescribeBackups command to get a list of backup IDs. Backup IDs are in the format @ServerName-yyyyMMddHHmmssSSS@ .
mkDeleteBackup ::
  -- | 'backupId'
  Lude.Text ->
  DeleteBackup
mkDeleteBackup pBackupId_ = DeleteBackup' {backupId = pBackupId_}

-- | The ID of the backup to delete. Run the DescribeBackups command to get a list of backup IDs. Backup IDs are in the format @ServerName-yyyyMMddHHmmssSSS@ .
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupId :: Lens.Lens' DeleteBackup Lude.Text
dbBackupId = Lens.lens (backupId :: DeleteBackup -> Lude.Text) (\s a -> s {backupId = a} :: DeleteBackup)
{-# DEPRECATED dbBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

instance Lude.AWSRequest DeleteBackup where
  type Rs DeleteBackup = DeleteBackupResponse
  request = Req.postJSON opsWorksCMService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteBackupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorksCM_V2016_11_01.DeleteBackup" :: Lude.ByteString),
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
newtype DeleteBackupResponse = DeleteBackupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBackupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBackupResponse
mkDeleteBackupResponse pResponseStatus_ =
  DeleteBackupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DeleteBackupResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DeleteBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBackupResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
