{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing backup of a table.
--
-- You can call @DeleteBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DeleteBackup
  ( -- * Creating a request
    DeleteBackup (..),
    mkDeleteBackup,

    -- ** Request lenses
    dbBackupARN,

    -- * Destructuring the response
    DeleteBackupResponse (..),
    mkDeleteBackupResponse,

    -- ** Response lenses
    dbrsBackupDescription,
    dbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup' {backupARN :: Lude.Text}
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
-- * 'backupARN' - The ARN associated with the backup.
mkDeleteBackup ::
  -- | 'backupARN'
  Lude.Text ->
  DeleteBackup
mkDeleteBackup pBackupARN_ = DeleteBackup' {backupARN = pBackupARN_}

-- | The ARN associated with the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupARN :: Lens.Lens' DeleteBackup Lude.Text
dbBackupARN = Lens.lens (backupARN :: DeleteBackup -> Lude.Text) (\s a -> s {backupARN = a} :: DeleteBackup)
{-# DEPRECATED dbBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

instance Lude.AWSRequest DeleteBackup where
  type Rs DeleteBackup = DeleteBackupResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteBackupResponse'
            Lude.<$> (x Lude..?> "BackupDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DeleteBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteBackup where
  toJSON DeleteBackup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BackupArn" Lude..= backupARN)])

instance Lude.ToPath DeleteBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { backupDescription ::
      Lude.Maybe BackupDescription,
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
-- * 'backupDescription' - Contains the description of the backup created for the table.
-- * 'responseStatus' - The response status code.
mkDeleteBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteBackupResponse
mkDeleteBackupResponse pResponseStatus_ =
  DeleteBackupResponse'
    { backupDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the description of the backup created for the table.
--
-- /Note:/ Consider using 'backupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBackupDescription :: Lens.Lens' DeleteBackupResponse (Lude.Maybe BackupDescription)
dbrsBackupDescription = Lens.lens (backupDescription :: DeleteBackupResponse -> Lude.Maybe BackupDescription) (\s a -> s {backupDescription = a} :: DeleteBackupResponse)
{-# DEPRECATED dbrsBackupDescription "Use generic-lens or generic-optics with 'backupDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DeleteBackupResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DeleteBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteBackupResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
