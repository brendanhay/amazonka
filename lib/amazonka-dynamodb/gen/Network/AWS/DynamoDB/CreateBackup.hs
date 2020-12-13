{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.CreateBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup for an existing table.
--
-- Each time you create an on-demand backup, the entire table data is backed up. There is no limit to the number of on-demand backups that can be taken.
-- When you create an on-demand backup, a time marker of the request is cataloged, and the backup is created asynchronously, by applying all changes until the time of the request to the last full table snapshot. Backup requests are processed instantaneously and become available for restore within minutes.
-- You can call @CreateBackup@ at a maximum rate of 50 times per second.
-- All backups in DynamoDB work without consuming any provisioned throughput on the table.
-- If you submit a backup request on 2018-12-14 at 14:25:00, the backup is guaranteed to contain all data committed to the table up to 14:24:00, and data committed after 14:26:00 will not be. The backup might contain data modifications made between 14:24:00 and 14:26:00. On-demand backup does not support causal consistency.
-- Along with data, the following are also included on the backups:
--
--     * Global secondary indexes (GSIs)
--
--
--     * Local secondary indexes (LSIs)
--
--
--     * Streams
--
--
--     * Provisioned read and write capacity
module Network.AWS.DynamoDB.CreateBackup
  ( -- * Creating a request
    CreateBackup (..),
    mkCreateBackup,

    -- ** Request lenses
    cbBackupName,
    cbTableName,

    -- * Destructuring the response
    CreateBackupResponse (..),
    mkCreateBackupResponse,

    -- ** Response lenses
    cbrsBackupDetails,
    cbrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateBackup' smart constructor.
data CreateBackup = CreateBackup'
  { -- | Specified name for the backup.
    backupName :: Lude.Text,
    -- | The name of the table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBackup' with the minimum fields required to make a request.
--
-- * 'backupName' - Specified name for the backup.
-- * 'tableName' - The name of the table.
mkCreateBackup ::
  -- | 'backupName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  CreateBackup
mkCreateBackup pBackupName_ pTableName_ =
  CreateBackup' {backupName = pBackupName_, tableName = pTableName_}

-- | Specified name for the backup.
--
-- /Note:/ Consider using 'backupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBackupName :: Lens.Lens' CreateBackup Lude.Text
cbBackupName = Lens.lens (backupName :: CreateBackup -> Lude.Text) (\s a -> s {backupName = a} :: CreateBackup)
{-# DEPRECATED cbBackupName "Use generic-lens or generic-optics with 'backupName' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbTableName :: Lens.Lens' CreateBackup Lude.Text
cbTableName = Lens.lens (tableName :: CreateBackup -> Lude.Text) (\s a -> s {tableName = a} :: CreateBackup)
{-# DEPRECATED cbTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest CreateBackup where
  type Rs CreateBackup = CreateBackupResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateBackupResponse'
            Lude.<$> (x Lude..?> "BackupDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.CreateBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateBackup where
  toJSON CreateBackup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BackupName" Lude..= backupName),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath CreateBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { -- | Contains the details of the backup created for the table.
    backupDetails :: Lude.Maybe BackupDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateBackupResponse' with the minimum fields required to make a request.
--
-- * 'backupDetails' - Contains the details of the backup created for the table.
-- * 'responseStatus' - The response status code.
mkCreateBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateBackupResponse
mkCreateBackupResponse pResponseStatus_ =
  CreateBackupResponse'
    { backupDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the details of the backup created for the table.
--
-- /Note:/ Consider using 'backupDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsBackupDetails :: Lens.Lens' CreateBackupResponse (Lude.Maybe BackupDetails)
cbrsBackupDetails = Lens.lens (backupDetails :: CreateBackupResponse -> Lude.Maybe BackupDetails) (\s a -> s {backupDetails = a} :: CreateBackupResponse)
{-# DEPRECATED cbrsBackupDetails "Use generic-lens or generic-optics with 'backupDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrsResponseStatus :: Lens.Lens' CreateBackupResponse Lude.Int
cbrsResponseStatus = Lens.lens (responseStatus :: CreateBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateBackupResponse)
{-# DEPRECATED cbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
