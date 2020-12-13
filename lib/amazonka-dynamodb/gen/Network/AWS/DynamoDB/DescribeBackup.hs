{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing backup of a table.
--
-- You can call @DescribeBackup@ at a maximum rate of 10 times per second.
module Network.AWS.DynamoDB.DescribeBackup
  ( -- * Creating a request
    DescribeBackup (..),
    mkDescribeBackup,

    -- ** Request lenses
    dbBackupARN,

    -- * Destructuring the response
    DescribeBackupResponse (..),
    mkDescribeBackupResponse,

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

-- | /See:/ 'mkDescribeBackup' smart constructor.
newtype DescribeBackup = DescribeBackup'
  { -- | The Amazon Resource Name (ARN) associated with the backup.
    backupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBackup' with the minimum fields required to make a request.
--
-- * 'backupARN' - The Amazon Resource Name (ARN) associated with the backup.
mkDescribeBackup ::
  -- | 'backupARN'
  Lude.Text ->
  DescribeBackup
mkDescribeBackup pBackupARN_ =
  DescribeBackup' {backupARN = pBackupARN_}

-- | The Amazon Resource Name (ARN) associated with the backup.
--
-- /Note:/ Consider using 'backupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbBackupARN :: Lens.Lens' DescribeBackup Lude.Text
dbBackupARN = Lens.lens (backupARN :: DescribeBackup -> Lude.Text) (\s a -> s {backupARN = a} :: DescribeBackup)
{-# DEPRECATED dbBackupARN "Use generic-lens or generic-optics with 'backupARN' instead." #-}

instance Lude.AWSRequest DescribeBackup where
  type Rs DescribeBackup = DescribeBackupResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBackupResponse'
            Lude.<$> (x Lude..?> "BackupDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBackup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.DescribeBackup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBackup where
  toJSON DescribeBackup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BackupArn" Lude..= backupARN)])

instance Lude.ToPath DescribeBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBackup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBackupResponse' smart constructor.
data DescribeBackupResponse = DescribeBackupResponse'
  { -- | Contains the description of the backup created for the table.
    backupDescription :: Lude.Maybe BackupDescription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBackupResponse' with the minimum fields required to make a request.
--
-- * 'backupDescription' - Contains the description of the backup created for the table.
-- * 'responseStatus' - The response status code.
mkDescribeBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBackupResponse
mkDescribeBackupResponse pResponseStatus_ =
  DescribeBackupResponse'
    { backupDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Contains the description of the backup created for the table.
--
-- /Note:/ Consider using 'backupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsBackupDescription :: Lens.Lens' DescribeBackupResponse (Lude.Maybe BackupDescription)
dbrsBackupDescription = Lens.lens (backupDescription :: DescribeBackupResponse -> Lude.Maybe BackupDescription) (\s a -> s {backupDescription = a} :: DescribeBackupResponse)
{-# DEPRECATED dbrsBackupDescription "Use generic-lens or generic-optics with 'backupDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrsResponseStatus :: Lens.Lens' DescribeBackupResponse Lude.Int
dbrsResponseStatus = Lens.lens (responseStatus :: DescribeBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBackupResponse)
{-# DEPRECATED dbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
