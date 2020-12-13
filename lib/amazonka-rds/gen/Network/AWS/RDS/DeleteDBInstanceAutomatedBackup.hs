{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes automated backups based on the source instance's @DbiResourceId@ value or the restorable instance's resource ID.
module Network.AWS.RDS.DeleteDBInstanceAutomatedBackup
  ( -- * Creating a request
    DeleteDBInstanceAutomatedBackup (..),
    mkDeleteDBInstanceAutomatedBackup,

    -- ** Request lenses
    ddbiabDBiResourceId,

    -- * Destructuring the response
    DeleteDBInstanceAutomatedBackupResponse (..),
    mkDeleteDBInstanceAutomatedBackupResponse,

    -- ** Response lenses
    ddbiabrsDBInstanceAutomatedBackup,
    ddbiabrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Parameter input for the @DeleteDBInstanceAutomatedBackup@ operation.
--
-- /See:/ 'mkDeleteDBInstanceAutomatedBackup' smart constructor.
newtype DeleteDBInstanceAutomatedBackup = DeleteDBInstanceAutomatedBackup'
  { -- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
    dbiResourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBInstanceAutomatedBackup' with the minimum fields required to make a request.
--
-- * 'dbiResourceId' - The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
mkDeleteDBInstanceAutomatedBackup ::
  -- | 'dbiResourceId'
  Lude.Text ->
  DeleteDBInstanceAutomatedBackup
mkDeleteDBInstanceAutomatedBackup pDBiResourceId_ =
  DeleteDBInstanceAutomatedBackup' {dbiResourceId = pDBiResourceId_}

-- | The identifier for the source DB instance, which can't be changed and which is unique to an AWS Region.
--
-- /Note:/ Consider using 'dbiResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabDBiResourceId :: Lens.Lens' DeleteDBInstanceAutomatedBackup Lude.Text
ddbiabDBiResourceId = Lens.lens (dbiResourceId :: DeleteDBInstanceAutomatedBackup -> Lude.Text) (\s a -> s {dbiResourceId = a} :: DeleteDBInstanceAutomatedBackup)
{-# DEPRECATED ddbiabDBiResourceId "Use generic-lens or generic-optics with 'dbiResourceId' instead." #-}

instance Lude.AWSRequest DeleteDBInstanceAutomatedBackup where
  type
    Rs DeleteDBInstanceAutomatedBackup =
      DeleteDBInstanceAutomatedBackupResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBInstanceAutomatedBackupResult"
      ( \s h x ->
          DeleteDBInstanceAutomatedBackupResponse'
            Lude.<$> (x Lude..@? "DBInstanceAutomatedBackup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDBInstanceAutomatedBackup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBInstanceAutomatedBackup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBInstanceAutomatedBackup where
  toQuery DeleteDBInstanceAutomatedBackup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteDBInstanceAutomatedBackup" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DbiResourceId" Lude.=: dbiResourceId
      ]

-- | /See:/ 'mkDeleteDBInstanceAutomatedBackupResponse' smart constructor.
data DeleteDBInstanceAutomatedBackupResponse = DeleteDBInstanceAutomatedBackupResponse'
  { dbInstanceAutomatedBackup :: Lude.Maybe DBInstanceAutomatedBackup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBInstanceAutomatedBackupResponse' with the minimum fields required to make a request.
--
-- * 'dbInstanceAutomatedBackup' -
-- * 'responseStatus' - The response status code.
mkDeleteDBInstanceAutomatedBackupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDBInstanceAutomatedBackupResponse
mkDeleteDBInstanceAutomatedBackupResponse pResponseStatus_ =
  DeleteDBInstanceAutomatedBackupResponse'
    { dbInstanceAutomatedBackup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstanceAutomatedBackup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrsDBInstanceAutomatedBackup :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse (Lude.Maybe DBInstanceAutomatedBackup)
ddbiabrsDBInstanceAutomatedBackup = Lens.lens (dbInstanceAutomatedBackup :: DeleteDBInstanceAutomatedBackupResponse -> Lude.Maybe DBInstanceAutomatedBackup) (\s a -> s {dbInstanceAutomatedBackup = a} :: DeleteDBInstanceAutomatedBackupResponse)
{-# DEPRECATED ddbiabrsDBInstanceAutomatedBackup "Use generic-lens or generic-optics with 'dbInstanceAutomatedBackup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbiabrsResponseStatus :: Lens.Lens' DeleteDBInstanceAutomatedBackupResponse Lude.Int
ddbiabrsResponseStatus = Lens.lens (responseStatus :: DeleteDBInstanceAutomatedBackupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDBInstanceAutomatedBackupResponse)
{-# DEPRECATED ddbiabrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
