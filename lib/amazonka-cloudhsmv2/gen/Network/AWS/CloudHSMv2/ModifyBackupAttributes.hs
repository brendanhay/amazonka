{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ModifyBackupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies attributes for AWS CloudHSM backup.
module Network.AWS.CloudHSMv2.ModifyBackupAttributes
  ( -- * Creating a request
    ModifyBackupAttributes (..),
    mkModifyBackupAttributes,

    -- ** Request lenses
    mbaBackupId,
    mbaNeverExpires,

    -- * Destructuring the response
    ModifyBackupAttributesResponse (..),
    mkModifyBackupAttributesResponse,

    -- ** Response lenses
    mbarsBackup,
    mbarsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyBackupAttributes' smart constructor.
data ModifyBackupAttributes = ModifyBackupAttributes'
  { backupId ::
      Lude.Text,
    neverExpires :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyBackupAttributes' with the minimum fields required to make a request.
--
-- * 'backupId' - The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
-- * 'neverExpires' - Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
mkModifyBackupAttributes ::
  -- | 'backupId'
  Lude.Text ->
  -- | 'neverExpires'
  Lude.Bool ->
  ModifyBackupAttributes
mkModifyBackupAttributes pBackupId_ pNeverExpires_ =
  ModifyBackupAttributes'
    { backupId = pBackupId_,
      neverExpires = pNeverExpires_
    }

-- | The identifier (ID) of the backup to modify. To find the ID of a backup, use the 'DescribeBackups' operation.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbaBackupId :: Lens.Lens' ModifyBackupAttributes Lude.Text
mbaBackupId = Lens.lens (backupId :: ModifyBackupAttributes -> Lude.Text) (\s a -> s {backupId = a} :: ModifyBackupAttributes)
{-# DEPRECATED mbaBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | Specifies whether the service should exempt a backup from the retention policy for the cluster. @True@ exempts a backup from the retention policy. @False@ means the service applies the backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'neverExpires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbaNeverExpires :: Lens.Lens' ModifyBackupAttributes Lude.Bool
mbaNeverExpires = Lens.lens (neverExpires :: ModifyBackupAttributes -> Lude.Bool) (\s a -> s {neverExpires = a} :: ModifyBackupAttributes)
{-# DEPRECATED mbaNeverExpires "Use generic-lens or generic-optics with 'neverExpires' instead." #-}

instance Lude.AWSRequest ModifyBackupAttributes where
  type Rs ModifyBackupAttributes = ModifyBackupAttributesResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyBackupAttributesResponse'
            Lude.<$> (x Lude..?> "Backup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyBackupAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.ModifyBackupAttributes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyBackupAttributes where
  toJSON ModifyBackupAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BackupId" Lude..= backupId),
            Lude.Just ("NeverExpires" Lude..= neverExpires)
          ]
      )

instance Lude.ToPath ModifyBackupAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyBackupAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyBackupAttributesResponse' smart constructor.
data ModifyBackupAttributesResponse = ModifyBackupAttributesResponse'
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

-- | Creates a value of 'ModifyBackupAttributesResponse' with the minimum fields required to make a request.
--
-- * 'backup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyBackupAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyBackupAttributesResponse
mkModifyBackupAttributesResponse pResponseStatus_ =
  ModifyBackupAttributesResponse'
    { backup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'backup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbarsBackup :: Lens.Lens' ModifyBackupAttributesResponse (Lude.Maybe Backup)
mbarsBackup = Lens.lens (backup :: ModifyBackupAttributesResponse -> Lude.Maybe Backup) (\s a -> s {backup = a} :: ModifyBackupAttributesResponse)
{-# DEPRECATED mbarsBackup "Use generic-lens or generic-optics with 'backup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mbarsResponseStatus :: Lens.Lens' ModifyBackupAttributesResponse Lude.Int
mbarsResponseStatus = Lens.lens (responseStatus :: ModifyBackupAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyBackupAttributesResponse)
{-# DEPRECATED mbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
