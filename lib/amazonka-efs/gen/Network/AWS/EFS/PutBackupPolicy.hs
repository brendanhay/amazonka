{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the file system's backup policy. Use this action to start or stop automatic backups of the file system.
module Network.AWS.EFS.PutBackupPolicy
  ( -- * Creating a request
    PutBackupPolicy (..),
    mkPutBackupPolicy,

    -- ** Request lenses
    pbpFileSystemId,
    pbpBackupPolicy,

    -- * Destructuring the response
    BackupPolicyDescription (..),
    mkBackupPolicyDescription,

    -- ** Response lenses
    bpdBackupPolicy,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutBackupPolicy' smart constructor.
data PutBackupPolicy = PutBackupPolicy'
  { -- | Specifies which EFS file system to update the backup policy for.
    fileSystemId :: Lude.Text,
    -- | The backup policy included in the @PutBackupPolicy@ request.
    backupPolicy :: BackupPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBackupPolicy' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - Specifies which EFS file system to update the backup policy for.
-- * 'backupPolicy' - The backup policy included in the @PutBackupPolicy@ request.
mkPutBackupPolicy ::
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'backupPolicy'
  BackupPolicy ->
  PutBackupPolicy
mkPutBackupPolicy pFileSystemId_ pBackupPolicy_ =
  PutBackupPolicy'
    { fileSystemId = pFileSystemId_,
      backupPolicy = pBackupPolicy_
    }

-- | Specifies which EFS file system to update the backup policy for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpFileSystemId :: Lens.Lens' PutBackupPolicy Lude.Text
pbpFileSystemId = Lens.lens (fileSystemId :: PutBackupPolicy -> Lude.Text) (\s a -> s {fileSystemId = a} :: PutBackupPolicy)
{-# DEPRECATED pbpFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The backup policy included in the @PutBackupPolicy@ request.
--
-- /Note:/ Consider using 'backupPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpBackupPolicy :: Lens.Lens' PutBackupPolicy BackupPolicy
pbpBackupPolicy = Lens.lens (backupPolicy :: PutBackupPolicy -> BackupPolicy) (\s a -> s {backupPolicy = a} :: PutBackupPolicy)
{-# DEPRECATED pbpBackupPolicy "Use generic-lens or generic-optics with 'backupPolicy' instead." #-}

instance Lude.AWSRequest PutBackupPolicy where
  type Rs PutBackupPolicy = BackupPolicyDescription
  request = Req.putJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutBackupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutBackupPolicy where
  toJSON PutBackupPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("BackupPolicy" Lude..= backupPolicy)])

instance Lude.ToPath PutBackupPolicy where
  toPath PutBackupPolicy' {..} =
    Lude.mconcat
      [ "/2015-02-01/file-systems/",
        Lude.toBS fileSystemId,
        "/backup-policy"
      ]

instance Lude.ToQuery PutBackupPolicy where
  toQuery = Lude.const Lude.mempty
