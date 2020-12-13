{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeBackupPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup policy for the specified EFS file system.
module Network.AWS.EFS.DescribeBackupPolicy
  ( -- * Creating a request
    DescribeBackupPolicy (..),
    mkDescribeBackupPolicy,

    -- ** Request lenses
    dbpFileSystemId,

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

-- | /See:/ 'mkDescribeBackupPolicy' smart constructor.
newtype DescribeBackupPolicy = DescribeBackupPolicy'
  { -- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
    fileSystemId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBackupPolicy' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - Specifies which EFS file system to retrieve the @BackupPolicy@ for.
mkDescribeBackupPolicy ::
  -- | 'fileSystemId'
  Lude.Text ->
  DescribeBackupPolicy
mkDescribeBackupPolicy pFileSystemId_ =
  DescribeBackupPolicy' {fileSystemId = pFileSystemId_}

-- | Specifies which EFS file system to retrieve the @BackupPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbpFileSystemId :: Lens.Lens' DescribeBackupPolicy Lude.Text
dbpFileSystemId = Lens.lens (fileSystemId :: DescribeBackupPolicy -> Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeBackupPolicy)
{-# DEPRECATED dbpFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest DescribeBackupPolicy where
  type Rs DescribeBackupPolicy = BackupPolicyDescription
  request = Req.get efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeBackupPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeBackupPolicy where
  toPath DescribeBackupPolicy' {..} =
    Lude.mconcat
      [ "/2015-02-01/file-systems/",
        Lude.toBS fileSystemId,
        "/backup-policy"
      ]

instance Lude.ToQuery DescribeBackupPolicy where
  toQuery = Lude.const Lude.mempty
