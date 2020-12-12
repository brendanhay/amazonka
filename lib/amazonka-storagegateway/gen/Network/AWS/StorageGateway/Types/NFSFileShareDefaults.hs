{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.NFSFileShareDefaults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.NFSFileShareDefaults
  ( NFSFileShareDefaults (..),

    -- * Smart constructor
    mkNFSFileShareDefaults,

    -- * Lenses
    nfsfsdFileMode,
    nfsfsdOwnerId,
    nfsfsdDirectoryMode,
    nfsfsdGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes Network File System (NFS) file share default values. Files and folders stored as Amazon S3 objects in S3 buckets don't, by default, have Unix file permissions assigned to them. Upon discovery in an S3 bucket by Storage Gateway, the S3 objects that represent files and folders are assigned these default Unix permissions. This operation is only supported for file gateways.
--
-- /See:/ 'mkNFSFileShareDefaults' smart constructor.
data NFSFileShareDefaults = NFSFileShareDefaults'
  { fileMode ::
      Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Natural,
    directoryMode :: Lude.Maybe Lude.Text,
    groupId :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NFSFileShareDefaults' with the minimum fields required to make a request.
--
-- * 'directoryMode' - The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
-- * 'fileMode' - The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
-- * 'groupId' - The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
-- * 'ownerId' - The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
mkNFSFileShareDefaults ::
  NFSFileShareDefaults
mkNFSFileShareDefaults =
  NFSFileShareDefaults'
    { fileMode = Lude.Nothing,
      ownerId = Lude.Nothing,
      directoryMode = Lude.Nothing,
      groupId = Lude.Nothing
    }

-- | The Unix file mode in the form "nnnn". For example, @0666@ represents the default file mode inside the file share. The default value is @0666@ .
--
-- /Note:/ Consider using 'fileMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdFileMode :: Lens.Lens' NFSFileShareDefaults (Lude.Maybe Lude.Text)
nfsfsdFileMode = Lens.lens (fileMode :: NFSFileShareDefaults -> Lude.Maybe Lude.Text) (\s a -> s {fileMode = a} :: NFSFileShareDefaults)
{-# DEPRECATED nfsfsdFileMode "Use generic-lens or generic-optics with 'fileMode' instead." #-}

-- | The default owner ID for files in the file share (unless the files have another owner ID specified). The default value is @nfsnobody@ .
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdOwnerId :: Lens.Lens' NFSFileShareDefaults (Lude.Maybe Lude.Natural)
nfsfsdOwnerId = Lens.lens (ownerId :: NFSFileShareDefaults -> Lude.Maybe Lude.Natural) (\s a -> s {ownerId = a} :: NFSFileShareDefaults)
{-# DEPRECATED nfsfsdOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The Unix directory mode in the form "nnnn". For example, @0666@ represents the default access mode for all directories inside the file share. The default value is @0777@ .
--
-- /Note:/ Consider using 'directoryMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdDirectoryMode :: Lens.Lens' NFSFileShareDefaults (Lude.Maybe Lude.Text)
nfsfsdDirectoryMode = Lens.lens (directoryMode :: NFSFileShareDefaults -> Lude.Maybe Lude.Text) (\s a -> s {directoryMode = a} :: NFSFileShareDefaults)
{-# DEPRECATED nfsfsdDirectoryMode "Use generic-lens or generic-optics with 'directoryMode' instead." #-}

-- | The default group ID for the file share (unless the files have another group ID specified). The default value is @nfsnobody@ .
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfsfsdGroupId :: Lens.Lens' NFSFileShareDefaults (Lude.Maybe Lude.Natural)
nfsfsdGroupId = Lens.lens (groupId :: NFSFileShareDefaults -> Lude.Maybe Lude.Natural) (\s a -> s {groupId = a} :: NFSFileShareDefaults)
{-# DEPRECATED nfsfsdGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.FromJSON NFSFileShareDefaults where
  parseJSON =
    Lude.withObject
      "NFSFileShareDefaults"
      ( \x ->
          NFSFileShareDefaults'
            Lude.<$> (x Lude..:? "FileMode")
            Lude.<*> (x Lude..:? "OwnerId")
            Lude.<*> (x Lude..:? "DirectoryMode")
            Lude.<*> (x Lude..:? "GroupId")
      )

instance Lude.ToJSON NFSFileShareDefaults where
  toJSON NFSFileShareDefaults' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FileMode" Lude..=) Lude.<$> fileMode,
            ("OwnerId" Lude..=) Lude.<$> ownerId,
            ("DirectoryMode" Lude..=) Lude.<$> directoryMode,
            ("GroupId" Lude..=) Lude.<$> groupId
          ]
      )
