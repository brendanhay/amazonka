{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SharedDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SharedDirectory
  ( SharedDirectory (..),

    -- * Smart constructor
    mkSharedDirectory,

    -- * Lenses
    sdSharedAccountId,
    sdOwnerAccountId,
    sdLastUpdatedDateTime,
    sdShareStatus,
    sdShareMethod,
    sdOwnerDirectoryId,
    sdSharedDirectoryId,
    sdShareNotes,
    sdCreatedDateTime,
  )
where

import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the shared directory in the directory owner account for which the share request in the directory consumer account has been accepted.
--
-- /See:/ 'mkSharedDirectory' smart constructor.
data SharedDirectory = SharedDirectory'
  { -- | Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
    sharedAccountId :: Lude.Maybe Lude.Text,
    -- | Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
    ownerAccountId :: Lude.Maybe Lude.Text,
    -- | The date and time that the shared directory was last updated.
    lastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    -- | Current directory status of the shared AWS Managed Microsoft AD directory.
    shareStatus :: Lude.Maybe ShareStatus,
    -- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
    shareMethod :: Lude.Maybe ShareMethod,
    -- | Identifier of the directory in the directory owner account.
    ownerDirectoryId :: Lude.Maybe Lude.Text,
    -- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
    sharedDirectoryId :: Lude.Maybe Lude.Text,
    -- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
    shareNotes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The date and time that the shared directory was created.
    createdDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SharedDirectory' with the minimum fields required to make a request.
--
-- * 'sharedAccountId' - Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
-- * 'ownerAccountId' - Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
-- * 'lastUpdatedDateTime' - The date and time that the shared directory was last updated.
-- * 'shareStatus' - Current directory status of the shared AWS Managed Microsoft AD directory.
-- * 'shareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
-- * 'ownerDirectoryId' - Identifier of the directory in the directory owner account.
-- * 'sharedDirectoryId' - Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
-- * 'shareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
-- * 'createdDateTime' - The date and time that the shared directory was created.
mkSharedDirectory ::
  SharedDirectory
mkSharedDirectory =
  SharedDirectory'
    { sharedAccountId = Lude.Nothing,
      ownerAccountId = Lude.Nothing,
      lastUpdatedDateTime = Lude.Nothing,
      shareStatus = Lude.Nothing,
      shareMethod = Lude.Nothing,
      ownerDirectoryId = Lude.Nothing,
      sharedDirectoryId = Lude.Nothing,
      shareNotes = Lude.Nothing,
      createdDateTime = Lude.Nothing
    }

-- | Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSharedAccountId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sdSharedAccountId = Lens.lens (sharedAccountId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {sharedAccountId = a} :: SharedDirectory)
{-# DEPRECATED sdSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwnerAccountId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sdOwnerAccountId = Lens.lens (ownerAccountId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccountId = a} :: SharedDirectory)
{-# DEPRECATED sdOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | The date and time that the shared directory was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLastUpdatedDateTime :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Timestamp)
sdLastUpdatedDateTime = Lens.lens (lastUpdatedDateTime :: SharedDirectory -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDateTime = a} :: SharedDirectory)
{-# DEPRECATED sdLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'shareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareStatus :: Lens.Lens' SharedDirectory (Lude.Maybe ShareStatus)
sdShareStatus = Lens.lens (shareStatus :: SharedDirectory -> Lude.Maybe ShareStatus) (\s a -> s {shareStatus = a} :: SharedDirectory)
{-# DEPRECATED sdShareStatus "Use generic-lens or generic-optics with 'shareStatus' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareMethod :: Lens.Lens' SharedDirectory (Lude.Maybe ShareMethod)
sdShareMethod = Lens.lens (shareMethod :: SharedDirectory -> Lude.Maybe ShareMethod) (\s a -> s {shareMethod = a} :: SharedDirectory)
{-# DEPRECATED sdShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | Identifier of the directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdOwnerDirectoryId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sdOwnerDirectoryId = Lens.lens (ownerDirectoryId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {ownerDirectoryId = a} :: SharedDirectory)
{-# DEPRECATED sdOwnerDirectoryId "Use generic-lens or generic-optics with 'ownerDirectoryId' instead." #-}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSharedDirectoryId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sdSharedDirectoryId = Lens.lens (sharedDirectoryId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {sharedDirectoryId = a} :: SharedDirectory)
{-# DEPRECATED sdSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShareNotes :: Lens.Lens' SharedDirectory (Lude.Maybe (Lude.Sensitive Lude.Text))
sdShareNotes = Lens.lens (shareNotes :: SharedDirectory -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {shareNotes = a} :: SharedDirectory)
{-# DEPRECATED sdShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

-- | The date and time that the shared directory was created.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCreatedDateTime :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Timestamp)
sdCreatedDateTime = Lens.lens (createdDateTime :: SharedDirectory -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDateTime = a} :: SharedDirectory)
{-# DEPRECATED sdCreatedDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead." #-}

instance Lude.FromJSON SharedDirectory where
  parseJSON =
    Lude.withObject
      "SharedDirectory"
      ( \x ->
          SharedDirectory'
            Lude.<$> (x Lude..:? "SharedAccountId")
            Lude.<*> (x Lude..:? "OwnerAccountId")
            Lude.<*> (x Lude..:? "LastUpdatedDateTime")
            Lude.<*> (x Lude..:? "ShareStatus")
            Lude.<*> (x Lude..:? "ShareMethod")
            Lude.<*> (x Lude..:? "OwnerDirectoryId")
            Lude.<*> (x Lude..:? "SharedDirectoryId")
            Lude.<*> (x Lude..:? "ShareNotes")
            Lude.<*> (x Lude..:? "CreatedDateTime")
      )
