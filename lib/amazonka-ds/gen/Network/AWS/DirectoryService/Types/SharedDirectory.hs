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
    sSharedAccountId,
    sOwnerAccountId,
    sLastUpdatedDateTime,
    sShareStatus,
    sShareMethod,
    sOwnerDirectoryId,
    sSharedDirectoryId,
    sShareNotes,
    sCreatedDateTime,
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
  { sharedAccountId ::
      Lude.Maybe Lude.Text,
    ownerAccountId :: Lude.Maybe Lude.Text,
    lastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    shareStatus :: Lude.Maybe ShareStatus,
    shareMethod :: Lude.Maybe ShareMethod,
    ownerDirectoryId :: Lude.Maybe Lude.Text,
    sharedDirectoryId :: Lude.Maybe Lude.Text,
    shareNotes :: Lude.Maybe (Lude.Sensitive Lude.Text),
    createdDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SharedDirectory' with the minimum fields required to make a request.
--
-- * 'createdDateTime' - The date and time that the shared directory was created.
-- * 'lastUpdatedDateTime' - The date and time that the shared directory was last updated.
-- * 'ownerAccountId' - Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
-- * 'ownerDirectoryId' - Identifier of the directory in the directory owner account.
-- * 'shareMethod' - The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
-- * 'shareNotes' - A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
-- * 'shareStatus' - Current directory status of the shared AWS Managed Microsoft AD directory.
-- * 'sharedAccountId' - Identifier of the directory consumer account that has access to the shared directory (@OwnerDirectoryId@ ) in the directory owner account.
-- * 'sharedDirectoryId' - Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
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
sSharedAccountId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sSharedAccountId = Lens.lens (sharedAccountId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {sharedAccountId = a} :: SharedDirectory)
{-# DEPRECATED sSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Identifier of the directory owner account, which contains the directory that has been shared to the consumer account.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerAccountId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sOwnerAccountId = Lens.lens (ownerAccountId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccountId = a} :: SharedDirectory)
{-# DEPRECATED sOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | The date and time that the shared directory was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastUpdatedDateTime :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Timestamp)
sLastUpdatedDateTime = Lens.lens (lastUpdatedDateTime :: SharedDirectory -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDateTime = a} :: SharedDirectory)
{-# DEPRECATED sLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | Current directory status of the shared AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'shareStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShareStatus :: Lens.Lens' SharedDirectory (Lude.Maybe ShareStatus)
sShareStatus = Lens.lens (shareStatus :: SharedDirectory -> Lude.Maybe ShareStatus) (\s a -> s {shareStatus = a} :: SharedDirectory)
{-# DEPRECATED sShareStatus "Use generic-lens or generic-optics with 'shareStatus' instead." #-}

-- | The method used when sharing a directory to determine whether the directory should be shared within your AWS organization (@ORGANIZATIONS@ ) or with any AWS account by sending a shared directory request (@HANDSHAKE@ ).
--
-- /Note:/ Consider using 'shareMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShareMethod :: Lens.Lens' SharedDirectory (Lude.Maybe ShareMethod)
sShareMethod = Lens.lens (shareMethod :: SharedDirectory -> Lude.Maybe ShareMethod) (\s a -> s {shareMethod = a} :: SharedDirectory)
{-# DEPRECATED sShareMethod "Use generic-lens or generic-optics with 'shareMethod' instead." #-}

-- | Identifier of the directory in the directory owner account.
--
-- /Note:/ Consider using 'ownerDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwnerDirectoryId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sOwnerDirectoryId = Lens.lens (ownerDirectoryId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {ownerDirectoryId = a} :: SharedDirectory)
{-# DEPRECATED sOwnerDirectoryId "Use generic-lens or generic-optics with 'ownerDirectoryId' instead." #-}

-- | Identifier of the shared directory in the directory consumer account. This identifier is different for each directory owner account.
--
-- /Note:/ Consider using 'sharedDirectoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSharedDirectoryId :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Text)
sSharedDirectoryId = Lens.lens (sharedDirectoryId :: SharedDirectory -> Lude.Maybe Lude.Text) (\s a -> s {sharedDirectoryId = a} :: SharedDirectory)
{-# DEPRECATED sSharedDirectoryId "Use generic-lens or generic-optics with 'sharedDirectoryId' instead." #-}

-- | A directory share request that is sent by the directory owner to the directory consumer. The request includes a typed message to help the directory consumer administrator determine whether to approve or reject the share invitation.
--
-- /Note:/ Consider using 'shareNotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sShareNotes :: Lens.Lens' SharedDirectory (Lude.Maybe (Lude.Sensitive Lude.Text))
sShareNotes = Lens.lens (shareNotes :: SharedDirectory -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {shareNotes = a} :: SharedDirectory)
{-# DEPRECATED sShareNotes "Use generic-lens or generic-optics with 'shareNotes' instead." #-}

-- | The date and time that the shared directory was created.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreatedDateTime :: Lens.Lens' SharedDirectory (Lude.Maybe Lude.Timestamp)
sCreatedDateTime = Lens.lens (createdDateTime :: SharedDirectory -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDateTime = a} :: SharedDirectory)
{-# DEPRECATED sCreatedDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead." #-}

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
