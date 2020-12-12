{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerEntry
  ( DeleteMarkerEntry (..),

    -- * Smart constructor
    mkDeleteMarkerEntry,

    -- * Lenses
    dmeVersionId,
    dmeIsLatest,
    dmeOwner,
    dmeKey,
    dmeLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Owner

-- | Information about the delete marker.
--
-- /See:/ 'mkDeleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { versionId ::
      Lude.Maybe ObjectVersionId,
    isLatest :: Lude.Maybe Lude.Bool,
    owner :: Lude.Maybe Owner,
    key :: Lude.Maybe ObjectKey,
    lastModified :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMarkerEntry' with the minimum fields required to make a request.
--
-- * 'isLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
-- * 'key' - The object key.
-- * 'lastModified' - Date and time the object was last modified.
-- * 'owner' - The account that created the delete marker.>
-- * 'versionId' - Version ID of an object.
mkDeleteMarkerEntry ::
  DeleteMarkerEntry
mkDeleteMarkerEntry =
  DeleteMarkerEntry'
    { versionId = Lude.Nothing,
      isLatest = Lude.Nothing,
      owner = Lude.Nothing,
      key = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | Version ID of an object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeVersionId :: Lens.Lens' DeleteMarkerEntry (Lude.Maybe ObjectVersionId)
dmeVersionId = Lens.lens (versionId :: DeleteMarkerEntry -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeleteMarkerEntry)
{-# DEPRECATED dmeVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- /Note:/ Consider using 'isLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeIsLatest :: Lens.Lens' DeleteMarkerEntry (Lude.Maybe Lude.Bool)
dmeIsLatest = Lens.lens (isLatest :: DeleteMarkerEntry -> Lude.Maybe Lude.Bool) (\s a -> s {isLatest = a} :: DeleteMarkerEntry)
{-# DEPRECATED dmeIsLatest "Use generic-lens or generic-optics with 'isLatest' instead." #-}

-- | The account that created the delete marker.>
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeOwner :: Lens.Lens' DeleteMarkerEntry (Lude.Maybe Owner)
dmeOwner = Lens.lens (owner :: DeleteMarkerEntry -> Lude.Maybe Owner) (\s a -> s {owner = a} :: DeleteMarkerEntry)
{-# DEPRECATED dmeOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeKey :: Lens.Lens' DeleteMarkerEntry (Lude.Maybe ObjectKey)
dmeKey = Lens.lens (key :: DeleteMarkerEntry -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: DeleteMarkerEntry)
{-# DEPRECATED dmeKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Date and time the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeLastModified :: Lens.Lens' DeleteMarkerEntry (Lude.Maybe Lude.DateTime)
dmeLastModified = Lens.lens (lastModified :: DeleteMarkerEntry -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: DeleteMarkerEntry)
{-# DEPRECATED dmeLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML DeleteMarkerEntry where
  parseXML x =
    DeleteMarkerEntry'
      Lude.<$> (x Lude..@? "VersionId")
      Lude.<*> (x Lude..@? "IsLatest")
      Lude.<*> (x Lude..@? "Owner")
      Lude.<*> (x Lude..@? "Key")
      Lude.<*> (x Lude..@? "LastModified")
