{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectVersion
  ( ObjectVersion (..),

    -- * Smart constructor
    mkObjectVersion,

    -- * Lenses
    ovETag,
    ovVersionId,
    ovSize,
    ovIsLatest,
    ovOwner,
    ovKey,
    ovStorageClass,
    ovLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectVersionStorageClass
import Network.AWS.S3.Types.Owner

-- | The version of an object.
--
-- /See:/ 'mkObjectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { -- | The entity tag is an MD5 hash of that version of the object.
    eTag :: Lude.Maybe ETag,
    -- | Version ID of an object.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | Size in bytes of the object.
    size :: Lude.Maybe Lude.Int,
    -- | Specifies whether the object is (true) or is not (false) the latest version of an object.
    isLatest :: Lude.Maybe Lude.Bool,
    -- | Specifies the owner of the object.
    owner :: Lude.Maybe Owner,
    -- | The object key.
    key :: Lude.Maybe ObjectKey,
    -- | The class of storage used to store the object.
    storageClass :: Lude.Maybe ObjectVersionStorageClass,
    -- | Date and time the object was last modified.
    lastModified :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectVersion' with the minimum fields required to make a request.
--
-- * 'eTag' - The entity tag is an MD5 hash of that version of the object.
-- * 'versionId' - Version ID of an object.
-- * 'size' - Size in bytes of the object.
-- * 'isLatest' - Specifies whether the object is (true) or is not (false) the latest version of an object.
-- * 'owner' - Specifies the owner of the object.
-- * 'key' - The object key.
-- * 'storageClass' - The class of storage used to store the object.
-- * 'lastModified' - Date and time the object was last modified.
mkObjectVersion ::
  ObjectVersion
mkObjectVersion =
  ObjectVersion'
    { eTag = Lude.Nothing,
      versionId = Lude.Nothing,
      size = Lude.Nothing,
      isLatest = Lude.Nothing,
      owner = Lude.Nothing,
      key = Lude.Nothing,
      storageClass = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | The entity tag is an MD5 hash of that version of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovETag :: Lens.Lens' ObjectVersion (Lude.Maybe ETag)
ovETag = Lens.lens (eTag :: ObjectVersion -> Lude.Maybe ETag) (\s a -> s {eTag = a} :: ObjectVersion)
{-# DEPRECATED ovETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Version ID of an object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovVersionId :: Lens.Lens' ObjectVersion (Lude.Maybe ObjectVersionId)
ovVersionId = Lens.lens (versionId :: ObjectVersion -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: ObjectVersion)
{-# DEPRECATED ovVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Size in bytes of the object.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovSize :: Lens.Lens' ObjectVersion (Lude.Maybe Lude.Int)
ovSize = Lens.lens (size :: ObjectVersion -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: ObjectVersion)
{-# DEPRECATED ovSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- /Note:/ Consider using 'isLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovIsLatest :: Lens.Lens' ObjectVersion (Lude.Maybe Lude.Bool)
ovIsLatest = Lens.lens (isLatest :: ObjectVersion -> Lude.Maybe Lude.Bool) (\s a -> s {isLatest = a} :: ObjectVersion)
{-# DEPRECATED ovIsLatest "Use generic-lens or generic-optics with 'isLatest' instead." #-}

-- | Specifies the owner of the object.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovOwner :: Lens.Lens' ObjectVersion (Lude.Maybe Owner)
ovOwner = Lens.lens (owner :: ObjectVersion -> Lude.Maybe Owner) (\s a -> s {owner = a} :: ObjectVersion)
{-# DEPRECATED ovOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovKey :: Lens.Lens' ObjectVersion (Lude.Maybe ObjectKey)
ovKey = Lens.lens (key :: ObjectVersion -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: ObjectVersion)
{-# DEPRECATED ovKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovStorageClass :: Lens.Lens' ObjectVersion (Lude.Maybe ObjectVersionStorageClass)
ovStorageClass = Lens.lens (storageClass :: ObjectVersion -> Lude.Maybe ObjectVersionStorageClass) (\s a -> s {storageClass = a} :: ObjectVersion)
{-# DEPRECATED ovStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Date and time the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovLastModified :: Lens.Lens' ObjectVersion (Lude.Maybe Lude.DateTime)
ovLastModified = Lens.lens (lastModified :: ObjectVersion -> Lude.Maybe Lude.DateTime) (\s a -> s {lastModified = a} :: ObjectVersion)
{-# DEPRECATED ovLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML ObjectVersion where
  parseXML x =
    ObjectVersion'
      Lude.<$> (x Lude..@? "ETag")
      Lude.<*> (x Lude..@? "VersionId")
      Lude.<*> (x Lude..@? "Size")
      Lude.<*> (x Lude..@? "IsLatest")
      Lude.<*> (x Lude..@? "Owner")
      Lude.<*> (x Lude..@? "Key")
      Lude.<*> (x Lude..@? "StorageClass")
      Lude.<*> (x Lude..@? "LastModified")
