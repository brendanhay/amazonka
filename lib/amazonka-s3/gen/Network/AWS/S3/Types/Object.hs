{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Object
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Object
  ( Object (..),

    -- * Smart constructor
    mkObject,

    -- * Lenses
    oOwner,
    oETag,
    oSize,
    oKey,
    oStorageClass,
    oLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectStorageClass
import Network.AWS.S3.Types.Owner

-- | An object consists of data and its descriptive metadata.
--
-- /See:/ 'mkObject' smart constructor.
data Object = Object'
  { owner :: Lude.Maybe Owner,
    eTag :: ETag,
    size :: Lude.Int,
    key :: ObjectKey,
    storageClass :: ObjectStorageClass,
    lastModified :: Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Object' with the minimum fields required to make a request.
--
-- * 'eTag' - The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:
--
--
--     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.
--
--
--     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.
--
--
--     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
--
--
-- * 'key' - The name that you assign to an object. You use the object key to retrieve the object.
-- * 'lastModified' - The date the Object was Last Modified
-- * 'owner' - The owner of the object
-- * 'size' - Size in bytes of the object
-- * 'storageClass' - The class of storage used to store the object.
mkObject ::
  -- | 'eTag'
  ETag ->
  -- | 'size'
  Lude.Int ->
  -- | 'key'
  ObjectKey ->
  -- | 'storageClass'
  ObjectStorageClass ->
  -- | 'lastModified'
  Lude.DateTime ->
  Object
mkObject pETag_ pSize_ pKey_ pStorageClass_ pLastModified_ =
  Object'
    { owner = Lude.Nothing,
      eTag = pETag_,
      size = pSize_,
      key = pKey_,
      storageClass = pStorageClass_,
      lastModified = pLastModified_
    }

-- | The owner of the object
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOwner :: Lens.Lens' Object (Lude.Maybe Owner)
oOwner = Lens.lens (owner :: Object -> Lude.Maybe Owner) (\s a -> s {owner = a} :: Object)
{-# DEPRECATED oOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:
--
--
--     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.
--
--
--     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.
--
--
--     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
--
--
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oETag :: Lens.Lens' Object ETag
oETag = Lens.lens (eTag :: Object -> ETag) (\s a -> s {eTag = a} :: Object)
{-# DEPRECATED oETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | Size in bytes of the object
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oSize :: Lens.Lens' Object Lude.Int
oSize = Lens.lens (size :: Object -> Lude.Int) (\s a -> s {size = a} :: Object)
{-# DEPRECATED oSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The name that you assign to an object. You use the object key to retrieve the object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oKey :: Lens.Lens' Object ObjectKey
oKey = Lens.lens (key :: Object -> ObjectKey) (\s a -> s {key = a} :: Object)
{-# DEPRECATED oKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oStorageClass :: Lens.Lens' Object ObjectStorageClass
oStorageClass = Lens.lens (storageClass :: Object -> ObjectStorageClass) (\s a -> s {storageClass = a} :: Object)
{-# DEPRECATED oStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The date the Object was Last Modified
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oLastModified :: Lens.Lens' Object Lude.DateTime
oLastModified = Lens.lens (lastModified :: Object -> Lude.DateTime) (\s a -> s {lastModified = a} :: Object)
{-# DEPRECATED oLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromXML Object where
  parseXML x =
    Object'
      Lude.<$> (x Lude..@? "Owner")
      Lude.<*> (x Lude..@ "ETag")
      Lude.<*> (x Lude..@ "Size")
      Lude.<*> (x Lude..@ "Key")
      Lude.<*> (x Lude..@ "StorageClass")
      Lude.<*> (x Lude..@ "LastModified")
