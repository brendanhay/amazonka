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
    oETag,
    oKey,
    oLastModified,
    oOwner,
    oSize,
    oStorageClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectStorageClass as Types
import qualified Network.AWS.S3.Types.Owner as Types

-- | An object consists of data and its descriptive metadata.
--
-- /See:/ 'mkObject' smart constructor.
data Object = Object'
  { -- | The entity tag is a hash of the object. The ETag reflects changes only to the contents of an object, not its metadata. The ETag may or may not be an MD5 digest of the object data. Whether or not it is depends on how the object was created and how it is encrypted as described below:
    --
    --
    --     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest of their object data.
    --
    --
    --     * Objects created by the PUT Object, POST Object, or Copy operation, or through the AWS Management Console, and are encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest of their object data.
    --
    --
    --     * If an object is created by either the Multipart Upload or Part Copy operation, the ETag is not an MD5 digest, regardless of the method of encryption.
    eTag :: Types.ETag,
    -- | The name that you assign to an object. You use the object key to retrieve the object.
    key :: Types.ObjectKey,
    -- | The date the Object was Last Modified
    lastModified :: Core.UTCTime,
    -- | The owner of the object
    owner :: Core.Maybe Types.Owner,
    -- | Size in bytes of the object
    size :: Core.Int,
    -- | The class of storage used to store the object.
    storageClass :: Types.ObjectStorageClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Object' value with any optional fields omitted.
mkObject ::
  -- | 'eTag'
  Types.ETag ->
  -- | 'key'
  Types.ObjectKey ->
  -- | 'lastModified'
  Core.UTCTime ->
  -- | 'size'
  Core.Int ->
  -- | 'storageClass'
  Types.ObjectStorageClass ->
  Object
mkObject eTag key lastModified size storageClass =
  Object'
    { eTag,
      key,
      lastModified,
      owner = Core.Nothing,
      size,
      storageClass
    }

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
oETag :: Lens.Lens' Object Types.ETag
oETag = Lens.field @"eTag"
{-# DEPRECATED oETag "Use generic-lens or generic-optics with 'eTag' instead." #-}

-- | The name that you assign to an object. You use the object key to retrieve the object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oKey :: Lens.Lens' Object Types.ObjectKey
oKey = Lens.field @"key"
{-# DEPRECATED oKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The date the Object was Last Modified
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oLastModified :: Lens.Lens' Object Core.UTCTime
oLastModified = Lens.field @"lastModified"
{-# DEPRECATED oLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The owner of the object
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOwner :: Lens.Lens' Object (Core.Maybe Types.Owner)
oOwner = Lens.field @"owner"
{-# DEPRECATED oOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Size in bytes of the object
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oSize :: Lens.Lens' Object Core.Int
oSize = Lens.field @"size"
{-# DEPRECATED oSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oStorageClass :: Lens.Lens' Object Types.ObjectStorageClass
oStorageClass = Lens.field @"storageClass"
{-# DEPRECATED oStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Core.FromXML Object where
  parseXML x =
    Object'
      Core.<$> (x Core..@ "ETag")
      Core.<*> (x Core..@ "Key")
      Core.<*> (x Core..@ "LastModified")
      Core.<*> (x Core..@? "Owner")
      Core.<*> (x Core..@ "Size")
      Core.<*> (x Core..@ "StorageClass")
