{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Object
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Object where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectStorageClass
import Network.AWS.S3.Types.Owner

-- | An object consists of data and its descriptive metadata.
--
-- /See:/ 'newObject' smart constructor.
data Object = Object'
  { -- | The owner of the object
    owner :: Prelude.Maybe Owner,
    -- | The entity tag is a hash of the object. The ETag reflects changes only
    -- to the contents of an object, not its metadata. The ETag may or may not
    -- be an MD5 digest of the object data. Whether or not it is depends on how
    -- the object was created and how it is encrypted as described below:
    --
    -- -   Objects created by the PUT Object, POST Object, or Copy operation,
    --     or through the AWS Management Console, and are encrypted by SSE-S3
    --     or plaintext, have ETags that are an MD5 digest of their object
    --     data.
    --
    -- -   Objects created by the PUT Object, POST Object, or Copy operation,
    --     or through the AWS Management Console, and are encrypted by SSE-C or
    --     SSE-KMS, have ETags that are not an MD5 digest of their object data.
    --
    -- -   If an object is created by either the Multipart Upload or Part Copy
    --     operation, the ETag is not an MD5 digest, regardless of the method
    --     of encryption.
    eTag :: ETag,
    -- | Size in bytes of the object
    size :: Prelude.Int,
    -- | The name that you assign to an object. You use the object key to
    -- retrieve the object.
    key :: ObjectKey,
    -- | The class of storage used to store the object.
    storageClass :: ObjectStorageClass,
    -- | Creation date of the object.
    lastModified :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Object' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'object_owner' - The owner of the object
--
-- 'eTag', 'object_eTag' - The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata. The ETag may or may not
-- be an MD5 digest of the object data. Whether or not it is depends on how
-- the object was created and how it is encrypted as described below:
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the AWS Management Console, and are encrypted by SSE-S3
--     or plaintext, have ETags that are an MD5 digest of their object
--     data.
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the AWS Management Console, and are encrypted by SSE-C or
--     SSE-KMS, have ETags that are not an MD5 digest of their object data.
--
-- -   If an object is created by either the Multipart Upload or Part Copy
--     operation, the ETag is not an MD5 digest, regardless of the method
--     of encryption.
--
-- 'size', 'object_size' - Size in bytes of the object
--
-- 'key', 'object_key' - The name that you assign to an object. You use the object key to
-- retrieve the object.
--
-- 'storageClass', 'object_storageClass' - The class of storage used to store the object.
--
-- 'lastModified', 'object_lastModified' - Creation date of the object.
newObject ::
  -- | 'eTag'
  ETag ->
  -- | 'size'
  Prelude.Int ->
  -- | 'key'
  ObjectKey ->
  -- | 'storageClass'
  ObjectStorageClass ->
  -- | 'lastModified'
  Prelude.UTCTime ->
  Object
newObject
  pETag_
  pSize_
  pKey_
  pStorageClass_
  pLastModified_ =
    Object'
      { owner = Prelude.Nothing,
        eTag = pETag_,
        size = pSize_,
        key = pKey_,
        storageClass = pStorageClass_,
        lastModified = Prelude._Time Lens.# pLastModified_
      }

-- | The owner of the object
object_owner :: Lens.Lens' Object (Prelude.Maybe Owner)
object_owner = Lens.lens (\Object' {owner} -> owner) (\s@Object' {} a -> s {owner = a} :: Object)

-- | The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata. The ETag may or may not
-- be an MD5 digest of the object data. Whether or not it is depends on how
-- the object was created and how it is encrypted as described below:
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the AWS Management Console, and are encrypted by SSE-S3
--     or plaintext, have ETags that are an MD5 digest of their object
--     data.
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the AWS Management Console, and are encrypted by SSE-C or
--     SSE-KMS, have ETags that are not an MD5 digest of their object data.
--
-- -   If an object is created by either the Multipart Upload or Part Copy
--     operation, the ETag is not an MD5 digest, regardless of the method
--     of encryption.
object_eTag :: Lens.Lens' Object ETag
object_eTag = Lens.lens (\Object' {eTag} -> eTag) (\s@Object' {} a -> s {eTag = a} :: Object)

-- | Size in bytes of the object
object_size :: Lens.Lens' Object Prelude.Int
object_size = Lens.lens (\Object' {size} -> size) (\s@Object' {} a -> s {size = a} :: Object)

-- | The name that you assign to an object. You use the object key to
-- retrieve the object.
object_key :: Lens.Lens' Object ObjectKey
object_key = Lens.lens (\Object' {key} -> key) (\s@Object' {} a -> s {key = a} :: Object)

-- | The class of storage used to store the object.
object_storageClass :: Lens.Lens' Object ObjectStorageClass
object_storageClass = Lens.lens (\Object' {storageClass} -> storageClass) (\s@Object' {} a -> s {storageClass = a} :: Object)

-- | Creation date of the object.
object_lastModified :: Lens.Lens' Object Prelude.UTCTime
object_lastModified = Lens.lens (\Object' {lastModified} -> lastModified) (\s@Object' {} a -> s {lastModified = a} :: Object) Prelude.. Prelude._Time

instance Prelude.FromXML Object where
  parseXML x =
    Object'
      Prelude.<$> (x Prelude..@? "Owner")
      Prelude.<*> (x Prelude..@ "ETag")
      Prelude.<*> (x Prelude..@ "Size")
      Prelude.<*> (x Prelude..@ "Key")
      Prelude.<*> (x Prelude..@ "StorageClass")
      Prelude.<*> (x Prelude..@ "LastModified")

instance Prelude.Hashable Object

instance Prelude.NFData Object
