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
-- Module      : Amazonka.S3.Types.Object
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Object where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ChecksumAlgorithm
import Amazonka.S3.Types.ObjectStorageClass
import Amazonka.S3.Types.Owner

-- | An object consists of data and its descriptive metadata.
--
-- /See:/ 'newObject' smart constructor.
data Object = Object'
  { -- | The algorithm that was used to create a checksum of the object.
    checksumAlgorithm :: Prelude.Maybe [ChecksumAlgorithm],
    -- | The owner of the object
    owner :: Prelude.Maybe Owner,
    -- | The entity tag is a hash of the object. The ETag reflects changes only
    -- to the contents of an object, not its metadata. The ETag may or may not
    -- be an MD5 digest of the object data. Whether or not it is depends on how
    -- the object was created and how it is encrypted as described below:
    --
    -- -   Objects created by the PUT Object, POST Object, or Copy operation,
    --     or through the Amazon Web Services Management Console, and are
    --     encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest
    --     of their object data.
    --
    -- -   Objects created by the PUT Object, POST Object, or Copy operation,
    --     or through the Amazon Web Services Management Console, and are
    --     encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest
    --     of their object data.
    --
    -- -   If an object is created by either the Multipart Upload or Part Copy
    --     operation, the ETag is not an MD5 digest, regardless of the method
    --     of encryption. If an object is larger than 16 MB, the Amazon Web
    --     Services Management Console will upload or copy that object as a
    --     Multipart Upload, and therefore the ETag will not be an MD5 digest.
    eTag :: ETag,
    -- | Size in bytes of the object
    size :: Prelude.Integer,
    -- | The name that you assign to an object. You use the object key to
    -- retrieve the object.
    key :: ObjectKey,
    -- | The class of storage used to store the object.
    storageClass :: ObjectStorageClass,
    -- | Creation date of the object.
    lastModified :: Data.RFC822
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Object' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'object_checksumAlgorithm' - The algorithm that was used to create a checksum of the object.
--
-- 'owner', 'object_owner' - The owner of the object
--
-- 'eTag', 'object_eTag' - The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata. The ETag may or may not
-- be an MD5 digest of the object data. Whether or not it is depends on how
-- the object was created and how it is encrypted as described below:
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the Amazon Web Services Management Console, and are
--     encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest
--     of their object data.
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the Amazon Web Services Management Console, and are
--     encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest
--     of their object data.
--
-- -   If an object is created by either the Multipart Upload or Part Copy
--     operation, the ETag is not an MD5 digest, regardless of the method
--     of encryption. If an object is larger than 16 MB, the Amazon Web
--     Services Management Console will upload or copy that object as a
--     Multipart Upload, and therefore the ETag will not be an MD5 digest.
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
  Prelude.Integer ->
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
      { checksumAlgorithm = Prelude.Nothing,
        owner = Prelude.Nothing,
        eTag = pETag_,
        size = pSize_,
        key = pKey_,
        storageClass = pStorageClass_,
        lastModified = Data._Time Lens.# pLastModified_
      }

-- | The algorithm that was used to create a checksum of the object.
object_checksumAlgorithm :: Lens.Lens' Object (Prelude.Maybe [ChecksumAlgorithm])
object_checksumAlgorithm = Lens.lens (\Object' {checksumAlgorithm} -> checksumAlgorithm) (\s@Object' {} a -> s {checksumAlgorithm = a} :: Object) Prelude.. Lens.mapping Lens.coerced

-- | The owner of the object
object_owner :: Lens.Lens' Object (Prelude.Maybe Owner)
object_owner = Lens.lens (\Object' {owner} -> owner) (\s@Object' {} a -> s {owner = a} :: Object)

-- | The entity tag is a hash of the object. The ETag reflects changes only
-- to the contents of an object, not its metadata. The ETag may or may not
-- be an MD5 digest of the object data. Whether or not it is depends on how
-- the object was created and how it is encrypted as described below:
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the Amazon Web Services Management Console, and are
--     encrypted by SSE-S3 or plaintext, have ETags that are an MD5 digest
--     of their object data.
--
-- -   Objects created by the PUT Object, POST Object, or Copy operation,
--     or through the Amazon Web Services Management Console, and are
--     encrypted by SSE-C or SSE-KMS, have ETags that are not an MD5 digest
--     of their object data.
--
-- -   If an object is created by either the Multipart Upload or Part Copy
--     operation, the ETag is not an MD5 digest, regardless of the method
--     of encryption. If an object is larger than 16 MB, the Amazon Web
--     Services Management Console will upload or copy that object as a
--     Multipart Upload, and therefore the ETag will not be an MD5 digest.
object_eTag :: Lens.Lens' Object ETag
object_eTag = Lens.lens (\Object' {eTag} -> eTag) (\s@Object' {} a -> s {eTag = a} :: Object)

-- | Size in bytes of the object
object_size :: Lens.Lens' Object Prelude.Integer
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
object_lastModified = Lens.lens (\Object' {lastModified} -> lastModified) (\s@Object' {} a -> s {lastModified = a} :: Object) Prelude.. Data._Time

instance Data.FromXML Object where
  parseXML x =
    Object'
      Prelude.<$> (Core.may (Data.parseXMLList "ChecksumAlgorithm") x)
      Prelude.<*> (x Data..@? "Owner")
      Prelude.<*> (x Data..@ "ETag")
      Prelude.<*> (x Data..@ "Size")
      Prelude.<*> (x Data..@ "Key")
      Prelude.<*> (x Data..@ "StorageClass")
      Prelude.<*> (x Data..@ "LastModified")

instance Prelude.Hashable Object where
  hashWithSalt _salt Object' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData Object where
  rnf Object' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf lastModified
