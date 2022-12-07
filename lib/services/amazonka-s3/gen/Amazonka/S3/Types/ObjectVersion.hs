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
-- Module      : Amazonka.S3.Types.ObjectVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ChecksumAlgorithm
import Amazonka.S3.Types.ObjectVersionStorageClass
import Amazonka.S3.Types.Owner

-- | The version of an object.
--
-- /See:/ 'newObjectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { -- | The algorithm that was used to create a checksum of the object.
    checksumAlgorithm :: Prelude.Maybe [ChecksumAlgorithm],
    -- | The object key.
    key :: Prelude.Maybe ObjectKey,
    -- | Specifies whether the object is (true) or is not (false) the latest
    -- version of an object.
    isLatest :: Prelude.Maybe Prelude.Bool,
    -- | Size in bytes of the object.
    size :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the owner of the object.
    owner :: Prelude.Maybe Owner,
    -- | Date and time the object was last modified.
    lastModified :: Prelude.Maybe Data.ISO8601,
    -- | The class of storage used to store the object.
    storageClass :: Prelude.Maybe ObjectVersionStorageClass,
    -- | The entity tag is an MD5 hash of that version of the object.
    eTag :: Prelude.Maybe ETag,
    -- | Version ID of an object.
    versionId :: Prelude.Maybe ObjectVersionId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'objectVersion_checksumAlgorithm' - The algorithm that was used to create a checksum of the object.
--
-- 'key', 'objectVersion_key' - The object key.
--
-- 'isLatest', 'objectVersion_isLatest' - Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
--
-- 'size', 'objectVersion_size' - Size in bytes of the object.
--
-- 'owner', 'objectVersion_owner' - Specifies the owner of the object.
--
-- 'lastModified', 'objectVersion_lastModified' - Date and time the object was last modified.
--
-- 'storageClass', 'objectVersion_storageClass' - The class of storage used to store the object.
--
-- 'eTag', 'objectVersion_eTag' - The entity tag is an MD5 hash of that version of the object.
--
-- 'versionId', 'objectVersion_versionId' - Version ID of an object.
newObjectVersion ::
  ObjectVersion
newObjectVersion =
  ObjectVersion'
    { checksumAlgorithm = Prelude.Nothing,
      key = Prelude.Nothing,
      isLatest = Prelude.Nothing,
      size = Prelude.Nothing,
      owner = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      eTag = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The algorithm that was used to create a checksum of the object.
objectVersion_checksumAlgorithm :: Lens.Lens' ObjectVersion (Prelude.Maybe [ChecksumAlgorithm])
objectVersion_checksumAlgorithm = Lens.lens (\ObjectVersion' {checksumAlgorithm} -> checksumAlgorithm) (\s@ObjectVersion' {} a -> s {checksumAlgorithm = a} :: ObjectVersion) Prelude.. Lens.mapping Lens.coerced

-- | The object key.
objectVersion_key :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectKey)
objectVersion_key = Lens.lens (\ObjectVersion' {key} -> key) (\s@ObjectVersion' {} a -> s {key = a} :: ObjectVersion)

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
objectVersion_isLatest :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.Bool)
objectVersion_isLatest = Lens.lens (\ObjectVersion' {isLatest} -> isLatest) (\s@ObjectVersion' {} a -> s {isLatest = a} :: ObjectVersion)

-- | Size in bytes of the object.
objectVersion_size :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.Integer)
objectVersion_size = Lens.lens (\ObjectVersion' {size} -> size) (\s@ObjectVersion' {} a -> s {size = a} :: ObjectVersion)

-- | Specifies the owner of the object.
objectVersion_owner :: Lens.Lens' ObjectVersion (Prelude.Maybe Owner)
objectVersion_owner = Lens.lens (\ObjectVersion' {owner} -> owner) (\s@ObjectVersion' {} a -> s {owner = a} :: ObjectVersion)

-- | Date and time the object was last modified.
objectVersion_lastModified :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.UTCTime)
objectVersion_lastModified = Lens.lens (\ObjectVersion' {lastModified} -> lastModified) (\s@ObjectVersion' {} a -> s {lastModified = a} :: ObjectVersion) Prelude.. Lens.mapping Data._Time

-- | The class of storage used to store the object.
objectVersion_storageClass :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectVersionStorageClass)
objectVersion_storageClass = Lens.lens (\ObjectVersion' {storageClass} -> storageClass) (\s@ObjectVersion' {} a -> s {storageClass = a} :: ObjectVersion)

-- | The entity tag is an MD5 hash of that version of the object.
objectVersion_eTag :: Lens.Lens' ObjectVersion (Prelude.Maybe ETag)
objectVersion_eTag = Lens.lens (\ObjectVersion' {eTag} -> eTag) (\s@ObjectVersion' {} a -> s {eTag = a} :: ObjectVersion)

-- | Version ID of an object.
objectVersion_versionId :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectVersionId)
objectVersion_versionId = Lens.lens (\ObjectVersion' {versionId} -> versionId) (\s@ObjectVersion' {} a -> s {versionId = a} :: ObjectVersion)

instance Data.FromXML ObjectVersion where
  parseXML x =
    ObjectVersion'
      Prelude.<$> (Core.may (Data.parseXMLList "ChecksumAlgorithm") x)
      Prelude.<*> (x Data..@? "Key")
      Prelude.<*> (x Data..@? "IsLatest")
      Prelude.<*> (x Data..@? "Size")
      Prelude.<*> (x Data..@? "Owner")
      Prelude.<*> (x Data..@? "LastModified")
      Prelude.<*> (x Data..@? "StorageClass")
      Prelude.<*> (x Data..@? "ETag")
      Prelude.<*> (x Data..@? "VersionId")

instance Prelude.Hashable ObjectVersion where
  hashWithSalt _salt ObjectVersion' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` isLatest
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` storageClass
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData ObjectVersion where
  rnf ObjectVersion' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf isLatest
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf storageClass
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf versionId
