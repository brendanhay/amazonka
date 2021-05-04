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
-- Module      : Network.AWS.S3.Types.ObjectVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectVersionStorageClass
import Network.AWS.S3.Types.Owner

-- | The version of an object.
--
-- /See:/ 'newObjectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { -- | The entity tag is an MD5 hash of that version of the object.
    eTag :: Prelude.Maybe ETag,
    -- | The object key.
    key :: Prelude.Maybe ObjectKey,
    -- | Specifies whether the object is (true) or is not (false) the latest
    -- version of an object.
    isLatest :: Prelude.Maybe Prelude.Bool,
    -- | The class of storage used to store the object.
    storageClass :: Prelude.Maybe ObjectVersionStorageClass,
    -- | Version ID of an object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Specifies the owner of the object.
    owner :: Prelude.Maybe Owner,
    -- | Date and time the object was last modified.
    lastModified :: Prelude.Maybe Prelude.ISO8601,
    -- | Size in bytes of the object.
    size :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'objectVersion_eTag' - The entity tag is an MD5 hash of that version of the object.
--
-- 'key', 'objectVersion_key' - The object key.
--
-- 'isLatest', 'objectVersion_isLatest' - Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
--
-- 'storageClass', 'objectVersion_storageClass' - The class of storage used to store the object.
--
-- 'versionId', 'objectVersion_versionId' - Version ID of an object.
--
-- 'owner', 'objectVersion_owner' - Specifies the owner of the object.
--
-- 'lastModified', 'objectVersion_lastModified' - Date and time the object was last modified.
--
-- 'size', 'objectVersion_size' - Size in bytes of the object.
newObjectVersion ::
  ObjectVersion
newObjectVersion =
  ObjectVersion'
    { eTag = Prelude.Nothing,
      key = Prelude.Nothing,
      isLatest = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      versionId = Prelude.Nothing,
      owner = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      size = Prelude.Nothing
    }

-- | The entity tag is an MD5 hash of that version of the object.
objectVersion_eTag :: Lens.Lens' ObjectVersion (Prelude.Maybe ETag)
objectVersion_eTag = Lens.lens (\ObjectVersion' {eTag} -> eTag) (\s@ObjectVersion' {} a -> s {eTag = a} :: ObjectVersion)

-- | The object key.
objectVersion_key :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectKey)
objectVersion_key = Lens.lens (\ObjectVersion' {key} -> key) (\s@ObjectVersion' {} a -> s {key = a} :: ObjectVersion)

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
objectVersion_isLatest :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.Bool)
objectVersion_isLatest = Lens.lens (\ObjectVersion' {isLatest} -> isLatest) (\s@ObjectVersion' {} a -> s {isLatest = a} :: ObjectVersion)

-- | The class of storage used to store the object.
objectVersion_storageClass :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectVersionStorageClass)
objectVersion_storageClass = Lens.lens (\ObjectVersion' {storageClass} -> storageClass) (\s@ObjectVersion' {} a -> s {storageClass = a} :: ObjectVersion)

-- | Version ID of an object.
objectVersion_versionId :: Lens.Lens' ObjectVersion (Prelude.Maybe ObjectVersionId)
objectVersion_versionId = Lens.lens (\ObjectVersion' {versionId} -> versionId) (\s@ObjectVersion' {} a -> s {versionId = a} :: ObjectVersion)

-- | Specifies the owner of the object.
objectVersion_owner :: Lens.Lens' ObjectVersion (Prelude.Maybe Owner)
objectVersion_owner = Lens.lens (\ObjectVersion' {owner} -> owner) (\s@ObjectVersion' {} a -> s {owner = a} :: ObjectVersion)

-- | Date and time the object was last modified.
objectVersion_lastModified :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.UTCTime)
objectVersion_lastModified = Lens.lens (\ObjectVersion' {lastModified} -> lastModified) (\s@ObjectVersion' {} a -> s {lastModified = a} :: ObjectVersion) Prelude.. Lens.mapping Prelude._Time

-- | Size in bytes of the object.
objectVersion_size :: Lens.Lens' ObjectVersion (Prelude.Maybe Prelude.Int)
objectVersion_size = Lens.lens (\ObjectVersion' {size} -> size) (\s@ObjectVersion' {} a -> s {size = a} :: ObjectVersion)

instance Prelude.FromXML ObjectVersion where
  parseXML x =
    ObjectVersion'
      Prelude.<$> (x Prelude..@? "ETag")
      Prelude.<*> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "IsLatest")
      Prelude.<*> (x Prelude..@? "StorageClass")
      Prelude.<*> (x Prelude..@? "VersionId")
      Prelude.<*> (x Prelude..@? "Owner")
      Prelude.<*> (x Prelude..@? "LastModified")
      Prelude.<*> (x Prelude..@? "Size")

instance Prelude.Hashable ObjectVersion

instance Prelude.NFData ObjectVersion
