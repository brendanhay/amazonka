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
-- Module      : Network.AWS.S3.Types.DeleteMarkerEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Owner

-- | Information about the delete marker.
--
-- /See:/ 'newDeleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { -- | The object key.
    key :: Prelude.Maybe ObjectKey,
    -- | Specifies whether the object is (true) or is not (false) the latest
    -- version of an object.
    isLatest :: Prelude.Maybe Prelude.Bool,
    -- | Version ID of an object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The account that created the delete marker.>
    owner :: Prelude.Maybe Owner,
    -- | Date and time the object was last modified.
    lastModified :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMarkerEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'deleteMarkerEntry_key' - The object key.
--
-- 'isLatest', 'deleteMarkerEntry_isLatest' - Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
--
-- 'versionId', 'deleteMarkerEntry_versionId' - Version ID of an object.
--
-- 'owner', 'deleteMarkerEntry_owner' - The account that created the delete marker.>
--
-- 'lastModified', 'deleteMarkerEntry_lastModified' - Date and time the object was last modified.
newDeleteMarkerEntry ::
  DeleteMarkerEntry
newDeleteMarkerEntry =
  DeleteMarkerEntry'
    { key = Prelude.Nothing,
      isLatest = Prelude.Nothing,
      versionId = Prelude.Nothing,
      owner = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | The object key.
deleteMarkerEntry_key :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe ObjectKey)
deleteMarkerEntry_key = Lens.lens (\DeleteMarkerEntry' {key} -> key) (\s@DeleteMarkerEntry' {} a -> s {key = a} :: DeleteMarkerEntry)

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
deleteMarkerEntry_isLatest :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Prelude.Bool)
deleteMarkerEntry_isLatest = Lens.lens (\DeleteMarkerEntry' {isLatest} -> isLatest) (\s@DeleteMarkerEntry' {} a -> s {isLatest = a} :: DeleteMarkerEntry)

-- | Version ID of an object.
deleteMarkerEntry_versionId :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe ObjectVersionId)
deleteMarkerEntry_versionId = Lens.lens (\DeleteMarkerEntry' {versionId} -> versionId) (\s@DeleteMarkerEntry' {} a -> s {versionId = a} :: DeleteMarkerEntry)

-- | The account that created the delete marker.>
deleteMarkerEntry_owner :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Owner)
deleteMarkerEntry_owner = Lens.lens (\DeleteMarkerEntry' {owner} -> owner) (\s@DeleteMarkerEntry' {} a -> s {owner = a} :: DeleteMarkerEntry)

-- | Date and time the object was last modified.
deleteMarkerEntry_lastModified :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Prelude.UTCTime)
deleteMarkerEntry_lastModified = Lens.lens (\DeleteMarkerEntry' {lastModified} -> lastModified) (\s@DeleteMarkerEntry' {} a -> s {lastModified = a} :: DeleteMarkerEntry) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML DeleteMarkerEntry where
  parseXML x =
    DeleteMarkerEntry'
      Prelude.<$> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "IsLatest")
      Prelude.<*> (x Prelude..@? "VersionId")
      Prelude.<*> (x Prelude..@? "Owner")
      Prelude.<*> (x Prelude..@? "LastModified")

instance Prelude.Hashable DeleteMarkerEntry

instance Prelude.NFData DeleteMarkerEntry
