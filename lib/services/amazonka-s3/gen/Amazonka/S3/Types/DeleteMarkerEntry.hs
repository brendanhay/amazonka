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
-- Module      : Amazonka.S3.Types.DeleteMarkerEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.DeleteMarkerEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Owner

-- | Information about the delete marker.
--
-- /See:/ 'newDeleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { -- | Version ID of an object.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | Specifies whether the object is (true) or is not (false) the latest
    -- version of an object.
    isLatest :: Prelude.Maybe Prelude.Bool,
    -- | The account that created the delete marker.>
    owner :: Prelude.Maybe Owner,
    -- | The object key.
    key :: Prelude.Maybe ObjectKey,
    -- | Date and time the object was last modified.
    lastModified :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMarkerEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'deleteMarkerEntry_versionId' - Version ID of an object.
--
-- 'isLatest', 'deleteMarkerEntry_isLatest' - Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
--
-- 'owner', 'deleteMarkerEntry_owner' - The account that created the delete marker.>
--
-- 'key', 'deleteMarkerEntry_key' - The object key.
--
-- 'lastModified', 'deleteMarkerEntry_lastModified' - Date and time the object was last modified.
newDeleteMarkerEntry ::
  DeleteMarkerEntry
newDeleteMarkerEntry =
  DeleteMarkerEntry'
    { versionId = Prelude.Nothing,
      isLatest = Prelude.Nothing,
      owner = Prelude.Nothing,
      key = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | Version ID of an object.
deleteMarkerEntry_versionId :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe ObjectVersionId)
deleteMarkerEntry_versionId = Lens.lens (\DeleteMarkerEntry' {versionId} -> versionId) (\s@DeleteMarkerEntry' {} a -> s {versionId = a} :: DeleteMarkerEntry)

-- | Specifies whether the object is (true) or is not (false) the latest
-- version of an object.
deleteMarkerEntry_isLatest :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Prelude.Bool)
deleteMarkerEntry_isLatest = Lens.lens (\DeleteMarkerEntry' {isLatest} -> isLatest) (\s@DeleteMarkerEntry' {} a -> s {isLatest = a} :: DeleteMarkerEntry)

-- | The account that created the delete marker.>
deleteMarkerEntry_owner :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Owner)
deleteMarkerEntry_owner = Lens.lens (\DeleteMarkerEntry' {owner} -> owner) (\s@DeleteMarkerEntry' {} a -> s {owner = a} :: DeleteMarkerEntry)

-- | The object key.
deleteMarkerEntry_key :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe ObjectKey)
deleteMarkerEntry_key = Lens.lens (\DeleteMarkerEntry' {key} -> key) (\s@DeleteMarkerEntry' {} a -> s {key = a} :: DeleteMarkerEntry)

-- | Date and time the object was last modified.
deleteMarkerEntry_lastModified :: Lens.Lens' DeleteMarkerEntry (Prelude.Maybe Prelude.UTCTime)
deleteMarkerEntry_lastModified = Lens.lens (\DeleteMarkerEntry' {lastModified} -> lastModified) (\s@DeleteMarkerEntry' {} a -> s {lastModified = a} :: DeleteMarkerEntry) Prelude.. Lens.mapping Core._Time

instance Core.FromXML DeleteMarkerEntry where
  parseXML x =
    DeleteMarkerEntry'
      Prelude.<$> (x Core..@? "VersionId")
      Prelude.<*> (x Core..@? "IsLatest")
      Prelude.<*> (x Core..@? "Owner")
      Prelude.<*> (x Core..@? "Key")
      Prelude.<*> (x Core..@? "LastModified")

instance Prelude.Hashable DeleteMarkerEntry where
  hashWithSalt _salt DeleteMarkerEntry' {..} =
    _salt `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` isLatest
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData DeleteMarkerEntry where
  rnf DeleteMarkerEntry' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf isLatest
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf lastModified
