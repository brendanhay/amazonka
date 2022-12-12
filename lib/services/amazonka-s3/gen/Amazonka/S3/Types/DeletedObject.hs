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
-- Module      : Amazonka.S3.Types.DeletedObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.DeletedObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Information about the deleted object.
--
-- /See:/ 'newDeletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { -- | Specifies whether the versioned object that was permanently deleted was
    -- (true) or was not (false) a delete marker. In a simple DELETE, this
    -- header indicates whether (true) or not (false) a delete marker was
    -- created.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The version ID of the delete marker created as a result of the DELETE
    -- operation. If you delete a specific object version, the value returned
    -- by this header is the version ID of the object version deleted.
    deleteMarkerVersionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the deleted object.
    key :: Prelude.Maybe ObjectKey,
    -- | The version ID of the deleted object.
    versionId :: Prelude.Maybe ObjectVersionId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletedObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteMarker', 'deletedObject_deleteMarker' - Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker. In a simple DELETE, this
-- header indicates whether (true) or not (false) a delete marker was
-- created.
--
-- 'deleteMarkerVersionId', 'deletedObject_deleteMarkerVersionId' - The version ID of the delete marker created as a result of the DELETE
-- operation. If you delete a specific object version, the value returned
-- by this header is the version ID of the object version deleted.
--
-- 'key', 'deletedObject_key' - The name of the deleted object.
--
-- 'versionId', 'deletedObject_versionId' - The version ID of the deleted object.
newDeletedObject ::
  DeletedObject
newDeletedObject =
  DeletedObject'
    { deleteMarker = Prelude.Nothing,
      deleteMarkerVersionId = Prelude.Nothing,
      key = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker. In a simple DELETE, this
-- header indicates whether (true) or not (false) a delete marker was
-- created.
deletedObject_deleteMarker :: Lens.Lens' DeletedObject (Prelude.Maybe Prelude.Bool)
deletedObject_deleteMarker = Lens.lens (\DeletedObject' {deleteMarker} -> deleteMarker) (\s@DeletedObject' {} a -> s {deleteMarker = a} :: DeletedObject)

-- | The version ID of the delete marker created as a result of the DELETE
-- operation. If you delete a specific object version, the value returned
-- by this header is the version ID of the object version deleted.
deletedObject_deleteMarkerVersionId :: Lens.Lens' DeletedObject (Prelude.Maybe Prelude.Text)
deletedObject_deleteMarkerVersionId = Lens.lens (\DeletedObject' {deleteMarkerVersionId} -> deleteMarkerVersionId) (\s@DeletedObject' {} a -> s {deleteMarkerVersionId = a} :: DeletedObject)

-- | The name of the deleted object.
deletedObject_key :: Lens.Lens' DeletedObject (Prelude.Maybe ObjectKey)
deletedObject_key = Lens.lens (\DeletedObject' {key} -> key) (\s@DeletedObject' {} a -> s {key = a} :: DeletedObject)

-- | The version ID of the deleted object.
deletedObject_versionId :: Lens.Lens' DeletedObject (Prelude.Maybe ObjectVersionId)
deletedObject_versionId = Lens.lens (\DeletedObject' {versionId} -> versionId) (\s@DeletedObject' {} a -> s {versionId = a} :: DeletedObject)

instance Data.FromXML DeletedObject where
  parseXML x =
    DeletedObject'
      Prelude.<$> (x Data..@? "DeleteMarker")
      Prelude.<*> (x Data..@? "DeleteMarkerVersionId")
      Prelude.<*> (x Data..@? "Key")
      Prelude.<*> (x Data..@? "VersionId")

instance Prelude.Hashable DeletedObject where
  hashWithSalt _salt DeletedObject' {..} =
    _salt `Prelude.hashWithSalt` deleteMarker
      `Prelude.hashWithSalt` deleteMarkerVersionId
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData DeletedObject where
  rnf DeletedObject' {..} =
    Prelude.rnf deleteMarker
      `Prelude.seq` Prelude.rnf deleteMarkerVersionId
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf versionId
