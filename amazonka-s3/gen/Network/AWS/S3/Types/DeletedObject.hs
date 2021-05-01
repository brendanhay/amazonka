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
-- Module      : Network.AWS.S3.Types.DeletedObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeletedObject where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Information about the deleted object.
--
-- /See:/ 'newDeletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { -- | The name of the deleted object.
    key :: Prelude.Maybe ObjectKey,
    -- | The version ID of the delete marker created as a result of the DELETE
    -- operation. If you delete a specific object version, the value returned
    -- by this header is the version ID of the object version deleted.
    deleteMarkerVersionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the versioned object that was permanently deleted was
    -- (true) or was not (false) a delete marker. In a simple DELETE, this
    -- header indicates whether (true) or not (false) a delete marker was
    -- created.
    deleteMarker :: Prelude.Maybe Prelude.Bool,
    -- | The version ID of the deleted object.
    versionId :: Prelude.Maybe ObjectVersionId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletedObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'deletedObject_key' - The name of the deleted object.
--
-- 'deleteMarkerVersionId', 'deletedObject_deleteMarkerVersionId' - The version ID of the delete marker created as a result of the DELETE
-- operation. If you delete a specific object version, the value returned
-- by this header is the version ID of the object version deleted.
--
-- 'deleteMarker', 'deletedObject_deleteMarker' - Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker. In a simple DELETE, this
-- header indicates whether (true) or not (false) a delete marker was
-- created.
--
-- 'versionId', 'deletedObject_versionId' - The version ID of the deleted object.
newDeletedObject ::
  DeletedObject
newDeletedObject =
  DeletedObject'
    { key = Prelude.Nothing,
      deleteMarkerVersionId = Prelude.Nothing,
      deleteMarker = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The name of the deleted object.
deletedObject_key :: Lens.Lens' DeletedObject (Prelude.Maybe ObjectKey)
deletedObject_key = Lens.lens (\DeletedObject' {key} -> key) (\s@DeletedObject' {} a -> s {key = a} :: DeletedObject)

-- | The version ID of the delete marker created as a result of the DELETE
-- operation. If you delete a specific object version, the value returned
-- by this header is the version ID of the object version deleted.
deletedObject_deleteMarkerVersionId :: Lens.Lens' DeletedObject (Prelude.Maybe Prelude.Text)
deletedObject_deleteMarkerVersionId = Lens.lens (\DeletedObject' {deleteMarkerVersionId} -> deleteMarkerVersionId) (\s@DeletedObject' {} a -> s {deleteMarkerVersionId = a} :: DeletedObject)

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker. In a simple DELETE, this
-- header indicates whether (true) or not (false) a delete marker was
-- created.
deletedObject_deleteMarker :: Lens.Lens' DeletedObject (Prelude.Maybe Prelude.Bool)
deletedObject_deleteMarker = Lens.lens (\DeletedObject' {deleteMarker} -> deleteMarker) (\s@DeletedObject' {} a -> s {deleteMarker = a} :: DeletedObject)

-- | The version ID of the deleted object.
deletedObject_versionId :: Lens.Lens' DeletedObject (Prelude.Maybe ObjectVersionId)
deletedObject_versionId = Lens.lens (\DeletedObject' {versionId} -> versionId) (\s@DeletedObject' {} a -> s {versionId = a} :: DeletedObject)

instance Prelude.FromXML DeletedObject where
  parseXML x =
    DeletedObject'
      Prelude.<$> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "DeleteMarkerVersionId")
      Prelude.<*> (x Prelude..@? "DeleteMarker")
      Prelude.<*> (x Prelude..@? "VersionId")

instance Prelude.Hashable DeletedObject

instance Prelude.NFData DeletedObject
