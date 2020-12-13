{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeletedObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeletedObject
  ( DeletedObject (..),

    -- * Smart constructor
    mkDeletedObject,

    -- * Lenses
    doVersionId,
    doDeleteMarker,
    doDeleteMarkerVersionId,
    doKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Information about the deleted object.
--
-- /See:/ 'mkDeletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { -- | The version ID of the deleted object.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
    deleteMarker :: Lude.Maybe Lude.Bool,
    -- | The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
    deleteMarkerVersionId :: Lude.Maybe Lude.Text,
    -- | The name of the deleted object.
    key :: Lude.Maybe ObjectKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletedObject' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the deleted object.
-- * 'deleteMarker' - Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
-- * 'deleteMarkerVersionId' - The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
-- * 'key' - The name of the deleted object.
mkDeletedObject ::
  DeletedObject
mkDeletedObject =
  DeletedObject'
    { versionId = Lude.Nothing,
      deleteMarker = Lude.Nothing,
      deleteMarkerVersionId = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The version ID of the deleted object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doVersionId :: Lens.Lens' DeletedObject (Lude.Maybe ObjectVersionId)
doVersionId = Lens.lens (versionId :: DeletedObject -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: DeletedObject)
{-# DEPRECATED doVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteMarker :: Lens.Lens' DeletedObject (Lude.Maybe Lude.Bool)
doDeleteMarker = Lens.lens (deleteMarker :: DeletedObject -> Lude.Maybe Lude.Bool) (\s a -> s {deleteMarker = a} :: DeletedObject)
{-# DEPRECATED doDeleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead." #-}

-- | The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
--
-- /Note:/ Consider using 'deleteMarkerVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteMarkerVersionId :: Lens.Lens' DeletedObject (Lude.Maybe Lude.Text)
doDeleteMarkerVersionId = Lens.lens (deleteMarkerVersionId :: DeletedObject -> Lude.Maybe Lude.Text) (\s a -> s {deleteMarkerVersionId = a} :: DeletedObject)
{-# DEPRECATED doDeleteMarkerVersionId "Use generic-lens or generic-optics with 'deleteMarkerVersionId' instead." #-}

-- | The name of the deleted object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doKey :: Lens.Lens' DeletedObject (Lude.Maybe ObjectKey)
doKey = Lens.lens (key :: DeletedObject -> Lude.Maybe ObjectKey) (\s a -> s {key = a} :: DeletedObject)
{-# DEPRECATED doKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromXML DeletedObject where
  parseXML x =
    DeletedObject'
      Lude.<$> (x Lude..@? "VersionId")
      Lude.<*> (x Lude..@? "DeleteMarker")
      Lude.<*> (x Lude..@? "DeleteMarkerVersionId")
      Lude.<*> (x Lude..@? "Key")
