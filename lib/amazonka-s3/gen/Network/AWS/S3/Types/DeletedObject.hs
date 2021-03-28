{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeletedObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.DeletedObject
  ( DeletedObject (..)
  -- * Smart constructor
  , mkDeletedObject
  -- * Lenses
  , doDeleteMarker
  , doDeleteMarkerVersionId
  , doKey
  , doVersionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DeleteMarkerVersionId as Types
import qualified Network.AWS.S3.Types.Key as Types

-- | Information about the deleted object.
--
-- /See:/ 'mkDeletedObject' smart constructor.
data DeletedObject = DeletedObject'
  { deleteMarker :: Core.Maybe Core.Bool
    -- ^ Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
  , deleteMarkerVersionId :: Core.Maybe Types.DeleteMarkerVersionId
    -- ^ The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
  , key :: Core.Maybe Types.Key
    -- ^ The name of the deleted object.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ The version ID of the deleted object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletedObject' value with any optional fields omitted.
mkDeletedObject
    :: DeletedObject
mkDeletedObject
  = DeletedObject'{deleteMarker = Core.Nothing,
                   deleteMarkerVersionId = Core.Nothing, key = Core.Nothing,
                   versionId = Core.Nothing}

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker. In a simple DELETE, this header indicates whether (true) or not (false) a delete marker was created.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteMarker :: Lens.Lens' DeletedObject (Core.Maybe Core.Bool)
doDeleteMarker = Lens.field @"deleteMarker"
{-# INLINEABLE doDeleteMarker #-}
{-# DEPRECATED deleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead"  #-}

-- | The version ID of the delete marker created as a result of the DELETE operation. If you delete a specific object version, the value returned by this header is the version ID of the object version deleted.
--
-- /Note:/ Consider using 'deleteMarkerVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDeleteMarkerVersionId :: Lens.Lens' DeletedObject (Core.Maybe Types.DeleteMarkerVersionId)
doDeleteMarkerVersionId = Lens.field @"deleteMarkerVersionId"
{-# INLINEABLE doDeleteMarkerVersionId #-}
{-# DEPRECATED deleteMarkerVersionId "Use generic-lens or generic-optics with 'deleteMarkerVersionId' instead"  #-}

-- | The name of the deleted object.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doKey :: Lens.Lens' DeletedObject (Core.Maybe Types.Key)
doKey = Lens.field @"key"
{-# INLINEABLE doKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The version ID of the deleted object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doVersionId :: Lens.Lens' DeletedObject (Core.Maybe Types.ObjectVersionId)
doVersionId = Lens.field @"versionId"
{-# INLINEABLE doVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromXML DeletedObject where
        parseXML x
          = DeletedObject' Core.<$>
              (x Core..@? "DeleteMarker") Core.<*>
                x Core..@? "DeleteMarkerVersionId"
                Core.<*> x Core..@? "Key"
                Core.<*> x Core..@? "VersionId"
