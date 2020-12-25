{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerEntry
  ( DeleteMarkerEntry (..),

    -- * Smart constructor
    mkDeleteMarkerEntry,

    -- * Lenses
    dmeIsLatest,
    dmeKey,
    dmeLastModified,
    dmeOwner,
    dmeVersionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Key as Types
import qualified Network.AWS.S3.Types.Owner as Types
import qualified Network.AWS.S3.Types.VersionId as Types

-- | Information about the delete marker.
--
-- /See:/ 'mkDeleteMarkerEntry' smart constructor.
data DeleteMarkerEntry = DeleteMarkerEntry'
  { -- | Specifies whether the object is (true) or is not (false) the latest version of an object.
    isLatest :: Core.Maybe Core.Bool,
    -- | The object key.
    key :: Core.Maybe Types.Key,
    -- | Date and time the object was last modified.
    lastModified :: Core.Maybe Core.UTCTime,
    -- | The account that created the delete marker.>
    owner :: Core.Maybe Types.Owner,
    -- | Version ID of an object.
    versionId :: Core.Maybe Types.VersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeleteMarkerEntry' value with any optional fields omitted.
mkDeleteMarkerEntry ::
  DeleteMarkerEntry
mkDeleteMarkerEntry =
  DeleteMarkerEntry'
    { isLatest = Core.Nothing,
      key = Core.Nothing,
      lastModified = Core.Nothing,
      owner = Core.Nothing,
      versionId = Core.Nothing
    }

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- /Note:/ Consider using 'isLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeIsLatest :: Lens.Lens' DeleteMarkerEntry (Core.Maybe Core.Bool)
dmeIsLatest = Lens.field @"isLatest"
{-# DEPRECATED dmeIsLatest "Use generic-lens or generic-optics with 'isLatest' instead." #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeKey :: Lens.Lens' DeleteMarkerEntry (Core.Maybe Types.Key)
dmeKey = Lens.field @"key"
{-# DEPRECATED dmeKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Date and time the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeLastModified :: Lens.Lens' DeleteMarkerEntry (Core.Maybe Core.UTCTime)
dmeLastModified = Lens.field @"lastModified"
{-# DEPRECATED dmeLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The account that created the delete marker.>
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeOwner :: Lens.Lens' DeleteMarkerEntry (Core.Maybe Types.Owner)
dmeOwner = Lens.field @"owner"
{-# DEPRECATED dmeOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | Version ID of an object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmeVersionId :: Lens.Lens' DeleteMarkerEntry (Core.Maybe Types.VersionId)
dmeVersionId = Lens.field @"versionId"
{-# DEPRECATED dmeVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromXML DeleteMarkerEntry where
  parseXML x =
    DeleteMarkerEntry'
      Core.<$> (x Core..@? "IsLatest")
      Core.<*> (x Core..@? "Key")
      Core.<*> (x Core..@? "LastModified")
      Core.<*> (x Core..@? "Owner")
      Core.<*> (x Core..@? "VersionId")
