{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectVersion
  ( ObjectVersion (..)
  -- * Smart constructor
  , mkObjectVersion
  -- * Lenses
  , ovETag
  , ovIsLatest
  , ovKey
  , ovLastModified
  , ovOwner
  , ovSize
  , ovStorageClass
  , ovVersionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Key as Types
import qualified Network.AWS.S3.Types.ObjectVersionStorageClass as Types
import qualified Network.AWS.S3.Types.Owner as Types

-- | The version of an object.
--
-- /See:/ 'mkObjectVersion' smart constructor.
data ObjectVersion = ObjectVersion'
  { eTag :: Core.Maybe Types.ETag
    -- ^ The entity tag is an MD5 hash of that version of the object.
  , isLatest :: Core.Maybe Core.Bool
    -- ^ Specifies whether the object is (true) or is not (false) the latest version of an object.
  , key :: Core.Maybe Types.Key
    -- ^ The object key.
  , lastModified :: Core.Maybe Core.UTCTime
    -- ^ Date and time the object was last modified.
  , owner :: Core.Maybe Types.Owner
    -- ^ Specifies the owner of the object.
  , size :: Core.Maybe Core.Int
    -- ^ Size in bytes of the object.
  , storageClass :: Core.Maybe Types.ObjectVersionStorageClass
    -- ^ The class of storage used to store the object.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ Version ID of an object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ObjectVersion' value with any optional fields omitted.
mkObjectVersion
    :: ObjectVersion
mkObjectVersion
  = ObjectVersion'{eTag = Core.Nothing, isLatest = Core.Nothing,
                   key = Core.Nothing, lastModified = Core.Nothing,
                   owner = Core.Nothing, size = Core.Nothing,
                   storageClass = Core.Nothing, versionId = Core.Nothing}

-- | The entity tag is an MD5 hash of that version of the object.
--
-- /Note:/ Consider using 'eTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovETag :: Lens.Lens' ObjectVersion (Core.Maybe Types.ETag)
ovETag = Lens.field @"eTag"
{-# INLINEABLE ovETag #-}
{-# DEPRECATED eTag "Use generic-lens or generic-optics with 'eTag' instead"  #-}

-- | Specifies whether the object is (true) or is not (false) the latest version of an object.
--
-- /Note:/ Consider using 'isLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovIsLatest :: Lens.Lens' ObjectVersion (Core.Maybe Core.Bool)
ovIsLatest = Lens.field @"isLatest"
{-# INLINEABLE ovIsLatest #-}
{-# DEPRECATED isLatest "Use generic-lens or generic-optics with 'isLatest' instead"  #-}

-- | The object key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovKey :: Lens.Lens' ObjectVersion (Core.Maybe Types.Key)
ovKey = Lens.field @"key"
{-# INLINEABLE ovKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Date and time the object was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovLastModified :: Lens.Lens' ObjectVersion (Core.Maybe Core.UTCTime)
ovLastModified = Lens.field @"lastModified"
{-# INLINEABLE ovLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | Specifies the owner of the object.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovOwner :: Lens.Lens' ObjectVersion (Core.Maybe Types.Owner)
ovOwner = Lens.field @"owner"
{-# INLINEABLE ovOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | Size in bytes of the object.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovSize :: Lens.Lens' ObjectVersion (Core.Maybe Core.Int)
ovSize = Lens.field @"size"
{-# INLINEABLE ovSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | The class of storage used to store the object.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovStorageClass :: Lens.Lens' ObjectVersion (Core.Maybe Types.ObjectVersionStorageClass)
ovStorageClass = Lens.field @"storageClass"
{-# INLINEABLE ovStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

-- | Version ID of an object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ovVersionId :: Lens.Lens' ObjectVersion (Core.Maybe Types.ObjectVersionId)
ovVersionId = Lens.field @"versionId"
{-# INLINEABLE ovVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.FromXML ObjectVersion where
        parseXML x
          = ObjectVersion' Core.<$>
              (x Core..@? "ETag") Core.<*> x Core..@? "IsLatest" Core.<*>
                x Core..@? "Key"
                Core.<*> x Core..@? "LastModified"
                Core.<*> x Core..@? "Owner"
                Core.<*> x Core..@? "Size"
                Core.<*> x Core..@? "StorageClass"
                Core.<*> x Core..@? "VersionId"
