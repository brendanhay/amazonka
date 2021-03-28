{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.S3Location
  ( S3Location (..)
  -- * Smart constructor
  , mkS3Location
  -- * Lenses
  , slBucketName
  , slPrefix
  , slAccessControlList
  , slCannedACL
  , slEncryption
  , slStorageClass
  , slTagging
  , slUserMetadata
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Encryption as Types
import qualified Network.AWS.S3.Types.Grant as Types
import qualified Network.AWS.S3.Types.LocationPrefix as Types
import qualified Network.AWS.S3.Types.MetadataEntry as Types
import qualified Network.AWS.S3.Types.ObjectCannedACL as Types
import qualified Network.AWS.S3.Types.StorageClass as Types
import qualified Network.AWS.S3.Types.Tagging as Types

-- | Describes an Amazon S3 location that will receive the results of the restore request.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bucketName :: Types.BucketName
    -- ^ The name of the bucket where the restore results will be placed.
  , prefix :: Types.LocationPrefix
    -- ^ The prefix that is prepended to the restore results for this request.
  , accessControlList :: Core.Maybe [Types.Grant]
    -- ^ A list of grants that control access to the staged results.
  , cannedACL :: Core.Maybe Types.ObjectCannedACL
    -- ^ The canned ACL to apply to the restore results.
  , encryption :: Core.Maybe Types.Encryption
  , storageClass :: Core.Maybe Types.StorageClass
    -- ^ The class of storage used to store the restore results.
  , tagging :: Core.Maybe Types.Tagging
    -- ^ The tag-set that is applied to the restore results.
  , userMetadata :: Core.Maybe [Types.MetadataEntry]
    -- ^ A list of metadata to store with the restore results in S3.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location
    :: Types.BucketName -- ^ 'bucketName'
    -> Types.LocationPrefix -- ^ 'prefix'
    -> S3Location
mkS3Location bucketName prefix
  = S3Location'{bucketName, prefix, accessControlList = Core.Nothing,
                cannedACL = Core.Nothing, encryption = Core.Nothing,
                storageClass = Core.Nothing, tagging = Core.Nothing,
                userMetadata = Core.Nothing}

-- | The name of the bucket where the restore results will be placed.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location Types.BucketName
slBucketName = Lens.field @"bucketName"
{-# INLINEABLE slBucketName #-}
{-# DEPRECATED bucketName "Use generic-lens or generic-optics with 'bucketName' instead"  #-}

-- | The prefix that is prepended to the restore results for this request.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slPrefix :: Lens.Lens' S3Location Types.LocationPrefix
slPrefix = Lens.field @"prefix"
{-# INLINEABLE slPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | A list of grants that control access to the staged results.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAccessControlList :: Lens.Lens' S3Location (Core.Maybe [Types.Grant])
slAccessControlList = Lens.field @"accessControlList"
{-# INLINEABLE slAccessControlList #-}
{-# DEPRECATED accessControlList "Use generic-lens or generic-optics with 'accessControlList' instead"  #-}

-- | The canned ACL to apply to the restore results.
--
-- /Note:/ Consider using 'cannedACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCannedACL :: Lens.Lens' S3Location (Core.Maybe Types.ObjectCannedACL)
slCannedACL = Lens.field @"cannedACL"
{-# INLINEABLE slCannedACL #-}
{-# DEPRECATED cannedACL "Use generic-lens or generic-optics with 'cannedACL' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slEncryption :: Lens.Lens' S3Location (Core.Maybe Types.Encryption)
slEncryption = Lens.field @"encryption"
{-# INLINEABLE slEncryption #-}
{-# DEPRECATED encryption "Use generic-lens or generic-optics with 'encryption' instead"  #-}

-- | The class of storage used to store the restore results.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slStorageClass :: Lens.Lens' S3Location (Core.Maybe Types.StorageClass)
slStorageClass = Lens.field @"storageClass"
{-# INLINEABLE slStorageClass #-}
{-# DEPRECATED storageClass "Use generic-lens or generic-optics with 'storageClass' instead"  #-}

-- | The tag-set that is applied to the restore results.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slTagging :: Lens.Lens' S3Location (Core.Maybe Types.Tagging)
slTagging = Lens.field @"tagging"
{-# INLINEABLE slTagging #-}
{-# DEPRECATED tagging "Use generic-lens or generic-optics with 'tagging' instead"  #-}

-- | A list of metadata to store with the restore results in S3.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slUserMetadata :: Lens.Lens' S3Location (Core.Maybe [Types.MetadataEntry])
slUserMetadata = Lens.field @"userMetadata"
{-# INLINEABLE slUserMetadata #-}
{-# DEPRECATED userMetadata "Use generic-lens or generic-optics with 'userMetadata' instead"  #-}

instance Core.ToXML S3Location where
        toXML S3Location{..}
          = Core.toXMLElement "BucketName" bucketName Core.<>
              Core.toXMLElement "Prefix" prefix
              Core.<>
              Core.toXMLElement "AccessControlList"
                (Core.maybe Core.mempty (Core.toXMLList "Grant") accessControlList)
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "CannedACL") cannedACL
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Encryption") encryption
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "StorageClass")
                storageClass
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Tagging") tagging
              Core.<>
              Core.toXMLElement "UserMetadata"
                (Core.maybe Core.mempty (Core.toXMLList "MetadataEntry")
                   userMetadata)
