{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slAccessControlList,
    slBucketName,
    slCannedACL,
    slEncryption,
    slPrefix,
    slStorageClass,
    slTagging,
    slUserMetadata,
  )
where

import qualified Network.AWS.Glacier.Types.CannedACL as Types
import qualified Network.AWS.Glacier.Types.Encryption as Types
import qualified Network.AWS.Glacier.Types.Grant as Types
import qualified Network.AWS.Glacier.Types.StorageClass as Types
import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the location in Amazon S3 where the select job results are stored.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | A list of grants that control access to the staged results.
    accessControlList :: Core.Maybe [Types.Grant],
    -- | The name of the Amazon S3 bucket where the job results are stored.
    bucketName :: Core.Maybe Types.String,
    -- | The canned access control list (ACL) to apply to the job results.
    cannedACL :: Core.Maybe Types.CannedACL,
    -- | Contains information about the encryption used to store the job results in Amazon S3.
    encryption :: Core.Maybe Types.Encryption,
    -- | The prefix that is prepended to the results for this request.
    prefix :: Core.Maybe Types.String,
    -- | The storage class used to store the job results.
    storageClass :: Core.Maybe Types.StorageClass,
    -- | The tag-set that is applied to the job results.
    tagging :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | A map of metadata to store with the job results in Amazon S3.
    userMetadata :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location'
    { accessControlList = Core.Nothing,
      bucketName = Core.Nothing,
      cannedACL = Core.Nothing,
      encryption = Core.Nothing,
      prefix = Core.Nothing,
      storageClass = Core.Nothing,
      tagging = Core.Nothing,
      userMetadata = Core.Nothing
    }

-- | A list of grants that control access to the staged results.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAccessControlList :: Lens.Lens' S3Location (Core.Maybe [Types.Grant])
slAccessControlList = Lens.field @"accessControlList"
{-# DEPRECATED slAccessControlList "Use generic-lens or generic-optics with 'accessControlList' instead." #-}

-- | The name of the Amazon S3 bucket where the job results are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location (Core.Maybe Types.String)
slBucketName = Lens.field @"bucketName"
{-# DEPRECATED slBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The canned access control list (ACL) to apply to the job results.
--
-- /Note:/ Consider using 'cannedACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCannedACL :: Lens.Lens' S3Location (Core.Maybe Types.CannedACL)
slCannedACL = Lens.field @"cannedACL"
{-# DEPRECATED slCannedACL "Use generic-lens or generic-optics with 'cannedACL' instead." #-}

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slEncryption :: Lens.Lens' S3Location (Core.Maybe Types.Encryption)
slEncryption = Lens.field @"encryption"
{-# DEPRECATED slEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The prefix that is prepended to the results for this request.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slPrefix :: Lens.Lens' S3Location (Core.Maybe Types.String)
slPrefix = Lens.field @"prefix"
{-# DEPRECATED slPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The storage class used to store the job results.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slStorageClass :: Lens.Lens' S3Location (Core.Maybe Types.StorageClass)
slStorageClass = Lens.field @"storageClass"
{-# DEPRECATED slStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set that is applied to the job results.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slTagging :: Lens.Lens' S3Location (Core.Maybe (Core.HashMap Types.String Types.String))
slTagging = Lens.field @"tagging"
{-# DEPRECATED slTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

-- | A map of metadata to store with the job results in Amazon S3.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slUserMetadata :: Lens.Lens' S3Location (Core.Maybe (Core.HashMap Types.String Types.String))
slUserMetadata = Lens.field @"userMetadata"
{-# DEPRECATED slUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

instance Core.FromJSON S3Location where
  toJSON S3Location {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessControlList" Core..=) Core.<$> accessControlList,
            ("BucketName" Core..=) Core.<$> bucketName,
            ("CannedACL" Core..=) Core.<$> cannedACL,
            ("Encryption" Core..=) Core.<$> encryption,
            ("Prefix" Core..=) Core.<$> prefix,
            ("StorageClass" Core..=) Core.<$> storageClass,
            ("Tagging" Core..=) Core.<$> tagging,
            ("UserMetadata" Core..=) Core.<$> userMetadata
          ]
      )

instance Core.FromJSON S3Location where
  parseJSON =
    Core.withObject "S3Location" Core.$
      \x ->
        S3Location'
          Core.<$> (x Core..:? "AccessControlList")
          Core.<*> (x Core..:? "BucketName")
          Core.<*> (x Core..:? "CannedACL")
          Core.<*> (x Core..:? "Encryption")
          Core.<*> (x Core..:? "Prefix")
          Core.<*> (x Core..:? "StorageClass")
          Core.<*> (x Core..:? "Tagging")
          Core.<*> (x Core..:? "UserMetadata")
