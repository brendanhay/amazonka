{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slCannedACL,
    slPrefix,
    slBucketName,
    slAccessControlList,
    slUserMetadata,
    slEncryption,
    slStorageClass,
    slTagging,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Encryption
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.MetadataEntry
import Network.AWS.S3.Types.ObjectCannedACL
import Network.AWS.S3.Types.StorageClass
import Network.AWS.S3.Types.Tagging

-- | Describes an Amazon S3 location that will receive the results of the restore request.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The canned ACL to apply to the restore results.
    cannedACL :: Lude.Maybe ObjectCannedACL,
    -- | The prefix that is prepended to the restore results for this request.
    prefix :: Lude.Text,
    -- | The name of the bucket where the restore results will be placed.
    bucketName :: BucketName,
    -- | A list of grants that control access to the staged results.
    accessControlList :: Lude.Maybe [Grant],
    -- | A list of metadata to store with the restore results in S3.
    userMetadata :: Lude.Maybe [MetadataEntry],
    encryption :: Lude.Maybe Encryption,
    -- | The class of storage used to store the restore results.
    storageClass :: Lude.Maybe StorageClass,
    -- | The tag-set that is applied to the restore results.
    tagging :: Lude.Maybe Tagging
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 'cannedACL' - The canned ACL to apply to the restore results.
-- * 'prefix' - The prefix that is prepended to the restore results for this request.
-- * 'bucketName' - The name of the bucket where the restore results will be placed.
-- * 'accessControlList' - A list of grants that control access to the staged results.
-- * 'userMetadata' - A list of metadata to store with the restore results in S3.
-- * 'encryption' -
-- * 'storageClass' - The class of storage used to store the restore results.
-- * 'tagging' - The tag-set that is applied to the restore results.
mkS3Location ::
  -- | 'prefix'
  Lude.Text ->
  -- | 'bucketName'
  BucketName ->
  S3Location
mkS3Location pPrefix_ pBucketName_ =
  S3Location'
    { cannedACL = Lude.Nothing,
      prefix = pPrefix_,
      bucketName = pBucketName_,
      accessControlList = Lude.Nothing,
      userMetadata = Lude.Nothing,
      encryption = Lude.Nothing,
      storageClass = Lude.Nothing,
      tagging = Lude.Nothing
    }

-- | The canned ACL to apply to the restore results.
--
-- /Note:/ Consider using 'cannedACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCannedACL :: Lens.Lens' S3Location (Lude.Maybe ObjectCannedACL)
slCannedACL = Lens.lens (cannedACL :: S3Location -> Lude.Maybe ObjectCannedACL) (\s a -> s {cannedACL = a} :: S3Location)
{-# DEPRECATED slCannedACL "Use generic-lens or generic-optics with 'cannedACL' instead." #-}

-- | The prefix that is prepended to the restore results for this request.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slPrefix :: Lens.Lens' S3Location Lude.Text
slPrefix = Lens.lens (prefix :: S3Location -> Lude.Text) (\s a -> s {prefix = a} :: S3Location)
{-# DEPRECATED slPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The name of the bucket where the restore results will be placed.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location BucketName
slBucketName = Lens.lens (bucketName :: S3Location -> BucketName) (\s a -> s {bucketName = a} :: S3Location)
{-# DEPRECATED slBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | A list of grants that control access to the staged results.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAccessControlList :: Lens.Lens' S3Location (Lude.Maybe [Grant])
slAccessControlList = Lens.lens (accessControlList :: S3Location -> Lude.Maybe [Grant]) (\s a -> s {accessControlList = a} :: S3Location)
{-# DEPRECATED slAccessControlList "Use generic-lens or generic-optics with 'accessControlList' instead." #-}

-- | A list of metadata to store with the restore results in S3.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slUserMetadata :: Lens.Lens' S3Location (Lude.Maybe [MetadataEntry])
slUserMetadata = Lens.lens (userMetadata :: S3Location -> Lude.Maybe [MetadataEntry]) (\s a -> s {userMetadata = a} :: S3Location)
{-# DEPRECATED slUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slEncryption :: Lens.Lens' S3Location (Lude.Maybe Encryption)
slEncryption = Lens.lens (encryption :: S3Location -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: S3Location)
{-# DEPRECATED slEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The class of storage used to store the restore results.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slStorageClass :: Lens.Lens' S3Location (Lude.Maybe StorageClass)
slStorageClass = Lens.lens (storageClass :: S3Location -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: S3Location)
{-# DEPRECATED slStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set that is applied to the restore results.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slTagging :: Lens.Lens' S3Location (Lude.Maybe Tagging)
slTagging = Lens.lens (tagging :: S3Location -> Lude.Maybe Tagging) (\s a -> s {tagging = a} :: S3Location)
{-# DEPRECATED slTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

instance Lude.ToXML S3Location where
  toXML S3Location' {..} =
    Lude.mconcat
      [ "CannedACL" Lude.@= cannedACL,
        "Prefix" Lude.@= prefix,
        "BucketName" Lude.@= bucketName,
        "AccessControlList"
          Lude.@= Lude.toXML (Lude.toXMLList "Grant" Lude.<$> accessControlList),
        "UserMetadata"
          Lude.@= Lude.toXML (Lude.toXMLList "MetadataEntry" Lude.<$> userMetadata),
        "Encryption" Lude.@= encryption,
        "StorageClass" Lude.@= storageClass,
        "Tagging" Lude.@= tagging
      ]
