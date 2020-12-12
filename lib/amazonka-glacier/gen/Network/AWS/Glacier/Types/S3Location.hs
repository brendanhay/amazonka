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

import Network.AWS.Glacier.Types.CannedACL
import Network.AWS.Glacier.Types.Encryption
import Network.AWS.Glacier.Types.Grant
import Network.AWS.Glacier.Types.StorageClass
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the location in Amazon S3 where the select job results are stored.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { cannedACL :: Lude.Maybe CannedACL,
    prefix :: Lude.Maybe Lude.Text,
    bucketName :: Lude.Maybe Lude.Text,
    accessControlList :: Lude.Maybe [Grant],
    userMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    encryption :: Lude.Maybe Encryption,
    storageClass :: Lude.Maybe StorageClass,
    tagging :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 'accessControlList' - A list of grants that control access to the staged results.
-- * 'bucketName' - The name of the Amazon S3 bucket where the job results are stored.
-- * 'cannedACL' - The canned access control list (ACL) to apply to the job results.
-- * 'encryption' - Contains information about the encryption used to store the job results in Amazon S3.
-- * 'prefix' - The prefix that is prepended to the results for this request.
-- * 'storageClass' - The storage class used to store the job results.
-- * 'tagging' - The tag-set that is applied to the job results.
-- * 'userMetadata' - A map of metadata to store with the job results in Amazon S3.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location'
    { cannedACL = Lude.Nothing,
      prefix = Lude.Nothing,
      bucketName = Lude.Nothing,
      accessControlList = Lude.Nothing,
      userMetadata = Lude.Nothing,
      encryption = Lude.Nothing,
      storageClass = Lude.Nothing,
      tagging = Lude.Nothing
    }

-- | The canned access control list (ACL) to apply to the job results.
--
-- /Note:/ Consider using 'cannedACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCannedACL :: Lens.Lens' S3Location (Lude.Maybe CannedACL)
slCannedACL = Lens.lens (cannedACL :: S3Location -> Lude.Maybe CannedACL) (\s a -> s {cannedACL = a} :: S3Location)
{-# DEPRECATED slCannedACL "Use generic-lens or generic-optics with 'cannedACL' instead." #-}

-- | The prefix that is prepended to the results for this request.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slPrefix :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slPrefix = Lens.lens (prefix :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3Location)
{-# DEPRECATED slPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The name of the Amazon S3 bucket where the job results are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucketName = Lens.lens (bucketName :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: S3Location)
{-# DEPRECATED slBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | A list of grants that control access to the staged results.
--
-- /Note:/ Consider using 'accessControlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slAccessControlList :: Lens.Lens' S3Location (Lude.Maybe [Grant])
slAccessControlList = Lens.lens (accessControlList :: S3Location -> Lude.Maybe [Grant]) (\s a -> s {accessControlList = a} :: S3Location)
{-# DEPRECATED slAccessControlList "Use generic-lens or generic-optics with 'accessControlList' instead." #-}

-- | A map of metadata to store with the job results in Amazon S3.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slUserMetadata :: Lens.Lens' S3Location (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
slUserMetadata = Lens.lens (userMetadata :: S3Location -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userMetadata = a} :: S3Location)
{-# DEPRECATED slUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | Contains information about the encryption used to store the job results in Amazon S3.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slEncryption :: Lens.Lens' S3Location (Lude.Maybe Encryption)
slEncryption = Lens.lens (encryption :: S3Location -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: S3Location)
{-# DEPRECATED slEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The storage class used to store the job results.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slStorageClass :: Lens.Lens' S3Location (Lude.Maybe StorageClass)
slStorageClass = Lens.lens (storageClass :: S3Location -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: S3Location)
{-# DEPRECATED slStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | The tag-set that is applied to the job results.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slTagging :: Lens.Lens' S3Location (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
slTagging = Lens.lens (tagging :: S3Location -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tagging = a} :: S3Location)
{-# DEPRECATED slTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

instance Lude.FromJSON S3Location where
  parseJSON =
    Lude.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Lude.<$> (x Lude..:? "CannedACL")
            Lude.<*> (x Lude..:? "Prefix")
            Lude.<*> (x Lude..:? "BucketName")
            Lude.<*> (x Lude..:? "AccessControlList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "UserMetadata" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Encryption")
            Lude.<*> (x Lude..:? "StorageClass")
            Lude.<*> (x Lude..:? "Tagging" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON S3Location where
  toJSON S3Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CannedACL" Lude..=) Lude.<$> cannedACL,
            ("Prefix" Lude..=) Lude.<$> prefix,
            ("BucketName" Lude..=) Lude.<$> bucketName,
            ("AccessControlList" Lude..=) Lude.<$> accessControlList,
            ("UserMetadata" Lude..=) Lude.<$> userMetadata,
            ("Encryption" Lude..=) Lude.<$> encryption,
            ("StorageClass" Lude..=) Lude.<$> storageClass,
            ("Tagging" Lude..=) Lude.<$> tagging
          ]
      )
