-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBucket,
    slKey,
    slObjectVersion,
    slRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location in S3 where build or script files are stored for access by Amazon GameLift. This location is specified in 'CreateBuild' , 'CreateScript' , and 'UpdateScript' requests.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { bucket :: Lude.Maybe Lude.Text,
    key :: Lude.Maybe Lude.Text,
    objectVersion :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
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
-- * 'bucket' - An S3 bucket identifier. This is the name of the S3 bucket.
-- * 'key' - The name of the zip file that contains the build files or script files.
-- * 'objectVersion' - The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved.
-- * 'roleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location'
    { bucket = Lude.Nothing,
      key = Lude.Nothing,
      objectVersion = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | An S3 bucket identifier. This is the name of the S3 bucket.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucket :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucket = Lens.lens (bucket :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Location)
{-# DEPRECATED slBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The name of the zip file that contains the build files or script files.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slKey :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slKey = Lens.lens (key :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: S3Location)
{-# DEPRECATED slKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved.
--
-- /Note:/ Consider using 'objectVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slObjectVersion :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slObjectVersion = Lens.lens (objectVersion :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {objectVersion = a} :: S3Location)
{-# DEPRECATED slObjectVersion "Use generic-lens or generic-optics with 'objectVersion' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slRoleARN :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slRoleARN = Lens.lens (roleARN :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: S3Location)
{-# DEPRECATED slRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON S3Location where
  parseJSON =
    Lude.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Lude.<$> (x Lude..:? "Bucket")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "ObjectVersion")
            Lude.<*> (x Lude..:? "RoleArn")
      )

instance Lude.ToJSON S3Location where
  toJSON S3Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Bucket" Lude..=) Lude.<$> bucket,
            ("Key" Lude..=) Lude.<$> key,
            ("ObjectVersion" Lude..=) Lude.<$> objectVersion,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )
