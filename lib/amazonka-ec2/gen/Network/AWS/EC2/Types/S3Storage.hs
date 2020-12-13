{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.S3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.S3Storage
  ( S3Storage (..),

    -- * Smart constructor
    mkS3Storage,

    -- * Lenses
    ssPrefix,
    ssUploadPolicy,
    ssBucket,
    ssUploadPolicySignature,
    ssAWSAccessKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the storage parameters for S3 and S3 buckets for an instance store-backed AMI.
--
-- /See:/ 'mkS3Storage' smart constructor.
data S3Storage = S3Storage'
  { -- | The beginning of the file name of the AMI.
    prefix :: Lude.Maybe Lude.Text,
    -- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.
    uploadPolicy :: Lude.Maybe Lude.Base64,
    -- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
    bucket :: Lude.Maybe Lude.Text,
    -- | The signature of the JSON document.
    uploadPolicySignature :: Lude.Maybe Lude.Text,
    -- | The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
    awsAccessKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Storage' with the minimum fields required to make a request.
--
-- * 'prefix' - The beginning of the file name of the AMI.
-- * 'uploadPolicy' - An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.
-- * 'bucket' - The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
-- * 'uploadPolicySignature' - The signature of the JSON document.
-- * 'awsAccessKeyId' - The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
mkS3Storage ::
  S3Storage
mkS3Storage =
  S3Storage'
    { prefix = Lude.Nothing,
      uploadPolicy = Lude.Nothing,
      bucket = Lude.Nothing,
      uploadPolicySignature = Lude.Nothing,
      awsAccessKeyId = Lude.Nothing
    }

-- | The beginning of the file name of the AMI.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPrefix :: Lens.Lens' S3Storage (Lude.Maybe Lude.Text)
ssPrefix = Lens.lens (prefix :: S3Storage -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: S3Storage)
{-# DEPRECATED ssPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'uploadPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUploadPolicy :: Lens.Lens' S3Storage (Lude.Maybe Lude.Base64)
ssUploadPolicy = Lens.lens (uploadPolicy :: S3Storage -> Lude.Maybe Lude.Base64) (\s a -> s {uploadPolicy = a} :: S3Storage)
{-# DEPRECATED ssUploadPolicy "Use generic-lens or generic-optics with 'uploadPolicy' instead." #-}

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucket :: Lens.Lens' S3Storage (Lude.Maybe Lude.Text)
ssBucket = Lens.lens (bucket :: S3Storage -> Lude.Maybe Lude.Text) (\s a -> s {bucket = a} :: S3Storage)
{-# DEPRECATED ssBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The signature of the JSON document.
--
-- /Note:/ Consider using 'uploadPolicySignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUploadPolicySignature :: Lens.Lens' S3Storage (Lude.Maybe Lude.Text)
ssUploadPolicySignature = Lens.lens (uploadPolicySignature :: S3Storage -> Lude.Maybe Lude.Text) (\s a -> s {uploadPolicySignature = a} :: S3Storage)
{-# DEPRECATED ssUploadPolicySignature "Use generic-lens or generic-optics with 'uploadPolicySignature' instead." #-}

-- | The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
--
-- /Note:/ Consider using 'awsAccessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAWSAccessKeyId :: Lens.Lens' S3Storage (Lude.Maybe Lude.Text)
ssAWSAccessKeyId = Lens.lens (awsAccessKeyId :: S3Storage -> Lude.Maybe Lude.Text) (\s a -> s {awsAccessKeyId = a} :: S3Storage)
{-# DEPRECATED ssAWSAccessKeyId "Use generic-lens or generic-optics with 'awsAccessKeyId' instead." #-}

instance Lude.FromXML S3Storage where
  parseXML x =
    S3Storage'
      Lude.<$> (x Lude..@? "prefix")
      Lude.<*> (x Lude..@? "uploadPolicy")
      Lude.<*> (x Lude..@? "bucket")
      Lude.<*> (x Lude..@? "uploadPolicySignature")
      Lude.<*> (x Lude..@? "AWSAccessKeyId")

instance Lude.ToQuery S3Storage where
  toQuery S3Storage' {..} =
    Lude.mconcat
      [ "Prefix" Lude.=: prefix,
        "UploadPolicy" Lude.=: uploadPolicy,
        "Bucket" Lude.=: bucket,
        "UploadPolicySignature" Lude.=: uploadPolicySignature,
        "AWSAccessKeyId" Lude.=: awsAccessKeyId
      ]
