{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.S3Storage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.S3Storage
  ( S3Storage (..)
  -- * Smart constructor
  , mkS3Storage
  -- * Lenses
  , ssAWSAccessKeyId
  , ssBucket
  , ssPrefix
  , ssUploadPolicy
  , ssUploadPolicySignature
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the storage parameters for S3 and S3 buckets for an instance store-backed AMI.
--
-- /See:/ 'mkS3Storage' smart constructor.
data S3Storage = S3Storage'
  { aWSAccessKeyId :: Core.Maybe Core.Text
    -- ^ The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
  , bucket :: Core.Maybe Core.Text
    -- ^ The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
  , prefix :: Core.Maybe Core.Text
    -- ^ The beginning of the file name of the AMI.
  , uploadPolicy :: Core.Maybe Core.Base64
    -- ^ An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.
  , uploadPolicySignature :: Core.Maybe Core.Text
    -- ^ The signature of the JSON document.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Storage' value with any optional fields omitted.
mkS3Storage
    :: S3Storage
mkS3Storage
  = S3Storage'{aWSAccessKeyId = Core.Nothing, bucket = Core.Nothing,
               prefix = Core.Nothing, uploadPolicy = Core.Nothing,
               uploadPolicySignature = Core.Nothing}

-- | The access key ID of the owner of the bucket. Before you specify a value for your access key ID, review and follow the guidance in <https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html Best Practices for Managing AWS Access Keys> .
--
-- /Note:/ Consider using 'aWSAccessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAWSAccessKeyId :: Lens.Lens' S3Storage (Core.Maybe Core.Text)
ssAWSAccessKeyId = Lens.field @"aWSAccessKeyId"
{-# INLINEABLE ssAWSAccessKeyId #-}
{-# DEPRECATED aWSAccessKeyId "Use generic-lens or generic-optics with 'aWSAccessKeyId' instead"  #-}

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucket :: Lens.Lens' S3Storage (Core.Maybe Core.Text)
ssBucket = Lens.field @"bucket"
{-# INLINEABLE ssBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The beginning of the file name of the AMI.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPrefix :: Lens.Lens' S3Storage (Core.Maybe Core.Text)
ssPrefix = Lens.field @"prefix"
{-# INLINEABLE ssPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | An Amazon S3 upload policy that gives Amazon EC2 permission to upload items into Amazon S3 on your behalf.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'uploadPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUploadPolicy :: Lens.Lens' S3Storage (Core.Maybe Core.Base64)
ssUploadPolicy = Lens.field @"uploadPolicy"
{-# INLINEABLE ssUploadPolicy #-}
{-# DEPRECATED uploadPolicy "Use generic-lens or generic-optics with 'uploadPolicy' instead"  #-}

-- | The signature of the JSON document.
--
-- /Note:/ Consider using 'uploadPolicySignature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUploadPolicySignature :: Lens.Lens' S3Storage (Core.Maybe Core.Text)
ssUploadPolicySignature = Lens.field @"uploadPolicySignature"
{-# INLINEABLE ssUploadPolicySignature #-}
{-# DEPRECATED uploadPolicySignature "Use generic-lens or generic-optics with 'uploadPolicySignature' instead"  #-}

instance Core.ToQuery S3Storage where
        toQuery S3Storage{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AWSAccessKeyId")
              aWSAccessKeyId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Bucket") bucket
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UploadPolicy")
                uploadPolicy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UploadPolicySignature")
                uploadPolicySignature

instance Core.FromXML S3Storage where
        parseXML x
          = S3Storage' Core.<$>
              (x Core..@? "AWSAccessKeyId") Core.<*> x Core..@? "bucket" Core.<*>
                x Core..@? "prefix"
                Core.<*> x Core..@? "uploadPolicy"
                Core.<*> x Core..@? "uploadPolicySignature"
