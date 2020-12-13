{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionByDefault
  ( ServerSideEncryptionByDefault (..),

    -- * Smart constructor
    mkServerSideEncryptionByDefault,

    -- * Lenses
    ssebdSSEAlgorithm,
    ssebdKMSMasterKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Describes the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html PUT Bucket encryption> in the /Amazon Simple Storage Service API Reference/ .
--
-- /See:/ 'mkServerSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'
  { -- | Server-side encryption algorithm to use for the default encryption.
    sSEAlgorithm :: ServerSideEncryption,
    -- | AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ .
    --
    -- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .
    -- __For example:__
    --
    --     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    --     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    --
    -- /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
    kmsMasterKeyId :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerSideEncryptionByDefault' with the minimum fields required to make a request.
--
-- * 'sSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
-- * 'kmsMasterKeyId' - AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ .
--
-- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .
-- __For example:__
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
mkServerSideEncryptionByDefault ::
  -- | 'sSEAlgorithm'
  ServerSideEncryption ->
  ServerSideEncryptionByDefault
mkServerSideEncryptionByDefault pSSEAlgorithm_ =
  ServerSideEncryptionByDefault'
    { sSEAlgorithm = pSSEAlgorithm_,
      kmsMasterKeyId = Lude.Nothing
    }

-- | Server-side encryption algorithm to use for the default encryption.
--
-- /Note:/ Consider using 'sSEAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssebdSSEAlgorithm :: Lens.Lens' ServerSideEncryptionByDefault ServerSideEncryption
ssebdSSEAlgorithm = Lens.lens (sSEAlgorithm :: ServerSideEncryptionByDefault -> ServerSideEncryption) (\s a -> s {sSEAlgorithm = a} :: ServerSideEncryptionByDefault)
{-# DEPRECATED ssebdSSEAlgorithm "Use generic-lens or generic-optics with 'sSEAlgorithm' instead." #-}

-- | AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ .
--
-- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .
-- __For example:__
--
--     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
--     * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@
--
--
-- /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssebdKMSMasterKeyId :: Lens.Lens' ServerSideEncryptionByDefault (Lude.Maybe (Lude.Sensitive Lude.Text))
ssebdKMSMasterKeyId = Lens.lens (kmsMasterKeyId :: ServerSideEncryptionByDefault -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {kmsMasterKeyId = a} :: ServerSideEncryptionByDefault)
{-# DEPRECATED ssebdKMSMasterKeyId "Use generic-lens or generic-optics with 'kmsMasterKeyId' instead." #-}

instance Lude.FromXML ServerSideEncryptionByDefault where
  parseXML x =
    ServerSideEncryptionByDefault'
      Lude.<$> (x Lude..@ "SSEAlgorithm") Lude.<*> (x Lude..@? "KMSMasterKeyID")

instance Lude.ToXML ServerSideEncryptionByDefault where
  toXML ServerSideEncryptionByDefault' {..} =
    Lude.mconcat
      [ "SSEAlgorithm" Lude.@= sSEAlgorithm,
        "KMSMasterKeyID" Lude.@= kmsMasterKeyId
      ]
