{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionByDefault
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionByDefault where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Describes the default server-side encryption to apply to new objects in
-- the bucket. If a PUT Object request doesn\'t specify any server-side
-- encryption, this default encryption will be applied. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html PUT Bucket encryption>
-- in the /Amazon Simple Storage Service API Reference/.
--
-- /See:/ 'newServerSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'
  { -- | AWS Key Management Service (KMS) customer master key ID to use for the
    -- default encryption. This parameter is allowed if and only if
    -- @SSEAlgorithm@ is set to @aws:kms@.
    --
    -- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK.
    -- However, if you are using encryption with cross-account operations, you
    -- must use a fully qualified CMK ARN. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations>.
    --
    -- __For example:__
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>
    -- in the /AWS Key Management Service Developer Guide/.
    kmsMasterKeyID :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Server-side encryption algorithm to use for the default encryption.
    sSEAlgorithm :: ServerSideEncryption
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionByDefault' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsMasterKeyID', 'serverSideEncryptionByDefault_kmsMasterKeyID' - AWS Key Management Service (KMS) customer master key ID to use for the
-- default encryption. This parameter is allowed if and only if
-- @SSEAlgorithm@ is set to @aws:kms@.
--
-- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- However, if you are using encryption with cross-account operations, you
-- must use a fully qualified CMK ARN. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations>.
--
-- __For example:__
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'sSEAlgorithm', 'serverSideEncryptionByDefault_sSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
newServerSideEncryptionByDefault ::
  -- | 'sSEAlgorithm'
  ServerSideEncryption ->
  ServerSideEncryptionByDefault
newServerSideEncryptionByDefault pSSEAlgorithm_ =
  ServerSideEncryptionByDefault'
    { kmsMasterKeyID =
        Prelude.Nothing,
      sSEAlgorithm = pSSEAlgorithm_
    }

-- | AWS Key Management Service (KMS) customer master key ID to use for the
-- default encryption. This parameter is allowed if and only if
-- @SSEAlgorithm@ is set to @aws:kms@.
--
-- You can specify the key ID or the Amazon Resource Name (ARN) of the CMK.
-- However, if you are using encryption with cross-account operations, you
-- must use a fully qualified CMK ARN. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations>.
--
-- __For example:__
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-east-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>
-- in the /AWS Key Management Service Developer Guide/.
serverSideEncryptionByDefault_kmsMasterKeyID :: Lens.Lens' ServerSideEncryptionByDefault (Prelude.Maybe Prelude.Text)
serverSideEncryptionByDefault_kmsMasterKeyID = Lens.lens (\ServerSideEncryptionByDefault' {kmsMasterKeyID} -> kmsMasterKeyID) (\s@ServerSideEncryptionByDefault' {} a -> s {kmsMasterKeyID = a} :: ServerSideEncryptionByDefault) Prelude.. Lens.mapping Core._Sensitive

-- | Server-side encryption algorithm to use for the default encryption.
serverSideEncryptionByDefault_sSEAlgorithm :: Lens.Lens' ServerSideEncryptionByDefault ServerSideEncryption
serverSideEncryptionByDefault_sSEAlgorithm = Lens.lens (\ServerSideEncryptionByDefault' {sSEAlgorithm} -> sSEAlgorithm) (\s@ServerSideEncryptionByDefault' {} a -> s {sSEAlgorithm = a} :: ServerSideEncryptionByDefault)

instance Core.FromXML ServerSideEncryptionByDefault where
  parseXML x =
    ServerSideEncryptionByDefault'
      Prelude.<$> (x Core..@? "KMSMasterKeyID")
      Prelude.<*> (x Core..@ "SSEAlgorithm")

instance
  Prelude.Hashable
    ServerSideEncryptionByDefault

instance Prelude.NFData ServerSideEncryptionByDefault

instance Core.ToXML ServerSideEncryptionByDefault where
  toXML ServerSideEncryptionByDefault' {..} =
    Prelude.mconcat
      [ "KMSMasterKeyID" Core.@= kmsMasterKeyID,
        "SSEAlgorithm" Core.@= sSEAlgorithm
      ]
