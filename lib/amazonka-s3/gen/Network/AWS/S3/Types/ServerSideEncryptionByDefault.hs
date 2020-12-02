{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionByDefault where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryption

-- | Describes the default server-side encryption to apply to new objects in the bucket. If a PUT Object request doesn't specify any server-side encryption, this default encryption will be applied. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTencryption.html PUT Bucket encryption> in the /Amazon Simple Storage Service API Reference/ .
--
--
--
-- /See:/ 'serverSideEncryptionByDefault' smart constructor.
data ServerSideEncryptionByDefault = ServerSideEncryptionByDefault'
  { _ssebdKMSMasterKeyId ::
      !(Maybe (Sensitive Text)),
    _ssebdSSEAlgorithm ::
      !ServerSideEncryption
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerSideEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssebdKMSMasterKeyId' - AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ . You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .  __For example:__      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'ssebdSSEAlgorithm' - Server-side encryption algorithm to use for the default encryption.
serverSideEncryptionByDefault ::
  -- | 'ssebdSSEAlgorithm'
  ServerSideEncryption ->
  ServerSideEncryptionByDefault
serverSideEncryptionByDefault pSSEAlgorithm_ =
  ServerSideEncryptionByDefault'
    { _ssebdKMSMasterKeyId = Nothing,
      _ssebdSSEAlgorithm = pSSEAlgorithm_
    }

-- | AWS Key Management Service (KMS) customer master key ID to use for the default encryption. This parameter is allowed if and only if @SSEAlgorithm@ is set to @aws:kms@ . You can specify the key ID or the Amazon Resource Name (ARN) of the CMK. However, if you are using encryption with cross-account operations, you must use a fully qualified CMK ARN. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html#bucket-encryption-update-bucket-policy Using encryption for cross-account operations> .  __For example:__      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  /Important:/ Amazon S3 only supports symmetric CMKs and not asymmetric CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
ssebdKMSMasterKeyId :: Lens' ServerSideEncryptionByDefault (Maybe Text)
ssebdKMSMasterKeyId = lens _ssebdKMSMasterKeyId (\s a -> s {_ssebdKMSMasterKeyId = a}) . mapping _Sensitive

-- | Server-side encryption algorithm to use for the default encryption.
ssebdSSEAlgorithm :: Lens' ServerSideEncryptionByDefault ServerSideEncryption
ssebdSSEAlgorithm = lens _ssebdSSEAlgorithm (\s a -> s {_ssebdSSEAlgorithm = a})

instance FromXML ServerSideEncryptionByDefault where
  parseXML x =
    ServerSideEncryptionByDefault'
      <$> (x .@? "KMSMasterKeyID") <*> (x .@ "SSEAlgorithm")

instance Hashable ServerSideEncryptionByDefault

instance NFData ServerSideEncryptionByDefault

instance ToXML ServerSideEncryptionByDefault where
  toXML ServerSideEncryptionByDefault' {..} =
    mconcat
      [ "KMSMasterKeyID" @= _ssebdKMSMasterKeyId,
        "SSEAlgorithm" @= _ssebdSSEAlgorithm
      ]
