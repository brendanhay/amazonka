{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.EncryptionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies encryption-related information for an Amazon S3 bucket that is a destination for replicated objects.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
newtype EncryptionConfiguration = EncryptionConfiguration'
  { _ecReplicaKMSKeyId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecReplicaKMSKeyId' - Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
encryptionConfiguration ::
  EncryptionConfiguration
encryptionConfiguration =
  EncryptionConfiguration' {_ecReplicaKMSKeyId = Nothing}

-- | Specifies the ID (Key ARN or Alias ARN) of the customer managed customer master key (CMK) stored in AWS Key Management Service (KMS) for the destination bucket. Amazon S3 uses this key to encrypt replica objects. Amazon S3 only supports symmetric customer managed CMKs. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> in the /AWS Key Management Service Developer Guide/ .
ecReplicaKMSKeyId :: Lens' EncryptionConfiguration (Maybe Text)
ecReplicaKMSKeyId = lens _ecReplicaKMSKeyId (\s a -> s {_ecReplicaKMSKeyId = a})

instance FromXML EncryptionConfiguration where
  parseXML x = EncryptionConfiguration' <$> (x .@? "ReplicaKmsKeyID")

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToXML EncryptionConfiguration where
  toXML EncryptionConfiguration' {..} =
    mconcat ["ReplicaKmsKeyID" @= _ecReplicaKMSKeyId]
