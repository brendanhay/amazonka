{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput where

import Network.AWS.Firehose.Types.KeyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the type and Amazon Resource Name (ARN) of the CMK to use for Server-Side Encryption (SSE).
--
--
--
-- /See:/ 'deliveryStreamEncryptionConfigurationInput' smart constructor.
data DeliveryStreamEncryptionConfigurationInput = DeliveryStreamEncryptionConfigurationInput'
  { _dseciKeyARN ::
      !( Maybe
           Text
       ),
    _dseciKeyType ::
      !KeyType
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeliveryStreamEncryptionConfigurationInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dseciKeyARN' - If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@ , you must specify the Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to @AWS_OWNED_CMK@ , Kinesis Data Firehose uses a service-account CMK.
--
-- * 'dseciKeyType' - Indicates the type of customer master key (CMK) to use for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> . When you invoke 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose invokes the Amazon KMS operation <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant> to create a grant that allows the Kinesis Data Firehose service to use the customer managed CMK to perform encryption and decryption. Kinesis Data Firehose manages that grant.  When you invoke 'StartDeliveryStreamEncryption' to change the CMK for a delivery stream that is encrypted with a customer managed CMK, Kinesis Data Firehose schedules the grant it had on the old CMK for retirement. You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500 delivery streams. If a 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' operation exceeds this limit, Kinesis Data Firehose throws a @LimitExceededException@ .  /Important:/ To encrypt your delivery stream, use symmetric CMKs. Kinesis Data Firehose doesn't support asymmetric CMKs. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs> in the AWS Key Management Service developer guide.
deliveryStreamEncryptionConfigurationInput ::
  -- | 'dseciKeyType'
  KeyType ->
  DeliveryStreamEncryptionConfigurationInput
deliveryStreamEncryptionConfigurationInput pKeyType_ =
  DeliveryStreamEncryptionConfigurationInput'
    { _dseciKeyARN =
        Nothing,
      _dseciKeyType = pKeyType_
    }

-- | If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@ , you must specify the Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to @AWS_OWNED_CMK@ , Kinesis Data Firehose uses a service-account CMK.
dseciKeyARN :: Lens' DeliveryStreamEncryptionConfigurationInput (Maybe Text)
dseciKeyARN = lens _dseciKeyARN (\s a -> s {_dseciKeyARN = a})

-- | Indicates the type of customer master key (CMK) to use for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> . When you invoke 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose invokes the Amazon KMS operation <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant> to create a grant that allows the Kinesis Data Firehose service to use the customer managed CMK to perform encryption and decryption. Kinesis Data Firehose manages that grant.  When you invoke 'StartDeliveryStreamEncryption' to change the CMK for a delivery stream that is encrypted with a customer managed CMK, Kinesis Data Firehose schedules the grant it had on the old CMK for retirement. You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500 delivery streams. If a 'CreateDeliveryStream' or 'StartDeliveryStreamEncryption' operation exceeds this limit, Kinesis Data Firehose throws a @LimitExceededException@ .  /Important:/ To encrypt your delivery stream, use symmetric CMKs. Kinesis Data Firehose doesn't support asymmetric CMKs. For information about symmetric and asymmetric CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs> in the AWS Key Management Service developer guide.
dseciKeyType :: Lens' DeliveryStreamEncryptionConfigurationInput KeyType
dseciKeyType = lens _dseciKeyType (\s a -> s {_dseciKeyType = a})

instance Hashable DeliveryStreamEncryptionConfigurationInput

instance NFData DeliveryStreamEncryptionConfigurationInput

instance ToJSON DeliveryStreamEncryptionConfigurationInput where
  toJSON DeliveryStreamEncryptionConfigurationInput' {..} =
    object
      ( catMaybes
          [ ("KeyARN" .=) <$> _dseciKeyARN,
            Just ("KeyType" .= _dseciKeyType)
          ]
      )
