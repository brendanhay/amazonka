{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration where

import Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.KeyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the server-side encryption (SSE) status for the delivery stream, the type customer master key (CMK) in use, if any, and the ARN of the CMK. You can get @DeliveryStreamEncryptionConfiguration@ by invoking the 'DescribeDeliveryStream' operation.
--
--
--
-- /See:/ 'deliveryStreamEncryptionConfiguration' smart constructor.
data DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { _dsecStatus ::
      !( Maybe
           DeliveryStreamEncryptionStatus
       ),
    _dsecKeyType ::
      !( Maybe
           KeyType
       ),
    _dsecKeyARN ::
      !(Maybe Text),
    _dsecFailureDescription ::
      !( Maybe
           FailureDescription
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeliveryStreamEncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsecStatus' - This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
--
-- * 'dsecKeyType' - Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
--
-- * 'dsecKeyARN' - If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
--
-- * 'dsecFailureDescription' - Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
deliveryStreamEncryptionConfiguration ::
  DeliveryStreamEncryptionConfiguration
deliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration'
    { _dsecStatus = Nothing,
      _dsecKeyType = Nothing,
      _dsecKeyARN = Nothing,
      _dsecFailureDescription = Nothing
    }

-- | This is the server-side encryption (SSE) status for the delivery stream. For a full description of the different values of this status, see 'StartDeliveryStreamEncryption' and 'StopDeliveryStreamEncryption' . If this status is @ENABLING_FAILED@ or @DISABLING_FAILED@ , it is the status of the most recent attempt to enable or disable SSE, respectively.
dsecStatus :: Lens' DeliveryStreamEncryptionConfiguration (Maybe DeliveryStreamEncryptionStatus)
dsecStatus = lens _dsecStatus (\s a -> s {_dsecStatus = a})

-- | Indicates the type of customer master key (CMK) that is used for encryption. The default setting is @AWS_OWNED_CMK@ . For more information about CMKs, see <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)> .
dsecKeyType :: Lens' DeliveryStreamEncryptionConfiguration (Maybe KeyType)
dsecKeyType = lens _dsecKeyType (\s a -> s {_dsecKeyType = a})

-- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@ , this field contains the ARN of the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@ , @DeliveryStreamEncryptionConfiguration@ doesn't contain a value for @KeyARN@ .
dsecKeyARN :: Lens' DeliveryStreamEncryptionConfiguration (Maybe Text)
dsecKeyARN = lens _dsecKeyARN (\s a -> s {_dsecKeyARN = a})

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
dsecFailureDescription :: Lens' DeliveryStreamEncryptionConfiguration (Maybe FailureDescription)
dsecFailureDescription = lens _dsecFailureDescription (\s a -> s {_dsecFailureDescription = a})

instance FromJSON DeliveryStreamEncryptionConfiguration where
  parseJSON =
    withObject
      "DeliveryStreamEncryptionConfiguration"
      ( \x ->
          DeliveryStreamEncryptionConfiguration'
            <$> (x .:? "Status")
            <*> (x .:? "KeyType")
            <*> (x .:? "KeyARN")
            <*> (x .:? "FailureDescription")
      )

instance Hashable DeliveryStreamEncryptionConfiguration

instance NFData DeliveryStreamEncryptionConfiguration
