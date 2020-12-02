{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.StartDeliveryStreamEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables server-side encryption (SSE) for the delivery stream.
--
--
-- This operation is asynchronous. It returns immediately. When you invoke it, Kinesis Data Firehose first sets the encryption status of the stream to @ENABLING@ , and then to @ENABLED@ . The encryption status of a delivery stream is the @Status@ property in 'DeliveryStreamEncryptionConfiguration' . If the operation fails, the encryption status changes to @ENABLING_FAILED@ . You can continue to read and write data to your delivery stream while the encryption status is @ENABLING@ , but the data is not encrypted. It can take up to 5 seconds after the encryption status changes to @ENABLED@ before all records written to the delivery stream are encrypted. To find out whether a record or a batch of records was encrypted, check the response elements 'PutRecordOutput$Encrypted' and 'PutRecordBatchOutput$Encrypted' , respectively.
--
-- To check the encryption status of a delivery stream, use 'DescribeDeliveryStream' .
--
-- Even if encryption is currently enabled for a delivery stream, you can still invoke this operation on it to change the ARN of the CMK or both its type and ARN. If you invoke this method to change the CMK, and the old CMK is of type @CUSTOMER_MANAGED_CMK@ , Kinesis Data Firehose schedules the grant it had on the old CMK for retirement. If the new CMK is of type @CUSTOMER_MANAGED_CMK@ , Kinesis Data Firehose creates a grant that enables it to use the new CMK to encrypt and decrypt data and to manage the grant.
--
-- If a delivery stream already has encryption enabled and then you invoke this operation to change the ARN of the CMK or both its type and ARN and you get @ENABLING_FAILED@ , this only means that the attempt to change the CMK failed. In this case, encryption remains enabled with the old CMK.
--
-- If the encryption status of your delivery stream is @ENABLING_FAILED@ , you can invoke this operation again with a valid CMK. The CMK must be enabled and the key policy mustn't explicitly deny the permission for Kinesis Data Firehose to invoke KMS encrypt and decrypt operations.
--
-- You can enable SSE for a delivery stream only if it's a delivery stream that uses @DirectPut@ as its source.
--
-- The @StartDeliveryStreamEncryption@ and @StopDeliveryStreamEncryption@ operations have a combined limit of 25 calls per delivery stream per 24 hours. For example, you reach the limit if you call @StartDeliveryStreamEncryption@ 13 times and @StopDeliveryStreamEncryption@ 12 times for the same delivery stream in a 24-hour period.
module Network.AWS.Firehose.StartDeliveryStreamEncryption
  ( -- * Creating a Request
    startDeliveryStreamEncryption,
    StartDeliveryStreamEncryption,

    -- * Request Lenses
    sDeliveryStreamEncryptionConfigurationInput,
    sDeliveryStreamName,

    -- * Destructuring the Response
    startDeliveryStreamEncryptionResponse,
    StartDeliveryStreamEncryptionResponse,

    -- * Response Lenses
    srsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startDeliveryStreamEncryption' smart constructor.
data StartDeliveryStreamEncryption = StartDeliveryStreamEncryption'
  { _sDeliveryStreamEncryptionConfigurationInput ::
      !( Maybe
           DeliveryStreamEncryptionConfigurationInput
       ),
    _sDeliveryStreamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartDeliveryStreamEncryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDeliveryStreamEncryptionConfigurationInput' - Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
--
-- * 'sDeliveryStreamName' - The name of the delivery stream for which you want to enable server-side encryption (SSE).
startDeliveryStreamEncryption ::
  -- | 'sDeliveryStreamName'
  Text ->
  StartDeliveryStreamEncryption
startDeliveryStreamEncryption pDeliveryStreamName_ =
  StartDeliveryStreamEncryption'
    { _sDeliveryStreamEncryptionConfigurationInput =
        Nothing,
      _sDeliveryStreamName = pDeliveryStreamName_
    }

-- | Used to specify the type and Amazon Resource Name (ARN) of the KMS key needed for Server-Side Encryption (SSE).
sDeliveryStreamEncryptionConfigurationInput :: Lens' StartDeliveryStreamEncryption (Maybe DeliveryStreamEncryptionConfigurationInput)
sDeliveryStreamEncryptionConfigurationInput = lens _sDeliveryStreamEncryptionConfigurationInput (\s a -> s {_sDeliveryStreamEncryptionConfigurationInput = a})

-- | The name of the delivery stream for which you want to enable server-side encryption (SSE).
sDeliveryStreamName :: Lens' StartDeliveryStreamEncryption Text
sDeliveryStreamName = lens _sDeliveryStreamName (\s a -> s {_sDeliveryStreamName = a})

instance AWSRequest StartDeliveryStreamEncryption where
  type
    Rs StartDeliveryStreamEncryption =
      StartDeliveryStreamEncryptionResponse
  request = postJSON firehose
  response =
    receiveEmpty
      ( \s h x ->
          StartDeliveryStreamEncryptionResponse' <$> (pure (fromEnum s))
      )

instance Hashable StartDeliveryStreamEncryption

instance NFData StartDeliveryStreamEncryption

instance ToHeaders StartDeliveryStreamEncryption where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Firehose_20150804.StartDeliveryStreamEncryption" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartDeliveryStreamEncryption where
  toJSON StartDeliveryStreamEncryption' {..} =
    object
      ( catMaybes
          [ ("DeliveryStreamEncryptionConfigurationInput" .=)
              <$> _sDeliveryStreamEncryptionConfigurationInput,
            Just ("DeliveryStreamName" .= _sDeliveryStreamName)
          ]
      )

instance ToPath StartDeliveryStreamEncryption where
  toPath = const "/"

instance ToQuery StartDeliveryStreamEncryption where
  toQuery = const mempty

-- | /See:/ 'startDeliveryStreamEncryptionResponse' smart constructor.
newtype StartDeliveryStreamEncryptionResponse = StartDeliveryStreamEncryptionResponse'
  { _srsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartDeliveryStreamEncryptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
startDeliveryStreamEncryptionResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartDeliveryStreamEncryptionResponse
startDeliveryStreamEncryptionResponse pResponseStatus_ =
  StartDeliveryStreamEncryptionResponse'
    { _srsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
srsResponseStatus :: Lens' StartDeliveryStreamEncryptionResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartDeliveryStreamEncryptionResponse
