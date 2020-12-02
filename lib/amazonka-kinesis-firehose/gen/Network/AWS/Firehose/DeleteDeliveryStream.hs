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
-- Module      : Network.AWS.Firehose.DeleteDeliveryStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delivery stream and its data.
--
--
-- To check the state of a delivery stream, use 'DescribeDeliveryStream' . You can delete a delivery stream only if it is in one of the following states: @ACTIVE@ , @DELETING@ , @CREATING_FAILED@ , or @DELETING_FAILED@ . You can't delete a delivery stream that is in the @CREATING@ state. While the deletion request is in process, the delivery stream is in the @DELETING@ state.
--
-- While the delivery stream is in the @DELETING@ state, the service might continue to accept records, but it doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, first stop any applications that are sending records before you delete a delivery stream.
module Network.AWS.Firehose.DeleteDeliveryStream
  ( -- * Creating a Request
    deleteDeliveryStream,
    DeleteDeliveryStream,

    -- * Request Lenses
    dAllowForceDelete,
    dDeliveryStreamName,

    -- * Destructuring the Response
    deleteDeliveryStreamResponse,
    DeleteDeliveryStreamResponse,

    -- * Response Lenses
    drsResponseStatus,
  )
where

import Network.AWS.Firehose.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDeliveryStream' smart constructor.
data DeleteDeliveryStream = DeleteDeliveryStream'
  { _dAllowForceDelete ::
      !(Maybe Bool),
    _dDeliveryStreamName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAllowForceDelete' - Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation. The default value is false.
--
-- * 'dDeliveryStreamName' - The name of the delivery stream.
deleteDeliveryStream ::
  -- | 'dDeliveryStreamName'
  Text ->
  DeleteDeliveryStream
deleteDeliveryStream pDeliveryStreamName_ =
  DeleteDeliveryStream'
    { _dAllowForceDelete = Nothing,
      _dDeliveryStreamName = pDeliveryStreamName_
    }

-- | Set this to true if you want to delete the delivery stream even if Kinesis Data Firehose is unable to retire the grant for the CMK. Kinesis Data Firehose might be unable to retire the grant due to a customer error, such as when the CMK or the grant are in an invalid state. If you force deletion, you can then use the <https://docs.aws.amazon.com/kms/latest/APIReference/API_RevokeGrant.html RevokeGrant> operation to revoke the grant you gave to Kinesis Data Firehose. If a failure to retire the grant happens due to an AWS KMS issue, Kinesis Data Firehose keeps retrying the delete operation. The default value is false.
dAllowForceDelete :: Lens' DeleteDeliveryStream (Maybe Bool)
dAllowForceDelete = lens _dAllowForceDelete (\s a -> s {_dAllowForceDelete = a})

-- | The name of the delivery stream.
dDeliveryStreamName :: Lens' DeleteDeliveryStream Text
dDeliveryStreamName = lens _dDeliveryStreamName (\s a -> s {_dDeliveryStreamName = a})

instance AWSRequest DeleteDeliveryStream where
  type Rs DeleteDeliveryStream = DeleteDeliveryStreamResponse
  request = postJSON firehose
  response =
    receiveEmpty
      (\s h x -> DeleteDeliveryStreamResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteDeliveryStream

instance NFData DeleteDeliveryStream

instance ToHeaders DeleteDeliveryStream where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Firehose_20150804.DeleteDeliveryStream" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDeliveryStream where
  toJSON DeleteDeliveryStream' {..} =
    object
      ( catMaybes
          [ ("AllowForceDelete" .=) <$> _dAllowForceDelete,
            Just ("DeliveryStreamName" .= _dDeliveryStreamName)
          ]
      )

instance ToPath DeleteDeliveryStream where
  toPath = const "/"

instance ToQuery DeleteDeliveryStream where
  toQuery = const mempty

-- | /See:/ 'deleteDeliveryStreamResponse' smart constructor.
newtype DeleteDeliveryStreamResponse = DeleteDeliveryStreamResponse'
  { _drsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteDeliveryStreamResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteDeliveryStreamResponse
deleteDeliveryStreamResponse pResponseStatus_ =
  DeleteDeliveryStreamResponse'
    { _drsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteDeliveryStreamResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeleteDeliveryStreamResponse
