{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.DeleteDeliveryStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a delivery stream and its data.
--
--
-- You can delete a delivery stream only if it is in @ACTIVE@ or @DELETING@ state, and not in the @CREATING@ state. While the deletion request is in process, the delivery stream is in the @DELETING@ state.
--
-- To check the state of a delivery stream, use 'DescribeDeliveryStream' .
--
-- While the delivery stream is @DELETING@ state, the service might continue to accept the records, but it doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, you should first stop any applications that are sending records before deleting a delivery stream.
--
module Network.AWS.Firehose.DeleteDeliveryStream
    (
    -- * Creating a Request
      deleteDeliveryStream
    , DeleteDeliveryStream
    -- * Request Lenses
    , dDeliveryStreamName

    -- * Destructuring the Response
    , deleteDeliveryStreamResponse
    , DeleteDeliveryStreamResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDeliveryStream' smart constructor.
newtype DeleteDeliveryStream = DeleteDeliveryStream'
  { _dDeliveryStreamName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeliveryStreamName' - The name of the delivery stream.
deleteDeliveryStream
    :: Text -- ^ 'dDeliveryStreamName'
    -> DeleteDeliveryStream
deleteDeliveryStream pDeliveryStreamName_ =
  DeleteDeliveryStream' {_dDeliveryStreamName = pDeliveryStreamName_}


-- | The name of the delivery stream.
dDeliveryStreamName :: Lens' DeleteDeliveryStream Text
dDeliveryStreamName = lens _dDeliveryStreamName (\ s a -> s{_dDeliveryStreamName = a})

instance AWSRequest DeleteDeliveryStream where
        type Rs DeleteDeliveryStream =
             DeleteDeliveryStreamResponse
        request = postJSON firehose
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteDeliveryStreamResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteDeliveryStream where

instance NFData DeleteDeliveryStream where

instance ToHeaders DeleteDeliveryStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.DeleteDeliveryStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDeliveryStream where
        toJSON DeleteDeliveryStream'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _dDeliveryStreamName)])

instance ToPath DeleteDeliveryStream where
        toPath = const "/"

instance ToQuery DeleteDeliveryStream where
        toQuery = const mempty

-- | /See:/ 'deleteDeliveryStreamResponse' smart constructor.
newtype DeleteDeliveryStreamResponse = DeleteDeliveryStreamResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteDeliveryStreamResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteDeliveryStreamResponse
deleteDeliveryStreamResponse pResponseStatus_ =
  DeleteDeliveryStreamResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteDeliveryStreamResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteDeliveryStreamResponse where
