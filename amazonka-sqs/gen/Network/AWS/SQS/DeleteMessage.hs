{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.DeleteMessage
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified message from the specified queue. You specify the
-- message by using the message\'s @receipt handle@ and not the
-- @message ID@ you received when you sent the message. Even if the message
-- is locked by another reader due to the visibility timeout setting, it is
-- still deleted from the queue. If you leave a message in the queue for
-- longer than the queue\'s configured retention period, Amazon SQS
-- automatically deletes it.
--
-- The receipt handle is associated with a specific instance of receiving
-- the message. If you receive a message more than once, the receipt handle
-- you get each time you receive the message is different. When you request
-- @DeleteMessage@, if you don\'t provide the most recently received
-- receipt handle for the message, the request will still succeed, but the
-- message might not be deleted.
--
-- It is possible you will receive a message even after you have deleted
-- it. This might happen on rare occasions if one of the servers storing a
-- copy of the message is unavailable when you request to delete the
-- message. The copy remains on the server and might be returned to you
-- again on a subsequent receive request. You should create your system to
-- be idempotent so that receiving a particular message more than once is
-- not a problem.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessage.html>
module Network.AWS.SQS.DeleteMessage
    (
    -- * Request
      DeleteMessage
    -- ** Request constructor
    , deleteMessage
    -- ** Request lenses
    , dmQueueURL
    , dmReceiptHandle

    -- * Response
    , DeleteMessageResponse
    -- ** Response constructor
    , deleteMessageResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'deleteMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmQueueURL'
--
-- * 'dmReceiptHandle'
data DeleteMessage = DeleteMessage'{_dmQueueURL :: Text, _dmReceiptHandle :: Text} deriving (Eq, Read, Show)

-- | 'DeleteMessage' smart constructor.
deleteMessage :: Text -> Text -> DeleteMessage
deleteMessage pQueueURL pReceiptHandle = DeleteMessage'{_dmQueueURL = pQueueURL, _dmReceiptHandle = pReceiptHandle};

-- | The URL of the Amazon SQS queue to take action on.
dmQueueURL :: Lens' DeleteMessage Text
dmQueueURL = lens _dmQueueURL (\ s a -> s{_dmQueueURL = a});

-- | The receipt handle associated with the message to delete.
dmReceiptHandle :: Lens' DeleteMessage Text
dmReceiptHandle = lens _dmReceiptHandle (\ s a -> s{_dmReceiptHandle = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteMessage where
        type Sv DeleteMessage = SQS
        type Rs DeleteMessage = DeleteMessageResponse
        request = post
        response = receiveNull DeleteMessageResponse'

instance ToHeaders DeleteMessage where
        toHeaders = const mempty

instance ToPath DeleteMessage where
        toPath = const "/"

instance ToQuery DeleteMessage where
        toQuery DeleteMessage'{..}
          = mconcat
              ["Action" =: ("DeleteMessage" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _dmQueueURL,
               "ReceiptHandle" =: _dmReceiptHandle]

-- | /See:/ 'deleteMessageResponse' smart constructor.
data DeleteMessageResponse = DeleteMessageResponse' deriving (Eq, Read, Show)

-- | 'DeleteMessageResponse' smart constructor.
deleteMessageResponse :: DeleteMessageResponse
deleteMessageResponse = DeleteMessageResponse';
