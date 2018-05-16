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
-- Module      : Network.AWS.SQS.DeleteMessage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified message from the specified queue. You specify the message by using the message's /receipt handle/ and not the /MessageId/ you receive when you send the message. Even if the message is locked by another reader due to the visibility timeout setting, it is still deleted from the queue. If you leave a message in the queue for longer than the queue's configured retention period, Amazon SQS automatically deletes the message.
--
--
module Network.AWS.SQS.DeleteMessage
    (
    -- * Creating a Request
      deleteMessage
    , DeleteMessage
    -- * Request Lenses
    , dmQueueURL
    , dmReceiptHandle

    -- * Destructuring the Response
    , deleteMessageResponse
    , DeleteMessageResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'deleteMessage' smart constructor.
data DeleteMessage = DeleteMessage'
  { _dmQueueURL      :: !Text
  , _dmReceiptHandle :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmQueueURL' - The URL of the Amazon SQS queue from which messages are deleted. Queue URLs are case-sensitive.
--
-- * 'dmReceiptHandle' - The receipt handle associated with the message to delete.
deleteMessage
    :: Text -- ^ 'dmQueueURL'
    -> Text -- ^ 'dmReceiptHandle'
    -> DeleteMessage
deleteMessage pQueueURL_ pReceiptHandle_ =
  DeleteMessage' {_dmQueueURL = pQueueURL_, _dmReceiptHandle = pReceiptHandle_}


-- | The URL of the Amazon SQS queue from which messages are deleted. Queue URLs are case-sensitive.
dmQueueURL :: Lens' DeleteMessage Text
dmQueueURL = lens _dmQueueURL (\ s a -> s{_dmQueueURL = a})

-- | The receipt handle associated with the message to delete.
dmReceiptHandle :: Lens' DeleteMessage Text
dmReceiptHandle = lens _dmReceiptHandle (\ s a -> s{_dmReceiptHandle = a})

instance AWSRequest DeleteMessage where
        type Rs DeleteMessage = DeleteMessageResponse
        request = postQuery sqs
        response = receiveNull DeleteMessageResponse'

instance Hashable DeleteMessage where

instance NFData DeleteMessage where

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
data DeleteMessageResponse =
  DeleteMessageResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteMessageResponse' with the minimum fields required to make a request.
--
deleteMessageResponse
    :: DeleteMessageResponse
deleteMessageResponse = DeleteMessageResponse'


instance NFData DeleteMessageResponse where
