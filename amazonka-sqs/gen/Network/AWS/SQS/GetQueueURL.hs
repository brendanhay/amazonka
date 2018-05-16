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
-- Module      : Network.AWS.SQS.GetQueueURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an existing queue. This action provides a simple way to retrieve the URL of an Amazon SQS queue.
--
--
-- To access a queue that belongs to another AWS account, use the @QueueOwnerAWSAccountId@ parameter to specify the account ID of the queue's owner. The queue's owner must grant you permission to access the queue. For more information about shared queue access, see @'AddPermission' @ or see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html Shared Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
module Network.AWS.SQS.GetQueueURL
    (
    -- * Creating a Request
      getQueueURL
    , GetQueueURL
    -- * Request Lenses
    , gquQueueOwnerAWSAccountId
    , gquQueueName

    -- * Destructuring the Response
    , getQueueURLResponse
    , GetQueueURLResponse
    -- * Response Lenses
    , gqursResponseStatus
    , gqursQueueURL
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
-- /See:/ 'getQueueURL' smart constructor.
data GetQueueURL = GetQueueURL'
  { _gquQueueOwnerAWSAccountId :: !(Maybe Text)
  , _gquQueueName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueueURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gquQueueOwnerAWSAccountId' - The AWS account ID of the account that created the queue.
--
-- * 'gquQueueName' - The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ). Queue names are case-sensitive.
getQueueURL
    :: Text -- ^ 'gquQueueName'
    -> GetQueueURL
getQueueURL pQueueName_ =
  GetQueueURL'
    {_gquQueueOwnerAWSAccountId = Nothing, _gquQueueName = pQueueName_}


-- | The AWS account ID of the account that created the queue.
gquQueueOwnerAWSAccountId :: Lens' GetQueueURL (Maybe Text)
gquQueueOwnerAWSAccountId = lens _gquQueueOwnerAWSAccountId (\ s a -> s{_gquQueueOwnerAWSAccountId = a})

-- | The name of the queue whose URL must be fetched. Maximum 80 characters. Valid values: alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ). Queue names are case-sensitive.
gquQueueName :: Lens' GetQueueURL Text
gquQueueName = lens _gquQueueName (\ s a -> s{_gquQueueName = a})

instance AWSRequest GetQueueURL where
        type Rs GetQueueURL = GetQueueURLResponse
        request = postQuery sqs
        response
          = receiveXMLWrapper "GetQueueUrlResult"
              (\ s h x ->
                 GetQueueURLResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "QueueUrl"))

instance Hashable GetQueueURL where

instance NFData GetQueueURL where

instance ToHeaders GetQueueURL where
        toHeaders = const mempty

instance ToPath GetQueueURL where
        toPath = const "/"

instance ToQuery GetQueueURL where
        toQuery GetQueueURL'{..}
          = mconcat
              ["Action" =: ("GetQueueUrl" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueOwnerAWSAccountId" =:
                 _gquQueueOwnerAWSAccountId,
               "QueueName" =: _gquQueueName]

-- | For more information, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/UnderstandingResponses.html Responses> in the /Amazon Simple Queue Service Developer Guide/ .
--
--
--
-- /See:/ 'getQueueURLResponse' smart constructor.
data GetQueueURLResponse = GetQueueURLResponse'
  { _gqursResponseStatus :: !Int
  , _gqursQueueURL       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueueURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqursResponseStatus' - -- | The response status code.
--
-- * 'gqursQueueURL' - The URL of the queue.
getQueueURLResponse
    :: Int -- ^ 'gqursResponseStatus'
    -> Text -- ^ 'gqursQueueURL'
    -> GetQueueURLResponse
getQueueURLResponse pResponseStatus_ pQueueURL_ =
  GetQueueURLResponse'
    {_gqursResponseStatus = pResponseStatus_, _gqursQueueURL = pQueueURL_}


-- | -- | The response status code.
gqursResponseStatus :: Lens' GetQueueURLResponse Int
gqursResponseStatus = lens _gqursResponseStatus (\ s a -> s{_gqursResponseStatus = a})

-- | The URL of the queue.
gqursQueueURL :: Lens' GetQueueURLResponse Text
gqursQueueURL = lens _gqursQueueURL (\ s a -> s{_gqursQueueURL = a})

instance NFData GetQueueURLResponse where
