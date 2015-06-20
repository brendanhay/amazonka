{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.GetQueueURL
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

-- | Returns the URL of an existing queue. This action provides a simple way
-- to retrieve the URL of an Amazon SQS queue.
--
-- To access a queue that belongs to another AWS account, use the
-- @QueueOwnerAWSAccountId@ parameter to specify the account ID of the
-- queue\'s owner. The queue\'s owner must grant you permission to access
-- the queue. For more information about shared queue access, see
-- AddPermission or go to
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html Shared Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_GetQueueURL.html>
module Network.AWS.SQS.GetQueueURL
    (
    -- * Request
      GetQueueURL
    -- ** Request constructor
    , getQueueURL
    -- ** Request lenses
    , gquQueueOwnerAWSAccountId
    , gquQueueName

    -- * Response
    , GetQueueURLResponse
    -- ** Response constructor
    , getQueueURLResponse
    -- ** Response lenses
    , gqurQueueURL
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types

-- | /See:/ 'getQueueURL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gquQueueOwnerAWSAccountId'
--
-- * 'gquQueueName'
data GetQueueURL = GetQueueURL'{_gquQueueOwnerAWSAccountId :: Maybe Text, _gquQueueName :: Text} deriving (Eq, Read, Show)

-- | 'GetQueueURL' smart constructor.
getQueueURL :: Text -> GetQueueURL
getQueueURL pQueueName = GetQueueURL'{_gquQueueOwnerAWSAccountId = Nothing, _gquQueueName = pQueueName};

-- | The AWS account ID of the account that created the queue.
gquQueueOwnerAWSAccountId :: Lens' GetQueueURL (Maybe Text)
gquQueueOwnerAWSAccountId = lens _gquQueueOwnerAWSAccountId (\ s a -> s{_gquQueueOwnerAWSAccountId = a});

-- | The name of the queue whose URL must be fetched. Maximum 80 characters;
-- alphanumeric characters, hyphens (-), and underscores (_) are allowed.
gquQueueName :: Lens' GetQueueURL Text
gquQueueName = lens _gquQueueName (\ s a -> s{_gquQueueName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetQueueURL where
        type Sv GetQueueURL = SQS
        type Rs GetQueueURL = GetQueueURLResponse
        request = post
        response
          = receiveXMLWrapper "GetQueueUrlResult"
              (\ s h x ->
                 GetQueueURLResponse' <$> (x .@ "QueueUrl"))

instance ToHeaders GetQueueURL where
        toHeaders = const mempty

instance ToPath GetQueueURL where
        toPath = const "/"

instance ToQuery GetQueueURL where
        toQuery GetQueueURL'{..}
          = mconcat
              ["Action" =: ("GetQueueURL" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueOwnerAWSAccountId" =:
                 _gquQueueOwnerAWSAccountId,
               "QueueName" =: _gquQueueName]

-- | /See:/ 'getQueueURLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqurQueueURL'
newtype GetQueueURLResponse = GetQueueURLResponse'{_gqurQueueURL :: Text} deriving (Eq, Read, Show)

-- | 'GetQueueURLResponse' smart constructor.
getQueueURLResponse :: Text -> GetQueueURLResponse
getQueueURLResponse pQueueURL = GetQueueURLResponse'{_gqurQueueURL = pQueueURL};

-- | The URL for the queue.
gqurQueueURL :: Lens' GetQueueURLResponse Text
gqurQueueURL = lens _gqurQueueURL (\ s a -> s{_gqurQueueURL = a});
