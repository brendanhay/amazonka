{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.GetQueueURL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the URL of an existing queue. This action provides a simple way
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
    , gqurqQueueOwnerAWSAccountId
    , gqurqQueueName

    -- * Response
    , GetQueueURLResponse
    -- ** Response constructor
    , getQueueURLResponse
    -- ** Response lenses
    , gqursStatus
    , gqursQueueURL
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'getQueueURL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqurqQueueOwnerAWSAccountId'
--
-- * 'gqurqQueueName'
data GetQueueURL = GetQueueURL'
    { _gqurqQueueOwnerAWSAccountId :: !(Maybe Text)
    , _gqurqQueueName              :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetQueueURL' smart constructor.
getQueueURL :: Text -> GetQueueURL
getQueueURL pQueueName =
    GetQueueURL'
    { _gqurqQueueOwnerAWSAccountId = Nothing
    , _gqurqQueueName = pQueueName
    }

-- | The AWS account ID of the account that created the queue.
gqurqQueueOwnerAWSAccountId :: Lens' GetQueueURL (Maybe Text)
gqurqQueueOwnerAWSAccountId = lens _gqurqQueueOwnerAWSAccountId (\ s a -> s{_gqurqQueueOwnerAWSAccountId = a});

-- | The name of the queue whose URL must be fetched. Maximum 80 characters;
-- alphanumeric characters, hyphens (-), and underscores (_) are allowed.
gqurqQueueName :: Lens' GetQueueURL Text
gqurqQueueName = lens _gqurqQueueName (\ s a -> s{_gqurqQueueName = a});

instance AWSRequest GetQueueURL where
        type Sv GetQueueURL = SQS
        type Rs GetQueueURL = GetQueueURLResponse
        request = post
        response
          = receiveXMLWrapper "GetQueueUrlResult"
              (\ s h x ->
                 GetQueueURLResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "QueueUrl"))

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
                 _gqurqQueueOwnerAWSAccountId,
               "QueueName" =: _gqurqQueueName]

-- | For more information, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/UnderstandingResponses.html Responses>
-- in the /Amazon SQS Developer Guide/.
--
-- /See:/ 'getQueueURLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqursStatus'
--
-- * 'gqursQueueURL'
data GetQueueURLResponse = GetQueueURLResponse'
    { _gqursStatus   :: !Int
    , _gqursQueueURL :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetQueueURLResponse' smart constructor.
getQueueURLResponse :: Int -> Text -> GetQueueURLResponse
getQueueURLResponse pStatus pQueueURL =
    GetQueueURLResponse'
    { _gqursStatus = pStatus
    , _gqursQueueURL = pQueueURL
    }

-- | FIXME: Undocumented member.
gqursStatus :: Lens' GetQueueURLResponse Int
gqursStatus = lens _gqursStatus (\ s a -> s{_gqursStatus = a});

-- | The URL for the queue.
gqursQueueURL :: Lens' GetQueueURLResponse Text
gqursQueueURL = lens _gqursQueueURL (\ s a -> s{_gqursQueueURL = a});
