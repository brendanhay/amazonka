{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.DeleteTopic
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

-- | Deletes a topic and all its subscriptions. Deleting a topic might
-- prevent some messages previously sent to the topic from being delivered
-- to subscribers. This action is idempotent, so deleting a topic that does
-- not exist does not result in an error.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_DeleteTopic.html>
module Network.AWS.SNS.DeleteTopic
    (
    -- * Request
      DeleteTopic
    -- ** Request constructor
    , deleteTopic
    -- ** Request lenses
    , dtTopicARN

    -- * Response
    , DeleteTopicResponse
    -- ** Response constructor
    , deleteTopicResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SNS.Types

-- | /See:/ 'deleteTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTopicARN'
newtype DeleteTopic = DeleteTopic'{_dtTopicARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteTopic' smart constructor.
deleteTopic :: Text -> DeleteTopic
deleteTopic pTopicARN = DeleteTopic'{_dtTopicARN = pTopicARN};

-- | The ARN of the topic you want to delete.
dtTopicARN :: Lens' DeleteTopic Text
dtTopicARN = lens _dtTopicARN (\ s a -> s{_dtTopicARN = a});

instance AWSRequest DeleteTopic where
        type Sv DeleteTopic = SNS
        type Rs DeleteTopic = DeleteTopicResponse
        request = post
        response = receiveNull DeleteTopicResponse'

instance ToHeaders DeleteTopic where
        toHeaders = const mempty

instance ToPath DeleteTopic where
        toPath = const "/"

instance ToQuery DeleteTopic where
        toQuery DeleteTopic'{..}
          = mconcat
              ["Action" =: ("DeleteTopic" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _dtTopicARN]

-- | /See:/ 'deleteTopicResponse' smart constructor.
data DeleteTopicResponse = DeleteTopicResponse' deriving (Eq, Read, Show)

-- | 'DeleteTopicResponse' smart constructor.
deleteTopicResponse :: DeleteTopicResponse
deleteTopicResponse = DeleteTopicResponse';
