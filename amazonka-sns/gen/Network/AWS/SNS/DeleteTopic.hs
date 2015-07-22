{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might
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
    , dtrqTopicARN

    -- * Response
    , DeleteTopicResponse
    -- ** Response constructor
    , deleteTopicResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | /See:/ 'deleteTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrqTopicARN'
newtype DeleteTopic = DeleteTopic'
    { _dtrqTopicARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTopic' smart constructor.
deleteTopic :: Text -> DeleteTopic
deleteTopic pTopicARN_ =
    DeleteTopic'
    { _dtrqTopicARN = pTopicARN_
    }

-- | The ARN of the topic you want to delete.
dtrqTopicARN :: Lens' DeleteTopic Text
dtrqTopicARN = lens _dtrqTopicARN (\ s a -> s{_dtrqTopicARN = a});

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
               "TopicArn" =: _dtrqTopicARN]

-- | /See:/ 'deleteTopicResponse' smart constructor.
data DeleteTopicResponse =
    DeleteTopicResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTopicResponse' smart constructor.
deleteTopicResponse :: DeleteTopicResponse
deleteTopicResponse = DeleteTopicResponse'
