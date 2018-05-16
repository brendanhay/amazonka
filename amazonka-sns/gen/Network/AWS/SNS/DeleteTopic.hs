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
-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic and all its subscriptions. Deleting a topic might prevent some messages previously sent to the topic from being delivered to subscribers. This action is idempotent, so deleting a topic that does not exist does not result in an error.
--
--
module Network.AWS.SNS.DeleteTopic
    (
    -- * Creating a Request
      deleteTopic
    , DeleteTopic
    -- * Request Lenses
    , dtTopicARN

    -- * Destructuring the Response
    , deleteTopicResponse
    , DeleteTopicResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | /See:/ 'deleteTopic' smart constructor.
newtype DeleteTopic = DeleteTopic'
  { _dtTopicARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTopicARN' - The ARN of the topic you want to delete.
deleteTopic
    :: Text -- ^ 'dtTopicARN'
    -> DeleteTopic
deleteTopic pTopicARN_ = DeleteTopic' {_dtTopicARN = pTopicARN_}


-- | The ARN of the topic you want to delete.
dtTopicARN :: Lens' DeleteTopic Text
dtTopicARN = lens _dtTopicARN (\ s a -> s{_dtTopicARN = a})

instance AWSRequest DeleteTopic where
        type Rs DeleteTopic = DeleteTopicResponse
        request = postQuery sns
        response = receiveNull DeleteTopicResponse'

instance Hashable DeleteTopic where

instance NFData DeleteTopic where

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
data DeleteTopicResponse =
  DeleteTopicResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTopicResponse' with the minimum fields required to make a request.
--
deleteTopicResponse
    :: DeleteTopicResponse
deleteTopicResponse = DeleteTopicResponse'


instance NFData DeleteTopicResponse where
