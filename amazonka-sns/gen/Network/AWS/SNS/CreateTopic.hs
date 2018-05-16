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
-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic to which notifications can be published. Users can create at most 100,000 topics. For more information, see <http://aws.amazon.com/sns/ http://aws.amazon.com/sns> . This action is idempotent, so if the requester already owns a topic with the specified name, that topic's ARN is returned without creating a new topic.
--
--
module Network.AWS.SNS.CreateTopic
    (
    -- * Creating a Request
      createTopic
    , CreateTopic
    -- * Request Lenses
    , ctName

    -- * Destructuring the Response
    , createTopicResponse
    , CreateTopicResponse
    -- * Response Lenses
    , ctrsTopicARN
    , ctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for CreateTopic action.
--
--
--
-- /See:/ 'createTopic' smart constructor.
newtype CreateTopic = CreateTopic'
  { _ctName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTopic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctName' - The name of the topic you want to create. Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
createTopic
    :: Text -- ^ 'ctName'
    -> CreateTopic
createTopic pName_ = CreateTopic' {_ctName = pName_}


-- | The name of the topic you want to create. Constraints: Topic names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, and hyphens, and must be between 1 and 256 characters long.
ctName :: Lens' CreateTopic Text
ctName = lens _ctName (\ s a -> s{_ctName = a})

instance AWSRequest CreateTopic where
        type Rs CreateTopic = CreateTopicResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "CreateTopicResult"
              (\ s h x ->
                 CreateTopicResponse' <$>
                   (x .@? "TopicArn") <*> (pure (fromEnum s)))

instance Hashable CreateTopic where

instance NFData CreateTopic where

instance ToHeaders CreateTopic where
        toHeaders = const mempty

instance ToPath CreateTopic where
        toPath = const "/"

instance ToQuery CreateTopic where
        toQuery CreateTopic'{..}
          = mconcat
              ["Action" =: ("CreateTopic" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "Name" =: _ctName]

-- | Response from CreateTopic action.
--
--
--
-- /See:/ 'createTopicResponse' smart constructor.
data CreateTopicResponse = CreateTopicResponse'
  { _ctrsTopicARN       :: !(Maybe Text)
  , _ctrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTopicResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrsTopicARN' - The Amazon Resource Name (ARN) assigned to the created topic.
--
-- * 'ctrsResponseStatus' - -- | The response status code.
createTopicResponse
    :: Int -- ^ 'ctrsResponseStatus'
    -> CreateTopicResponse
createTopicResponse pResponseStatus_ =
  CreateTopicResponse'
    {_ctrsTopicARN = Nothing, _ctrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) assigned to the created topic.
ctrsTopicARN :: Lens' CreateTopicResponse (Maybe Text)
ctrsTopicARN = lens _ctrsTopicARN (\ s a -> s{_ctrsTopicARN = a})

-- | -- | The response status code.
ctrsResponseStatus :: Lens' CreateTopicResponse Int
ctrsResponseStatus = lens _ctrsResponseStatus (\ s a -> s{_ctrsResponseStatus = a})

instance NFData CreateTopicResponse where
