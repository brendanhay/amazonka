{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.CreateTopic
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

-- | Creates a topic to which notifications can be published. Users can
-- create at most 3000 topics. For more information, see
-- <http://aws.amazon.com/sns/ http:\/\/aws.amazon.com\/sns>. This action
-- is idempotent, so if the requester already owns a topic with the
-- specified name, that topic\'s ARN is returned without creating a new
-- topic.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreateTopic.html>
module Network.AWS.SNS.CreateTopic
    (
    -- * Request
      CreateTopic
    -- ** Request constructor
    , createTopic
    -- ** Request lenses
    , ctName

    -- * Response
    , CreateTopicResponse
    -- ** Response constructor
    , createTopicResponse
    -- ** Response lenses
    , ctrTopicARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SNS.Types

-- | /See:/ 'createTopic' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctName'
newtype CreateTopic = CreateTopic'{_ctName :: Text} deriving (Eq, Read, Show)

-- | 'CreateTopic' smart constructor.
createTopic :: Text -> CreateTopic
createTopic pName = CreateTopic'{_ctName = pName};

-- | The name of the topic you want to create.
--
-- Constraints: Topic names must be made up of only uppercase and lowercase
-- ASCII letters, numbers, underscores, and hyphens, and must be between 1
-- and 256 characters long.
ctName :: Lens' CreateTopic Text
ctName = lens _ctName (\ s a -> s{_ctName = a});

instance AWSRequest CreateTopic where
        type Sv CreateTopic = SNS
        type Rs CreateTopic = CreateTopicResponse
        request = post
        response
          = receiveXMLWrapper "CreateTopicResult"
              (\ s h x ->
                 CreateTopicResponse' <$> (x .@? "TopicArn"))

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

-- | /See:/ 'createTopicResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrTopicARN'
newtype CreateTopicResponse = CreateTopicResponse'{_ctrTopicARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateTopicResponse' smart constructor.
createTopicResponse :: CreateTopicResponse
createTopicResponse = CreateTopicResponse'{_ctrTopicARN = Nothing};

-- | The Amazon Resource Name (ARN) assigned to the created topic.
ctrTopicARN :: Lens' CreateTopicResponse (Maybe Text)
ctrTopicARN = lens _ctrTopicARN (\ s a -> s{_ctrTopicARN = a});
