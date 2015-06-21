{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.SetTopicAttributes
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

-- | Allows a topic owner to set an attribute of the topic to a new value.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetTopicAttributes.html>
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Request
      SetTopicAttributes
    -- ** Request constructor
    , setTopicAttributes
    -- ** Request lenses
    , staAttributeValue
    , staTopicARN
    , staAttributeName

    -- * Response
    , SetTopicAttributesResponse
    -- ** Response constructor
    , setTopicAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | /See:/ 'setTopicAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'staAttributeValue'
--
-- * 'staTopicARN'
--
-- * 'staAttributeName'
data SetTopicAttributes = SetTopicAttributes'{_staAttributeValue :: Maybe Text, _staTopicARN :: Text, _staAttributeName :: Text} deriving (Eq, Read, Show)

-- | 'SetTopicAttributes' smart constructor.
setTopicAttributes :: Text -> Text -> SetTopicAttributes
setTopicAttributes pTopicARN pAttributeName = SetTopicAttributes'{_staAttributeValue = Nothing, _staTopicARN = pTopicARN, _staAttributeName = pAttributeName};

-- | The new value for the attribute.
staAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
staAttributeValue = lens _staAttributeValue (\ s a -> s{_staAttributeValue = a});

-- | The ARN of the topic to modify.
staTopicARN :: Lens' SetTopicAttributes Text
staTopicARN = lens _staTopicARN (\ s a -> s{_staTopicARN = a});

-- | The name of the attribute you want to set. Only a subset of the topic\'s
-- attributes are mutable.
--
-- Valid values: @Policy@ | @DisplayName@ | @DeliveryPolicy@
staAttributeName :: Lens' SetTopicAttributes Text
staAttributeName = lens _staAttributeName (\ s a -> s{_staAttributeName = a});

instance AWSRequest SetTopicAttributes where
        type Sv SetTopicAttributes = SNS
        type Rs SetTopicAttributes =
             SetTopicAttributesResponse
        request = post
        response = receiveNull SetTopicAttributesResponse'

instance ToHeaders SetTopicAttributes where
        toHeaders = const mempty

instance ToPath SetTopicAttributes where
        toPath = const "/"

instance ToQuery SetTopicAttributes where
        toQuery SetTopicAttributes'{..}
          = mconcat
              ["Action" =: ("SetTopicAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "AttributeValue" =: _staAttributeValue,
               "TopicArn" =: _staTopicARN,
               "AttributeName" =: _staAttributeName]

-- | /See:/ 'setTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse = SetTopicAttributesResponse' deriving (Eq, Read, Show)

-- | 'SetTopicAttributesResponse' smart constructor.
setTopicAttributesResponse :: SetTopicAttributesResponse
setTopicAttributesResponse = SetTopicAttributesResponse';
