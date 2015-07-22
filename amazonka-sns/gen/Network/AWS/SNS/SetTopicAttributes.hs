{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Allows a topic owner to set an attribute of the topic to a new value.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetTopicAttributes.html>
module Network.AWS.SNS.SetTopicAttributes
    (
    -- * Request
      SetTopicAttributes
    -- ** Request constructor
    , setTopicAttributes
    -- ** Request lenses
    , starqAttributeValue
    , starqTopicARN
    , starqAttributeName

    -- * Response
    , SetTopicAttributesResponse
    -- ** Response constructor
    , setTopicAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for SetTopicAttributes action.
--
-- /See:/ 'setTopicAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'starqAttributeValue'
--
-- * 'starqTopicARN'
--
-- * 'starqAttributeName'
data SetTopicAttributes = SetTopicAttributes'
    { _starqAttributeValue :: !(Maybe Text)
    , _starqTopicARN       :: !Text
    , _starqAttributeName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTopicAttributes' smart constructor.
setTopicAttributes :: Text -> Text -> SetTopicAttributes
setTopicAttributes pTopicARN_ pAttributeName_ =
    SetTopicAttributes'
    { _starqAttributeValue = Nothing
    , _starqTopicARN = pTopicARN_
    , _starqAttributeName = pAttributeName_
    }

-- | The new value for the attribute.
starqAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
starqAttributeValue = lens _starqAttributeValue (\ s a -> s{_starqAttributeValue = a});

-- | The ARN of the topic to modify.
starqTopicARN :: Lens' SetTopicAttributes Text
starqTopicARN = lens _starqTopicARN (\ s a -> s{_starqTopicARN = a});

-- | The name of the attribute you want to set. Only a subset of the topic\'s
-- attributes are mutable.
--
-- Valid values: @Policy@ | @DisplayName@ | @DeliveryPolicy@
starqAttributeName :: Lens' SetTopicAttributes Text
starqAttributeName = lens _starqAttributeName (\ s a -> s{_starqAttributeName = a});

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
               "AttributeValue" =: _starqAttributeValue,
               "TopicArn" =: _starqTopicARN,
               "AttributeName" =: _starqAttributeName]

-- | /See:/ 'setTopicAttributesResponse' smart constructor.
data SetTopicAttributesResponse =
    SetTopicAttributesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetTopicAttributesResponse' smart constructor.
setTopicAttributesResponse :: SetTopicAttributesResponse
setTopicAttributesResponse = SetTopicAttributesResponse'
