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
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned
-- might differ based on the authorization of the user.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_GetTopicAttributes.html AWS API Reference> for GetTopicAttributes.
module Network.AWS.SNS.GetTopicAttributes
    (
    -- * Creating a Request
      getTopicAttributes
    , GetTopicAttributes
    -- * Request Lenses
    , gtaTopicARN

    -- * Destructuring the Response
    , getTopicAttributesResponse
    , GetTopicAttributesResponse
    -- * Response Lenses
    , gtarsAttributes
    , gtarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for GetTopicAttributes action.
--
-- /See:/ 'getTopicAttributes' smart constructor.
newtype GetTopicAttributes = GetTopicAttributes'
    { _gtaTopicARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTopicAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtaTopicARN'
getTopicAttributes
    :: Text -- ^ 'gtaTopicARN'
    -> GetTopicAttributes
getTopicAttributes pTopicARN_ =
    GetTopicAttributes'
    { _gtaTopicARN = pTopicARN_
    }

-- | The ARN of the topic whose properties you want to get.
gtaTopicARN :: Lens' GetTopicAttributes Text
gtaTopicARN = lens _gtaTopicARN (\ s a -> s{_gtaTopicARN = a});

instance AWSRequest GetTopicAttributes where
        type Rs GetTopicAttributes =
             GetTopicAttributesResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper "GetTopicAttributesResult"
              (\ s h x ->
                 GetTopicAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance ToHeaders GetTopicAttributes where
        toHeaders = const mempty

instance ToPath GetTopicAttributes where
        toPath = const "/"

instance ToQuery GetTopicAttributes where
        toQuery GetTopicAttributes'{..}
          = mconcat
              ["Action" =: ("GetTopicAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _gtaTopicARN]

-- | Response for GetTopicAttributes action.
--
-- /See:/ 'getTopicAttributesResponse' smart constructor.
data GetTopicAttributesResponse = GetTopicAttributesResponse'
    { _gtarsAttributes :: !(Maybe (Map Text Text))
    , _gtarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetTopicAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtarsAttributes'
--
-- * 'gtarsStatus'
getTopicAttributesResponse
    :: Int -- ^ 'gtarsStatus'
    -> GetTopicAttributesResponse
getTopicAttributesResponse pStatus_ =
    GetTopicAttributesResponse'
    { _gtarsAttributes = Nothing
    , _gtarsStatus = pStatus_
    }

-- | A map of the topic\'s attributes. Attributes in this map include the
-- following:
--
-- -   'TopicArn' -- the topic\'s ARN
-- -   'Owner' -- the AWS account ID of the topic\'s owner
-- -   'Policy' -- the JSON serialization of the topic\'s access control
--     policy
-- -   'DisplayName' -- the human-readable name used in the \"From\" field
--     for notifications to email and email-json endpoints
-- -   'SubscriptionsPending' -- the number of subscriptions pending
--     confirmation on this topic
-- -   'SubscriptionsConfirmed' -- the number of confirmed subscriptions on
--     this topic
-- -   'SubscriptionsDeleted' -- the number of deleted subscriptions on
--     this topic
-- -   'DeliveryPolicy' -- the JSON serialization of the topic\'s delivery
--     policy
-- -   'EffectiveDeliveryPolicy' -- the JSON serialization of the effective
--     delivery policy that takes into account system defaults
gtarsAttributes :: Lens' GetTopicAttributesResponse (HashMap Text Text)
gtarsAttributes = lens _gtarsAttributes (\ s a -> s{_gtarsAttributes = a}) . _Default . _Map;

-- | The response status code.
gtarsStatus :: Lens' GetTopicAttributesResponse Int
gtarsStatus = lens _gtarsStatus (\ s a -> s{_gtarsStatus = a});
