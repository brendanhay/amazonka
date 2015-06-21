{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.GetTopicAttributes
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

-- | Returns all of the properties of a topic. Topic properties returned
-- might differ based on the authorization of the user.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetTopicAttributes.html>
module Network.AWS.SNS.GetTopicAttributes
    (
    -- * Request
      GetTopicAttributes
    -- ** Request constructor
    , getTopicAttributes
    -- ** Request lenses
    , gtaTopicARN

    -- * Response
    , GetTopicAttributesResponse
    -- ** Response constructor
    , getTopicAttributesResponse
    -- ** Response lenses
    , gtarAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | /See:/ 'getTopicAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtaTopicARN'
newtype GetTopicAttributes = GetTopicAttributes'{_gtaTopicARN :: Text} deriving (Eq, Read, Show)

-- | 'GetTopicAttributes' smart constructor.
getTopicAttributes :: Text -> GetTopicAttributes
getTopicAttributes pTopicARN = GetTopicAttributes'{_gtaTopicARN = pTopicARN};

-- | The ARN of the topic whose properties you want to get.
gtaTopicARN :: Lens' GetTopicAttributes Text
gtaTopicARN = lens _gtaTopicARN (\ s a -> s{_gtaTopicARN = a});

instance AWSRequest GetTopicAttributes where
        type Sv GetTopicAttributes = SNS
        type Rs GetTopicAttributes =
             GetTopicAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetTopicAttributesResult"
              (\ s h x ->
                 GetTopicAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value")))

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

-- | /See:/ 'getTopicAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtarAttributes'
newtype GetTopicAttributesResponse = GetTopicAttributesResponse'{_gtarAttributes :: Maybe (Map Text Text)} deriving (Eq, Read, Show)

-- | 'GetTopicAttributesResponse' smart constructor.
getTopicAttributesResponse :: GetTopicAttributesResponse
getTopicAttributesResponse = GetTopicAttributesResponse'{_gtarAttributes = Nothing};

-- | A map of the topic\'s attributes. Attributes in this map include the
-- following:
--
-- -   @TopicArn@ -- the topic\'s ARN
-- -   @Owner@ -- the AWS account ID of the topic\'s owner
-- -   @Policy@ -- the JSON serialization of the topic\'s access control
--     policy
-- -   @DisplayName@ -- the human-readable name used in the \"From\" field
--     for notifications to email and email-json endpoints
-- -   @SubscriptionsPending@ -- the number of subscriptions pending
--     confirmation on this topic
-- -   @SubscriptionsConfirmed@ -- the number of confirmed subscriptions on
--     this topic
-- -   @SubscriptionsDeleted@ -- the number of deleted subscriptions on
--     this topic
-- -   @DeliveryPolicy@ -- the JSON serialization of the topic\'s delivery
--     policy
-- -   @EffectiveDeliveryPolicy@ -- the JSON serialization of the effective
--     delivery policy that takes into account system defaults
gtarAttributes :: Lens' GetTopicAttributesResponse (HashMap Text Text)
gtarAttributes = lens _gtarAttributes (\ s a -> s{_gtarAttributes = a}) . _Default . _Map;
