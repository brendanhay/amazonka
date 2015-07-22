{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetTopicAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a topic. Topic properties returned
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
    , gtarqTopicARN

    -- * Response
    , GetTopicAttributesResponse
    -- ** Response constructor
    , getTopicAttributesResponse
    -- ** Response lenses
    , gtarsAttributes
    , gtarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for GetTopicAttributes action.
--
-- /See:/ 'getTopicAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtarqTopicARN'
newtype GetTopicAttributes = GetTopicAttributes'
    { _gtarqTopicARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetTopicAttributes' smart constructor.
getTopicAttributes :: Text -> GetTopicAttributes
getTopicAttributes pTopicARN_ =
    GetTopicAttributes'
    { _gtarqTopicARN = pTopicARN_
    }

-- | The ARN of the topic whose properties you want to get.
gtarqTopicARN :: Lens' GetTopicAttributes Text
gtarqTopicARN = lens _gtarqTopicARN (\ s a -> s{_gtarqTopicARN = a});

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
               "TopicArn" =: _gtarqTopicARN]

-- | Response for GetTopicAttributes action.
--
-- /See:/ 'getTopicAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtarsAttributes'
--
-- * 'gtarsStatus'
data GetTopicAttributesResponse = GetTopicAttributesResponse'
    { _gtarsAttributes :: !(Maybe (Map Text Text))
    , _gtarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetTopicAttributesResponse' smart constructor.
getTopicAttributesResponse :: Int -> GetTopicAttributesResponse
getTopicAttributesResponse pStatus_ =
    GetTopicAttributesResponse'
    { _gtarsAttributes = Nothing
    , _gtarsStatus = pStatus_
    }

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
gtarsAttributes :: Lens' GetTopicAttributesResponse (HashMap Text Text)
gtarsAttributes = lens _gtarsAttributes (\ s a -> s{_gtarsAttributes = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
gtarsStatus :: Lens' GetTopicAttributesResponse Int
gtarsStatus = lens _gtarsStatus (\ s a -> s{_gtarsStatus = a});
