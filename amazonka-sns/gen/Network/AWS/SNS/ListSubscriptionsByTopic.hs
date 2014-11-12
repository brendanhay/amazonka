{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the subscriptions to a specific topic. Each call returns
-- a limited list of subscriptions, up to 100. If there are more
-- subscriptions, a NextToken is also returned. Use the NextToken parameter in
-- a new ListSubscriptionsByTopic call to get further results.
module Network.AWS.SNS.ListSubscriptionsByTopic
    (
    -- * Request
      ListSubscriptionsByTopicInput
    -- ** Request constructor
    , listSubscriptionsByTopicInput
    -- ** Request lenses
    , lsbtiNextToken
    , lsbtiTopicArn

    -- * Response
    , ListSubscriptionsByTopicResponse
    -- ** Response constructor
    , listSubscriptionsByTopicResponse
    -- ** Response lenses
    , lsbtrNextToken
    , lsbtrSubscriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data ListSubscriptionsByTopicInput = ListSubscriptionsByTopicInput
    { _lsbtiNextToken :: Maybe Text
    , _lsbtiTopicArn  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListSubscriptionsByTopicInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsbtiTopicArn' @::@ 'Text'
--
listSubscriptionsByTopicInput :: Text -- ^ 'lsbtiTopicArn'
                              -> ListSubscriptionsByTopicInput
listSubscriptionsByTopicInput p1 = ListSubscriptionsByTopicInput
    { _lsbtiTopicArn  = p1
    , _lsbtiNextToken = Nothing
    }

-- | Token returned by the previous ListSubscriptionsByTopic request.
lsbtiNextToken :: Lens' ListSubscriptionsByTopicInput (Maybe Text)
lsbtiNextToken = lens _lsbtiNextToken (\s a -> s { _lsbtiNextToken = a })

-- | The ARN of the topic for which you wish to find subscriptions.
lsbtiTopicArn :: Lens' ListSubscriptionsByTopicInput Text
lsbtiTopicArn = lens _lsbtiTopicArn (\s a -> s { _lsbtiTopicArn = a })

instance ToQuery ListSubscriptionsByTopicInput

instance ToPath ListSubscriptionsByTopicInput where
    toPath = const "/"

data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    { _lsbtrNextToken     :: Maybe Text
    , _lsbtrSubscriptions :: [Subscription]
    } deriving (Eq, Show, Generic)

-- | 'ListSubscriptionsByTopicResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsbtrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsbtrSubscriptions' @::@ ['Subscription']
--
listSubscriptionsByTopicResponse :: ListSubscriptionsByTopicResponse
listSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    { _lsbtrSubscriptions = mempty
    , _lsbtrNextToken     = Nothing
    }

-- | Token to pass along to the next ListSubscriptionsByTopic request. This
-- element is returned if there are more subscriptions to retrieve.
lsbtrNextToken :: Lens' ListSubscriptionsByTopicResponse (Maybe Text)
lsbtrNextToken = lens _lsbtrNextToken (\s a -> s { _lsbtrNextToken = a })

-- | A list of subscriptions.
lsbtrSubscriptions :: Lens' ListSubscriptionsByTopicResponse [Subscription]
lsbtrSubscriptions =
    lens _lsbtrSubscriptions (\s a -> s { _lsbtrSubscriptions = a })

instance FromXML ListSubscriptionsByTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListSubscriptionsByTopicResponse"

instance AWSRequest ListSubscriptionsByTopicInput where
    type Sv ListSubscriptionsByTopicInput = SNS
    type Rs ListSubscriptionsByTopicInput = ListSubscriptionsByTopicResponse

    request  = post "ListSubscriptionsByTopic"
    response = xmlResponse $ \h x -> ListSubscriptionsByTopicResponse
        <$> x %| "NextToken"
        <*> x %| "Subscriptions"
