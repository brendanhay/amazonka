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

-- Module      : Network.AWS.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the requester's subscriptions. Each call returns a
-- limited list of subscriptions, up to 100. If there are more subscriptions,
-- a NextToken is also returned. Use the NextToken parameter in a new
-- ListSubscriptions call to get further results.
module Network.AWS.SNS.ListSubscriptions
    (
    -- * Request
      ListSubscriptions
    -- ** Request constructor
    , listSubscriptions
    -- ** Request lenses
    , lsNextToken

    -- * Response
    , ListSubscriptionsResponse
    -- ** Response constructor
    , listSubscriptionsResponse
    -- ** Response lenses
    , lsrNextToken
    , lsrSubscriptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype ListSubscriptions = ListSubscriptions
    { _lsNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListSubscriptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsNextToken' @::@ 'Maybe' 'Text'
--
listSubscriptions :: ListSubscriptions
listSubscriptions = ListSubscriptions
    { _lsNextToken = Nothing
    }

-- | Token returned by the previous ListSubscriptions request.
lsNextToken :: Lens' ListSubscriptions (Maybe Text)
lsNextToken = lens _lsNextToken (\s a -> s { _lsNextToken = a })

instance ToQuery ListSubscriptions

instance ToPath ListSubscriptions where
    toPath = const "/"

data ListSubscriptionsResponse = ListSubscriptionsResponse
    { _lsrNextToken     :: Maybe Text
    , _lsrSubscriptions :: [Subscription]
    } deriving (Eq, Show, Generic)

-- | 'ListSubscriptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lsrSubscriptions' @::@ ['Subscription']
--
listSubscriptionsResponse :: ListSubscriptionsResponse
listSubscriptionsResponse = ListSubscriptionsResponse
    { _lsrSubscriptions = mempty
    , _lsrNextToken     = Nothing
    }

-- | Token to pass along to the next ListSubscriptions request. This element
-- is returned if there are more subscriptions to retrieve.
lsrNextToken :: Lens' ListSubscriptionsResponse (Maybe Text)
lsrNextToken = lens _lsrNextToken (\s a -> s { _lsrNextToken = a })

-- | A list of subscriptions.
lsrSubscriptions :: Lens' ListSubscriptionsResponse [Subscription]
lsrSubscriptions = lens _lsrSubscriptions (\s a -> s { _lsrSubscriptions = a })

instance FromXML ListSubscriptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListSubscriptionsResponse"

instance AWSRequest ListSubscriptions where
    type Sv ListSubscriptions = SNS
    type Rs ListSubscriptions = ListSubscriptionsResponse

    request  = post "ListSubscriptions"
    response = xmlResponse $ \h x -> ListSubscriptionsResponse
        <$> x %| "NextToken"
        <*> x %| "Subscriptions"
