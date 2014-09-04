{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
module Network.AWS.Redshift.V2012_12_01.DescribeEventSubscriptions
    (
    -- * Request
      DescribeEventSubscriptions
    -- ** Request constructor
    , mkDescribeEventSubscriptionsMessage
    -- ** Request lenses
    , desnSubscriptionName
    , desnMaxRecords
    , desnMarker

    -- * Response
    , DescribeEventSubscriptionsResponse
    -- ** Response lenses
    , esmMarker
    , esmEventSubscriptionsList
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventSubscriptions' request.
mkDescribeEventSubscriptionsMessage :: DescribeEventSubscriptions
mkDescribeEventSubscriptionsMessage = DescribeEventSubscriptions
    { _desnSubscriptionName = Nothing
    , _desnMaxRecords = Nothing
    , _desnMarker = Nothing
    }
{-# INLINE mkDescribeEventSubscriptionsMessage #-}

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { _desnSubscriptionName :: Maybe Text
      -- ^ The name of the Amazon Redshift event notification subscription
      -- to be described.
    , _desnMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _desnMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeEventSubscriptions request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The name of the Amazon Redshift event notification subscription to be
-- described.
desnSubscriptionName :: Lens' DescribeEventSubscriptions (Maybe Text)
desnSubscriptionName = lens _desnSubscriptionName (\s a -> s { _desnSubscriptionName = a })
{-# INLINE desnSubscriptionName #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
desnMaxRecords :: Lens' DescribeEventSubscriptions (Maybe Integer)
desnMaxRecords = lens _desnMaxRecords (\s a -> s { _desnMaxRecords = a })
{-# INLINE desnMaxRecords #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeEventSubscriptions request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
desnMarker :: Lens' DescribeEventSubscriptions (Maybe Text)
desnMarker = lens _desnMarker (\s a -> s { _desnMarker = a })
{-# INLINE desnMarker #-}

instance ToQuery DescribeEventSubscriptions where
    toQuery = genericQuery def

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { _esmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _esmEventSubscriptionsList :: [EventSubscription]
      -- ^ A list of event subscriptions.
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
esmMarker :: Lens' DescribeEventSubscriptionsResponse (Maybe Text)
esmMarker = lens _esmMarker (\s a -> s { _esmMarker = a })
{-# INLINE esmMarker #-}

-- | A list of event subscriptions.
esmEventSubscriptionsList :: Lens' DescribeEventSubscriptionsResponse ([EventSubscription])
esmEventSubscriptionsList = lens _esmEventSubscriptionsList (\s a -> s { _esmEventSubscriptionsList = a })
{-# INLINE esmEventSubscriptionsList #-}

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventSubscriptions where
    type Sv DescribeEventSubscriptions = Redshift
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse

    request = post "DescribeEventSubscriptions"
    response _ = xmlResponse

instance AWSPager DescribeEventSubscriptions where
    next rq rs = (\x -> rq { _desnMarker = Just x })
        <$> (_esmMarker rs)
