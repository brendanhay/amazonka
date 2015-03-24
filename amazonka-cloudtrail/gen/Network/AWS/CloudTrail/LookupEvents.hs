{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.LookupEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Looks up API activity events captured by CloudTrail that create, update, or
-- delete resources in your account. Events for a region can be looked up for
-- the times in which you had CloudTrail turned on in that region during the
-- last seven days. Lookup supports five different attributes: time range
-- (defined by a start time and end time), user name, event name, resource type,
-- and resource name. All attributes are optional. The maximum number of
-- attributes that can be specified in any one lookup request are time range and
-- one other attribute. The default number of results returned is 10, with a
-- maximum of 50 possible. The response includes a token that you can use to get
-- the next page of results. The rate of lookup requests is limited to one per
-- second per account.
--
-- Events that occurred during the selected time range will not be available
-- for lookup if CloudTrail logging was not enabled when the events occurred.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_LookupEvents.html>
module Network.AWS.CloudTrail.LookupEvents
    (
    -- * Request
      LookupEvents
    -- ** Request constructor
    , lookupEvents
    -- ** Request lenses
    , leEndTime
    , leLookupAttributes
    , leMaxResults
    , leNextToken
    , leStartTime

    -- * Response
    , LookupEventsResponse
    -- ** Response constructor
    , lookupEventsResponse
    -- ** Response lenses
    , lerEvents
    , lerNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.Types
import qualified GHC.Exts

data LookupEvents = LookupEvents
    { _leEndTime          :: Maybe POSIX
    , _leLookupAttributes :: List "LookupAttributes" LookupAttribute
    , _leMaxResults       :: Maybe Nat
    , _leNextToken        :: Maybe Text
    , _leStartTime        :: Maybe POSIX
    } deriving (Eq, Read, Show)

-- | 'LookupEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'leLookupAttributes' @::@ ['LookupAttribute']
--
-- * 'leMaxResults' @::@ 'Maybe' 'Natural'
--
-- * 'leNextToken' @::@ 'Maybe' 'Text'
--
-- * 'leStartTime' @::@ 'Maybe' 'UTCTime'
--
lookupEvents :: LookupEvents
lookupEvents = LookupEvents
    { _leLookupAttributes = mempty
    , _leStartTime        = Nothing
    , _leEndTime          = Nothing
    , _leMaxResults       = Nothing
    , _leNextToken        = Nothing
    }

-- | Specifies that only events that occur before or at the specified time are
-- returned. If the specified end time is before the specified start time, an
-- error is returned.
leEndTime :: Lens' LookupEvents (Maybe UTCTime)
leEndTime = lens _leEndTime (\s a -> s { _leEndTime = a }) . mapping _Time

-- | Contains a list of lookup attributes. Currently the list can contain only one
-- item.
leLookupAttributes :: Lens' LookupEvents [LookupAttribute]
leLookupAttributes =
    lens _leLookupAttributes (\s a -> s { _leLookupAttributes = a })
        . _List

-- | The number of events to return. Possible values are 1 through 50. The default
-- is 10.
leMaxResults :: Lens' LookupEvents (Maybe Natural)
leMaxResults = lens _leMaxResults (\s a -> s { _leMaxResults = a }) . mapping _Nat

-- | The token to use to get the next page of results after a previous API call.
-- This token must be passed in with the same parameters that were specified in
-- the the original call. For example, if the original call specified an
-- AttributeKey of 'Username' with a value of 'root', the call with NextToken
-- should include those same parameters.
leNextToken :: Lens' LookupEvents (Maybe Text)
leNextToken = lens _leNextToken (\s a -> s { _leNextToken = a })

-- | Specifies that only events that occur after or at the specified time are
-- returned. If the specified start time is after the specified end time, an
-- error is returned.
leStartTime :: Lens' LookupEvents (Maybe UTCTime)
leStartTime = lens _leStartTime (\s a -> s { _leStartTime = a }) . mapping _Time

data LookupEventsResponse = LookupEventsResponse
    { _lerEvents    :: List "Events" Event
    , _lerNextToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'LookupEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lerEvents' @::@ ['Event']
--
-- * 'lerNextToken' @::@ 'Maybe' 'Text'
--
lookupEventsResponse :: LookupEventsResponse
lookupEventsResponse = LookupEventsResponse
    { _lerEvents    = mempty
    , _lerNextToken = Nothing
    }

-- | A list of events returned based on the lookup attributes specified and the
-- CloudTrail event. The events list is sorted by time. The most recent event is
-- listed first.
lerEvents :: Lens' LookupEventsResponse [Event]
lerEvents = lens _lerEvents (\s a -> s { _lerEvents = a }) . _List

-- | The token to use to get the next page of results after a previous API call.
-- If the token does not appear, there are no more results to return. The token
-- must be passed in with the same parameters as the previous call. For example,
-- if the original call specified an AttributeKey of 'Username' with a value of
-- 'root', the call with NextToken should include those same parameters.
lerNextToken :: Lens' LookupEventsResponse (Maybe Text)
lerNextToken = lens _lerNextToken (\s a -> s { _lerNextToken = a })

instance ToPath LookupEvents where
    toPath = const "/"

instance ToQuery LookupEvents where
    toQuery = const mempty

instance ToHeaders LookupEvents

instance ToJSON LookupEvents where
    toJSON LookupEvents{..} = object
        [ "LookupAttributes" .= _leLookupAttributes
        , "StartTime"        .= _leStartTime
        , "EndTime"          .= _leEndTime
        , "MaxResults"       .= _leMaxResults
        , "NextToken"        .= _leNextToken
        ]

instance AWSRequest LookupEvents where
    type Sv LookupEvents = CloudTrail
    type Rs LookupEvents = LookupEventsResponse

    request  = post "LookupEvents"
    response = jsonResponse

instance FromJSON LookupEventsResponse where
    parseJSON = withObject "LookupEventsResponse" $ \o -> LookupEventsResponse
        <$> o .:? "Events" .!= mempty
        <*> o .:? "NextToken"
