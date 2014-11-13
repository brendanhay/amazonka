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

-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all stack related events for a specified stack. For more
-- information about a stack's event history, go to Stacks in the AWS
-- CloudFormation User Guide. You can list events for stacks that have failed
-- to create or have been deleted by specifying the unique stack identifier
-- (stack ID).
module Network.AWS.CloudFormation.DescribeStackEvents
    (
    -- * Request
      DescribeStackEvents
    -- ** Request constructor
    , describeStackEvents
    -- ** Request lenses
    , dseNextToken
    , dseStackName

    -- * Response
    , DescribeStackEventsResponse
    -- ** Response constructor
    , describeStackEventsResponse
    -- ** Response lenses
    , dserNextToken
    , dserStackEvents
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStackEvents = DescribeStackEvents
    { _dseNextToken :: Maybe Text
    , _dseStackName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStackEvents' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dseNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dseStackName' @::@ 'Maybe' 'Text'
--
describeStackEvents :: DescribeStackEvents
describeStackEvents = DescribeStackEvents
    { _dseStackName = Nothing
    , _dseNextToken = Nothing
    }

-- | String that identifies the start of the next list of events, if there is
-- one. Default: There is no default value.
dseNextToken :: Lens' DescribeStackEvents (Maybe Text)
dseNextToken = lens _dseNextToken (\s a -> s { _dseNextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
dseStackName :: Lens' DescribeStackEvents (Maybe Text)
dseStackName = lens _dseStackName (\s a -> s { _dseStackName = a })

instance ToQuery DescribeStackEvents

instance ToPath DescribeStackEvents where
    toPath = const "/"

data DescribeStackEventsResponse = DescribeStackEventsResponse
    { _dserNextToken   :: Maybe Text
    , _dserStackEvents :: [StackEvent]
    } deriving (Eq, Show, Generic)

-- | 'DescribeStackEventsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dserNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dserStackEvents' @::@ ['StackEvent']
--
describeStackEventsResponse :: DescribeStackEventsResponse
describeStackEventsResponse = DescribeStackEventsResponse
    { _dserStackEvents = mempty
    , _dserNextToken   = Nothing
    }

-- | String that identifies the start of the next list of events, if there is
-- one.
dserNextToken :: Lens' DescribeStackEventsResponse (Maybe Text)
dserNextToken = lens _dserNextToken (\s a -> s { _dserNextToken = a })

-- | A list of StackEvents structures.
dserStackEvents :: Lens' DescribeStackEventsResponse [StackEvent]
dserStackEvents = lens _dserStackEvents (\s a -> s { _dserStackEvents = a })

instance FromXML DescribeStackEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeStackEventsResponse"

instance AWSRequest DescribeStackEvents where
    type Sv DescribeStackEvents = CloudFormation
    type Rs DescribeStackEvents = DescribeStackEventsResponse

    request  = post "DescribeStackEvents"
    response = xmlResponse $ \h x -> DescribeStackEventsResponse
        <$> x %| "NextToken"
        <*> x %| "StackEvents"
