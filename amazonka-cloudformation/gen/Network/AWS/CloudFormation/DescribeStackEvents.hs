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
-- CloudFormation User Guide.
module Network.AWS.CloudFormation.DescribeStackEvents
    (
    -- * Request
      DescribeStackEventsInput
    -- ** Request constructor
    , describeStackEvents
    -- ** Request lenses
    , dseiNextToken
    , dseiStackName

    -- * Response
    , DescribeStackEventsOutput
    -- ** Response constructor
    , describeStackEventsResponse
    -- ** Response lenses
    , dseoNextToken
    , dseoStackEvents
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStackEventsInput = DescribeStackEventsInput
    { _dseiNextToken :: Maybe Text
    , _dseiStackName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStackEventsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dseiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dseiStackName' @::@ 'Maybe' 'Text'
--
describeStackEvents :: DescribeStackEventsInput
describeStackEvents = DescribeStackEventsInput
    { _dseiStackName = Nothing
    , _dseiNextToken = Nothing
    }

-- | String that identifies the start of the next list of events, if there is
-- one. Default: There is no default value.
dseiNextToken :: Lens' DescribeStackEventsInput (Maybe Text)
dseiNextToken = lens _dseiNextToken (\s a -> s { _dseiNextToken = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
dseiStackName :: Lens' DescribeStackEventsInput (Maybe Text)
dseiStackName = lens _dseiStackName (\s a -> s { _dseiStackName = a })

instance ToPath DescribeStackEventsInput where
    toPath = const "/"

instance ToQuery DescribeStackEventsInput

data DescribeStackEventsOutput = DescribeStackEventsOutput
    { _dseoNextToken   :: Maybe Text
    , _dseoStackEvents :: [StackEvent]
    } deriving (Eq, Show, Generic)

-- | 'DescribeStackEventsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dseoNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dseoStackEvents' @::@ ['StackEvent']
--
describeStackEventsResponse :: DescribeStackEventsOutput
describeStackEventsResponse = DescribeStackEventsOutput
    { _dseoStackEvents = mempty
    , _dseoNextToken   = Nothing
    }

-- | String that identifies the start of the next list of events, if there is
-- one.
dseoNextToken :: Lens' DescribeStackEventsOutput (Maybe Text)
dseoNextToken = lens _dseoNextToken (\s a -> s { _dseoNextToken = a })

-- | A list of StackEvents structures.
dseoStackEvents :: Lens' DescribeStackEventsOutput [StackEvent]
dseoStackEvents = lens _dseoStackEvents (\s a -> s { _dseoStackEvents = a })

instance AWSRequest DescribeStackEventsInput where
    type Sv DescribeStackEventsInput = CloudFormation
    type Rs DescribeStackEventsInput = DescribeStackEventsOutput

    request  = post "DescribeStackEvents"
    response = xmlResponse $ \h x -> DescribeStackEventsOutput
        <$> x %| "NextToken"
        <*> x %| "StackEvents"
