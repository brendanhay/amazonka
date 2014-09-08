{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.DescribeStackEvents
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
-- (stack ID). https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackEvents &StackName=MyStack &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature] Event-1-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyStack MyStack_One AWS::CloudFormation::Stack 2010-07-27T22:26:28Z
-- CREATE_IN_PROGRESS User initiated Event-2-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::SecurityGroup 2010-07-27T22:27:28Z
-- CREATE_IN_PROGRESS {"GroupDescription":...} Event-3-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MySG1 MyStack_SG1 AWS:: SecurityGroup 2010-07-27T22:28:28Z
-- CREATE_COMPLETE.
module Network.AWS.CloudFormation.V2010_05_15.DescribeStackEvents
    (
    -- * Request
      DescribeStackEvents
    -- ** Request constructor
    , mkDescribeStackEvents
    -- ** Request lenses
    , dseStackName
    , dseNextToken

    -- * Response
    , DescribeStackEventsResponse
    -- ** Response constructor
    , mkDescribeStackEventsResponse
    -- ** Response lenses
    , dserStackEvents
    , dserNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | The input for DescribeStackEvents action.
data DescribeStackEvents = DescribeStackEvents
    { _dseStackName :: Text
    , _dseNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStackEvents' request.
mkDescribeStackEvents :: Text -- ^ 'dseStackName'
                      -> DescribeStackEvents
mkDescribeStackEvents p1 = DescribeStackEvents
    { _dseStackName = p1
    , _dseNextToken = Nothing
    }

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable: Running stacks: You can specify either the stack's
-- name or its unique stack ID. Deleted stacks: You must specify the unique
-- stack ID. Default: There is no default value.
dseStackName :: Lens' DescribeStackEvents Text
dseStackName = lens _dseStackName (\s a -> s { _dseStackName = a })

-- | String that identifies the start of the next list of events, if there is
-- one. Default: There is no default value.
dseNextToken :: Lens' DescribeStackEvents (Maybe Text)
dseNextToken = lens _dseNextToken (\s a -> s { _dseNextToken = a })

instance ToQuery DescribeStackEvents where
    toQuery = genericQuery def

-- | The output for a DescribeStackEvents action.
data DescribeStackEventsResponse = DescribeStackEventsResponse
    { _dserStackEvents :: [StackEvent]
    , _dserNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStackEventsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeStackEventsResponse :: DescribeStackEventsResponse
mkDescribeStackEventsResponse = DescribeStackEventsResponse
    { _dserStackEvents = mempty
    , _dserNextToken = Nothing
    }

-- | A list of StackEvents structures.
dserStackEvents :: Lens' DescribeStackEventsResponse [StackEvent]
dserStackEvents = lens _dserStackEvents (\s a -> s { _dserStackEvents = a })

-- | String that identifies the start of the next list of events, if there is
-- one.
dserNextToken :: Lens' DescribeStackEventsResponse (Maybe Text)
dserNextToken = lens _dserNextToken (\s a -> s { _dserNextToken = a })

instance FromXML DescribeStackEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeStackEvents where
    type Sv DescribeStackEvents = CloudFormation
    type Rs DescribeStackEvents = DescribeStackEventsResponse

    request = post "DescribeStackEvents"
    response _ = xmlResponse

instance AWSPager DescribeStackEvents where
    next rq rs = (\x -> rq & dseNextToken ?~ x)
        <$> (rs ^. dserNextToken)
