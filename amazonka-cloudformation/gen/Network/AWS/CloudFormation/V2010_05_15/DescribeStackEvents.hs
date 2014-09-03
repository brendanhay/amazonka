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
    , describeStackEvents
    -- ** Request lenses
    , dseiStackName
    , dseiNextToken

    -- * Response
    , DescribeStackEventsResponse
    -- ** Response lenses
    , dseoNextToken
    , dseoStackEvents
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeStackEvents' request.
describeStackEvents :: Text -- ^ 'dseiStackName'
                    -> DescribeStackEvents
describeStackEvents p1 = DescribeStackEvents
    { _dseiStackName = p1
    , _dseiNextToken = Nothing
    }

data DescribeStackEvents = DescribeStackEvents
    { _dseiStackName :: Text
      -- ^ The name or the unique identifier associated with the stack,
      -- which are not always interchangeable: Running stacks: You can
      -- specify either the stack's name or its unique stack ID. Deleted
      -- stacks: You must specify the unique stack ID. Default: There is
      -- no default value.
    , _dseiNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of events, if
      -- there is one. Default: There is no default value.
    } deriving (Show, Generic)

-- | The name or the unique identifier associated with the stack, which are not
-- always interchangeable: Running stacks: You can specify either the stack's
-- name or its unique stack ID. Deleted stacks: You must specify the unique
-- stack ID. Default: There is no default value.
dseiStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeStackEvents
    -> f DescribeStackEvents
dseiStackName f x =
    (\y -> x { _dseiStackName = y })
       <$> f (_dseiStackName x)
{-# INLINE dseiStackName #-}

-- | String that identifies the start of the next list of events, if there is
-- one. Default: There is no default value.
dseiNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeStackEvents
    -> f DescribeStackEvents
dseiNextToken f x =
    (\y -> x { _dseiNextToken = y })
       <$> f (_dseiNextToken x)
{-# INLINE dseiNextToken #-}

instance ToQuery DescribeStackEvents where
    toQuery = genericQuery def

data DescribeStackEventsResponse = DescribeStackEventsResponse
    { _dseoNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of events, if
      -- there is one.
    , _dseoStackEvents :: [StackEvent]
      -- ^ A list of StackEvents structures.
    } deriving (Show, Generic)

-- | String that identifies the start of the next list of events, if there is
-- one.
dseoNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeStackEventsResponse
    -> f DescribeStackEventsResponse
dseoNextToken f x =
    (\y -> x { _dseoNextToken = y })
       <$> f (_dseoNextToken x)
{-# INLINE dseoNextToken #-}

-- | A list of StackEvents structures.
dseoStackEvents
    :: Functor f
    => ([StackEvent]
    -> f ([StackEvent]))
    -> DescribeStackEventsResponse
    -> f DescribeStackEventsResponse
dseoStackEvents f x =
    (\y -> x { _dseoStackEvents = y })
       <$> f (_dseoStackEvents x)
{-# INLINE dseoStackEvents #-}

instance FromXML DescribeStackEventsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeStackEvents where
    type Sv DescribeStackEvents = CloudFormation
    type Rs DescribeStackEvents = DescribeStackEventsResponse

    request = post "DescribeStackEvents"
    response _ = xmlResponse

instance AWSPager DescribeStackEvents where
    next rq rs = (\x -> rq { _dseiNextToken = Just x })
        <$> (_dseoNextToken rs)
