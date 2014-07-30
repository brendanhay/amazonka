{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.DescribeStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DescribeStacks
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] MyStack
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- 2010-07-27T22:28:28Z CREATE_COMPLETE false StartPage
-- http://my-load-balancer.amazonaws.com:80/index.html.
module Network.AWS.CloudFormation.V2010_05_15.DescribeStacks where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudFormation.V2010_05_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeStacks' request.
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
    { _dsjNextToken = Nothing
    , _dsjStackName = Nothing
    }

data DescribeStacks = DescribeStacks
    { _dsjNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stacks, if
      -- there is one.
    , _dsjStackName :: Maybe Text
      -- ^ The name or the unique identifier associated with the stack,
      -- which are not always interchangeable: Running stacks: You can
      -- specify either the stack's name or its unique stack ID. Deleted
      -- stacks: You must specify the unique stack ID. Default: There is
      -- no default value.
    } deriving (Generic)

instance ToQuery DescribeStacks where
    toQuery = genericToQuery def

instance AWSRequest DescribeStacks where
    type Sv DescribeStacks = CloudFormation
    type Rs DescribeStacks = DescribeStacksResponse

    request = post "DescribeStacks"
    response _ = xmlResponse

instance AWSPager DescribeStacks where
    next rq rs = (\x -> rq { _dsjNextToken = Just x })
        <$> _dsoNextToken rs

data DescribeStacksResponse = DescribeStacksResponse
    { _dsoNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stacks, if
      -- there is one.
    , _dsoStacks :: [Stack]
      -- ^ A list of stack structures.
    } deriving (Generic)

instance FromXML DescribeStacksResponse where
    fromXMLOptions = xmlOptions
