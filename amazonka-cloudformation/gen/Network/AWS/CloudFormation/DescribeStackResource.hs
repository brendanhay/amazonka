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

-- Module      : Network.AWS.CloudFormation.DescribeStackResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the specified resource in the specified stack. For
-- deleted stacks, DescribeStackResource returns resource information for up
-- to 90 days after the stack has been deleted.
module Network.AWS.CloudFormation.DescribeStackResource
    (
    -- * Request
      DescribeStackResourceInput
    -- ** Request constructor
    , describeStackResourceInput
    -- ** Request lenses
    , dsri1LogicalResourceId
    , dsri1StackName

    -- * Response
    , DescribeStackResourceOutput
    -- ** Response constructor
    , describeStackResourceOutput
    -- ** Response lenses
    , dsroStackResourceDetail
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data DescribeStackResourceInput = DescribeStackResourceInput
    { _dsri1LogicalResourceId :: Text
    , _dsri1StackName         :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeStackResourceInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsri1LogicalResourceId' @::@ 'Text'
--
-- * 'dsri1StackName' @::@ 'Text'
--
describeStackResourceInput :: Text -- ^ 'dsri1StackName'
                           -> Text -- ^ 'dsri1LogicalResourceId'
                           -> DescribeStackResourceInput
describeStackResourceInput p1 p2 = DescribeStackResourceInput
    { _dsri1StackName         = p1
    , _dsri1LogicalResourceId = p2
    }

-- | The logical name of the resource as specified in the template. Default:
-- There is no default value.
dsri1LogicalResourceId :: Lens' DescribeStackResourceInput Text
dsri1LogicalResourceId =
    lens _dsri1LogicalResourceId (\s a -> s { _dsri1LogicalResourceId = a })

-- | The name or the unique identifier associated with the stack, which are
-- not always interchangeable: Running stacks: You can specify either the
-- stack's name or its unique stack ID. Deleted stacks: You must specify the
-- unique stack ID. Default: There is no default value.
dsri1StackName :: Lens' DescribeStackResourceInput Text
dsri1StackName = lens _dsri1StackName (\s a -> s { _dsri1StackName = a })

instance ToQuery DescribeStackResourceInput

instance ToPath DescribeStackResourceInput where
    toPath = const "/"

newtype DescribeStackResourceOutput = DescribeStackResourceOutput
    { _dsroStackResourceDetail :: Maybe StackResourceDetail
    } deriving (Eq, Show, Generic)

-- | 'DescribeStackResourceOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsroStackResourceDetail' @::@ 'Maybe' 'StackResourceDetail'
--
describeStackResourceOutput :: DescribeStackResourceOutput
describeStackResourceOutput = DescribeStackResourceOutput
    { _dsroStackResourceDetail = Nothing
    }

-- | A StackResourceDetail structure containing the description of the
-- specified resource in the specified stack.
dsroStackResourceDetail :: Lens' DescribeStackResourceOutput (Maybe StackResourceDetail)
dsroStackResourceDetail =
    lens _dsroStackResourceDetail (\s a -> s { _dsroStackResourceDetail = a })

instance FromXML DescribeStackResourceOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeStackResourceOutput"

instance AWSRequest DescribeStackResourceInput where
    type Sv DescribeStackResourceInput = CloudFormation
    type Rs DescribeStackResourceInput = DescribeStackResourceOutput

    request  = post "DescribeStackResource"
    response = xmlResponse $ \h x -> DescribeStackResourceOutput
        <$> x %| "StackResourceDetail"
