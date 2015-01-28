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

-- Module      : Network.AWS.AutoScaling.DescribePolicies
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

-- | Describes the policies for the specified Auto Scaling group.
--
-- You can specify a maximum number of items to be returned with a single call.
-- If there are more items to return, the call returns a token. To get the next
-- set of items, repeat the call with the returned token in the 'NextToken'
-- parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Request
      DescribePolicies
    -- ** Request constructor
    , describePolicies
    -- ** Request lenses
    , dp1AutoScalingGroupName
    , dp1MaxRecords
    , dp1NextToken
    , dp1PolicyNames

    -- * Response
    , DescribePoliciesResponse
    -- ** Response constructor
    , describePoliciesResponse
    -- ** Response lenses
    , dprNextToken
    , dprScalingPolicies
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribePolicies = DescribePolicies
    { _dp1AutoScalingGroupName :: Maybe Text
    , _dp1MaxRecords           :: Maybe Int
    , _dp1NextToken            :: Maybe Text
    , _dp1PolicyNames          :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribePolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dp1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dp1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dp1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dp1PolicyNames' @::@ ['Text']
--
describePolicies :: DescribePolicies
describePolicies = DescribePolicies
    { _dp1AutoScalingGroupName = Nothing
    , _dp1PolicyNames          = mempty
    , _dp1NextToken            = Nothing
    , _dp1MaxRecords           = Nothing
    }

-- | The name of the group.
dp1AutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dp1AutoScalingGroupName =
    lens _dp1AutoScalingGroupName (\s a -> s { _dp1AutoScalingGroupName = a })

-- | The maximum number of items to be returned with each call.
dp1MaxRecords :: Lens' DescribePolicies (Maybe Int)
dp1MaxRecords = lens _dp1MaxRecords (\s a -> s { _dp1MaxRecords = a })

-- | The token for the next set of items to return. (You received this token from
-- a previous call.)
dp1NextToken :: Lens' DescribePolicies (Maybe Text)
dp1NextToken = lens _dp1NextToken (\s a -> s { _dp1NextToken = a })

-- | One or more policy names or policy ARNs to be described. If you omit this
-- list, all policy names are described. If an group name is provided, the
-- results are limited to that group. This list is limited to 50 items. If you
-- specify an unknown policy name, it is ignored with no error.
dp1PolicyNames :: Lens' DescribePolicies [Text]
dp1PolicyNames = lens _dp1PolicyNames (\s a -> s { _dp1PolicyNames = a }) . _List

data DescribePoliciesResponse = DescribePoliciesResponse
    { _dprNextToken       :: Maybe Text
    , _dprScalingPolicies :: List "member" ScalingPolicy
    } deriving (Eq, Read, Show)

-- | 'DescribePoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dprScalingPolicies' @::@ ['ScalingPolicy']
--
describePoliciesResponse :: DescribePoliciesResponse
describePoliciesResponse = DescribePoliciesResponse
    { _dprScalingPolicies = mempty
    , _dprNextToken       = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dprNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprNextToken = lens _dprNextToken (\s a -> s { _dprNextToken = a })

-- | The scaling policies.
dprScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dprScalingPolicies =
    lens _dprScalingPolicies (\s a -> s { _dprScalingPolicies = a })
        . _List

instance ToPath DescribePolicies where
    toPath = const "/"

instance ToQuery DescribePolicies where
    toQuery DescribePolicies{..} = mconcat
        [ "AutoScalingGroupName" =? _dp1AutoScalingGroupName
        , "MaxRecords"           =? _dp1MaxRecords
        , "NextToken"            =? _dp1NextToken
        , "PolicyNames"          =? _dp1PolicyNames
        ]

instance ToHeaders DescribePolicies

instance AWSRequest DescribePolicies where
    type Sv DescribePolicies = AutoScaling
    type Rs DescribePolicies = DescribePoliciesResponse

    request  = post "DescribePolicies"
    response = xmlResponse

instance FromXML DescribePoliciesResponse where
    parseXML = withElement "DescribePoliciesResult" $ \x -> DescribePoliciesResponse
        <$> x .@? "NextToken"
        <*> x .@? "ScalingPolicies" .!@ mempty

instance AWSPager DescribePolicies where
    page rq rs
        | stop (rs ^. dprNextToken) = Nothing
        | otherwise = (\x -> rq & dp1NextToken ?~ x)
            <$> (rs ^. dprNextToken)
