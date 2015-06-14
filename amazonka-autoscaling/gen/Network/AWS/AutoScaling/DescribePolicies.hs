{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- You can specify a maximum number of items to be returned with a single
-- call. If there are more items to return, the call returns a token. To
-- get the next set of items, repeat the call with the returned token in
-- the @NextToken@ parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Request
      DescribePolicies
    -- ** Request constructor
    , describePolicies
    -- ** Request lenses
    , describePolicyNames
    , describeNextToken
    , describeMaxRecords
    , describeAutoScalingGroupName

    -- * Response
    , DescribePoliciesResponse
    -- ** Response constructor
    , describePoliciesResponse
    -- ** Response lenses
    , dprNextToken
    , dprScalingPolicies
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'describePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'describePolicyNames'
--
-- * 'describeNextToken'
--
-- * 'describeMaxRecords'
--
-- * 'describeAutoScalingGroupName'
data DescribePolicies = DescribePolicies'{_describePolicyNames :: Maybe [Text], _describeNextToken :: Maybe Text, _describeMaxRecords :: Maybe Int, _describeAutoScalingGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribePolicies' smart constructor.
describePolicies :: DescribePolicies
describePolicies = DescribePolicies'{_describePolicyNames = Nothing, _describeNextToken = Nothing, _describeMaxRecords = Nothing, _describeAutoScalingGroupName = Nothing};

-- | One or more policy names or policy ARNs to be described. If you omit
-- this list, all policy names are described. If an group name is provided,
-- the results are limited to that group. This list is limited to 50 items.
-- If you specify an unknown policy name, it is ignored with no error.
describePolicyNames :: Lens' DescribePolicies (Maybe [Text])
describePolicyNames = lens _describePolicyNames (\ s a -> s{_describePolicyNames = a});

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeNextToken :: Lens' DescribePolicies (Maybe Text)
describeNextToken = lens _describeNextToken (\ s a -> s{_describeNextToken = a});

-- | The maximum number of items to be returned with each call.
describeMaxRecords :: Lens' DescribePolicies (Maybe Int)
describeMaxRecords = lens _describeMaxRecords (\ s a -> s{_describeMaxRecords = a});

-- | The name of the group.
describeAutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
describeAutoScalingGroupName = lens _describeAutoScalingGroupName (\ s a -> s{_describeAutoScalingGroupName = a});

instance AWSRequest DescribePolicies where
        type Sv DescribePolicies = AutoScaling
        type Rs DescribePolicies = DescribePoliciesResponse
        request = post
        response
          = receiveXMLWrapper "DescribePoliciesResult"
              (\ s h x ->
                 DescribePoliciesResponse' <$>
                   x .@? "NextToken" <*>
                     (x .@? "ScalingPolicies" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribePolicies where
        toHeaders = const mempty

instance ToPath DescribePolicies where
        toPath = const "/"

instance ToQuery DescribePolicies where
        toQuery DescribePolicies'{..}
          = mconcat
              ["Action" =: ("DescribePolicies" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "PolicyNames" =: "member" =: _describePolicyNames,
               "NextToken" =: _describeNextToken,
               "MaxRecords" =: _describeMaxRecords,
               "AutoScalingGroupName" =:
                 _describeAutoScalingGroupName]

-- | /See:/ 'describePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprNextToken'
--
-- * 'dprScalingPolicies'
data DescribePoliciesResponse = DescribePoliciesResponse'{_dprNextToken :: Maybe Text, _dprScalingPolicies :: Maybe [ScalingPolicy]} deriving (Eq, Read, Show)

-- | 'DescribePoliciesResponse' smart constructor.
describePoliciesResponse :: DescribePoliciesResponse
describePoliciesResponse = DescribePoliciesResponse'{_dprNextToken = Nothing, _dprScalingPolicies = Nothing};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dprNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprNextToken = lens _dprNextToken (\ s a -> s{_dprNextToken = a});

-- | The scaling policies.
dprScalingPolicies :: Lens' DescribePoliciesResponse (Maybe [ScalingPolicy])
dprScalingPolicies = lens _dprScalingPolicies (\ s a -> s{_dprScalingPolicies = a});
