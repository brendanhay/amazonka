{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the policies for the specified Auto Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribePolicies.html>
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Request
      DescribePolicies
    -- ** Request constructor
    , describePolicies
    -- ** Request lenses
    , dpsrqPolicyNames
    , dpsrqNextToken
    , dpsrqMaxRecords
    , dpsrqAutoScalingGroupName
    , dpsrqPolicyTypes

    -- * Response
    , DescribePoliciesResponse
    -- ** Response constructor
    , describePoliciesResponse
    -- ** Response lenses
    , dpsrsNextToken
    , dpsrsScalingPolicies
    , dpsrsStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describePolicies' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpsrqPolicyNames'
--
-- * 'dpsrqNextToken'
--
-- * 'dpsrqMaxRecords'
--
-- * 'dpsrqAutoScalingGroupName'
--
-- * 'dpsrqPolicyTypes'
data DescribePolicies = DescribePolicies'
    { _dpsrqPolicyNames          :: !(Maybe [Text])
    , _dpsrqNextToken            :: !(Maybe Text)
    , _dpsrqMaxRecords           :: !(Maybe Int)
    , _dpsrqAutoScalingGroupName :: !(Maybe Text)
    , _dpsrqPolicyTypes          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePolicies' smart constructor.
describePolicies :: DescribePolicies
describePolicies =
    DescribePolicies'
    { _dpsrqPolicyNames = Nothing
    , _dpsrqNextToken = Nothing
    , _dpsrqMaxRecords = Nothing
    , _dpsrqAutoScalingGroupName = Nothing
    , _dpsrqPolicyTypes = Nothing
    }

-- | One or more policy names or policy ARNs to be described. If you omit
-- this list, all policy names are described. If an group name is provided,
-- the results are limited to that group. This list is limited to 50 items.
-- If you specify an unknown policy name, it is ignored with no error.
dpsrqPolicyNames :: Lens' DescribePolicies [Text]
dpsrqPolicyNames = lens _dpsrqPolicyNames (\ s a -> s{_dpsrqPolicyNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dpsrqNextToken :: Lens' DescribePolicies (Maybe Text)
dpsrqNextToken = lens _dpsrqNextToken (\ s a -> s{_dpsrqNextToken = a});

-- | The maximum number of items to be returned with each call.
dpsrqMaxRecords :: Lens' DescribePolicies (Maybe Int)
dpsrqMaxRecords = lens _dpsrqMaxRecords (\ s a -> s{_dpsrqMaxRecords = a});

-- | The name of the group.
dpsrqAutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dpsrqAutoScalingGroupName = lens _dpsrqAutoScalingGroupName (\ s a -> s{_dpsrqAutoScalingGroupName = a});

-- | One or more policy types. Valid values are @SimpleScaling@ and
-- @StepScaling@.
dpsrqPolicyTypes :: Lens' DescribePolicies [Text]
dpsrqPolicyTypes = lens _dpsrqPolicyTypes (\ s a -> s{_dpsrqPolicyTypes = a}) . _Default;

instance AWSPager DescribePolicies where
        page rq rs
          | stop (rs ^. dpsrsNextToken) = Nothing
          | stop (rs ^. dpsrsScalingPolicies) = Nothing
          | otherwise =
            Just $ rq & dpsrqNextToken .~ rs ^. dpsrsNextToken

instance AWSRequest DescribePolicies where
        type Sv DescribePolicies = AutoScaling
        type Rs DescribePolicies = DescribePoliciesResponse
        request = post
        response
          = receiveXMLWrapper "DescribePoliciesResult"
              (\ s h x ->
                 DescribePoliciesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "ScalingPolicies" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribePolicies where
        toHeaders = const mempty

instance ToPath DescribePolicies where
        toPath = const "/"

instance ToQuery DescribePolicies where
        toQuery DescribePolicies'{..}
          = mconcat
              ["Action" =: ("DescribePolicies" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "PolicyNames" =:
                 toQuery (toQueryList "member" <$> _dpsrqPolicyNames),
               "NextToken" =: _dpsrqNextToken,
               "MaxRecords" =: _dpsrqMaxRecords,
               "AutoScalingGroupName" =: _dpsrqAutoScalingGroupName,
               "PolicyTypes" =:
                 toQuery (toQueryList "member" <$> _dpsrqPolicyTypes)]

-- | /See:/ 'describePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpsrsNextToken'
--
-- * 'dpsrsScalingPolicies'
--
-- * 'dpsrsStatus'
data DescribePoliciesResponse = DescribePoliciesResponse'
    { _dpsrsNextToken       :: !(Maybe Text)
    , _dpsrsScalingPolicies :: !(Maybe [ScalingPolicy])
    , _dpsrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePoliciesResponse' smart constructor.
describePoliciesResponse :: Int -> DescribePoliciesResponse
describePoliciesResponse pStatus =
    DescribePoliciesResponse'
    { _dpsrsNextToken = Nothing
    , _dpsrsScalingPolicies = Nothing
    , _dpsrsStatus = pStatus
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dpsrsNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dpsrsNextToken = lens _dpsrsNextToken (\ s a -> s{_dpsrsNextToken = a});

-- | The scaling policies.
dpsrsScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dpsrsScalingPolicies = lens _dpsrsScalingPolicies (\ s a -> s{_dpsrsScalingPolicies = a}) . _Default;

-- | FIXME: Undocumented member.
dpsrsStatus :: Lens' DescribePoliciesResponse Int
dpsrsStatus = lens _dpsrsStatus (\ s a -> s{_dpsrsStatus = a});
