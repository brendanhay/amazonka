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
    , dpsPolicyNames
    , dpsNextToken
    , dpsMaxRecords
    , dpsAutoScalingGroupName
    , dpsPolicyTypes

    -- * Response
    , DescribePoliciesResponse
    -- ** Response constructor
    , describePoliciesResponse
    -- ** Response lenses
    , dprsNextToken
    , dprsScalingPolicies
    , dprsStatus
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
-- * 'dpsPolicyNames'
--
-- * 'dpsNextToken'
--
-- * 'dpsMaxRecords'
--
-- * 'dpsAutoScalingGroupName'
--
-- * 'dpsPolicyTypes'
data DescribePolicies = DescribePolicies'
    { _dpsPolicyNames          :: !(Maybe [Text])
    , _dpsNextToken            :: !(Maybe Text)
    , _dpsMaxRecords           :: !(Maybe Int)
    , _dpsAutoScalingGroupName :: !(Maybe Text)
    , _dpsPolicyTypes          :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePolicies' smart constructor.
describePolicies :: DescribePolicies
describePolicies =
    DescribePolicies'
    { _dpsPolicyNames = Nothing
    , _dpsNextToken = Nothing
    , _dpsMaxRecords = Nothing
    , _dpsAutoScalingGroupName = Nothing
    , _dpsPolicyTypes = Nothing
    }

-- | One or more policy names or policy ARNs to be described. If you omit
-- this list, all policy names are described. If an group name is provided,
-- the results are limited to that group. This list is limited to 50 items.
-- If you specify an unknown policy name, it is ignored with no error.
dpsPolicyNames :: Lens' DescribePolicies [Text]
dpsPolicyNames = lens _dpsPolicyNames (\ s a -> s{_dpsPolicyNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dpsNextToken :: Lens' DescribePolicies (Maybe Text)
dpsNextToken = lens _dpsNextToken (\ s a -> s{_dpsNextToken = a});

-- | The maximum number of items to be returned with each call.
dpsMaxRecords :: Lens' DescribePolicies (Maybe Int)
dpsMaxRecords = lens _dpsMaxRecords (\ s a -> s{_dpsMaxRecords = a});

-- | The name of the group.
dpsAutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dpsAutoScalingGroupName = lens _dpsAutoScalingGroupName (\ s a -> s{_dpsAutoScalingGroupName = a});

-- | One or more policy types. Valid values are @SimpleScaling@ and
-- @StepScaling@.
dpsPolicyTypes :: Lens' DescribePolicies [Text]
dpsPolicyTypes = lens _dpsPolicyTypes (\ s a -> s{_dpsPolicyTypes = a}) . _Default;

instance AWSPager DescribePolicies where
        page rq rs
          | stop (rs ^. dprsNextToken) = Nothing
          | stop (rs ^. dprsScalingPolicies) = Nothing
          | otherwise =
            Just $ rq & dpsNextToken .~ rs ^. dprsNextToken

instance AWSRequest DescribePolicies where
        type Sv DescribePolicies = AutoScaling
        type Rs DescribePolicies = DescribePoliciesResponse
        request = postQuery
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
                 toQuery (toQueryList "member" <$> _dpsPolicyNames),
               "NextToken" =: _dpsNextToken,
               "MaxRecords" =: _dpsMaxRecords,
               "AutoScalingGroupName" =: _dpsAutoScalingGroupName,
               "PolicyTypes" =:
                 toQuery (toQueryList "member" <$> _dpsPolicyTypes)]

-- | /See:/ 'describePoliciesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprsNextToken'
--
-- * 'dprsScalingPolicies'
--
-- * 'dprsStatus'
data DescribePoliciesResponse = DescribePoliciesResponse'
    { _dprsNextToken       :: !(Maybe Text)
    , _dprsScalingPolicies :: !(Maybe [ScalingPolicy])
    , _dprsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePoliciesResponse' smart constructor.
describePoliciesResponse :: Int -> DescribePoliciesResponse
describePoliciesResponse pStatus_ =
    DescribePoliciesResponse'
    { _dprsNextToken = Nothing
    , _dprsScalingPolicies = Nothing
    , _dprsStatus = pStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dprsNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprsNextToken = lens _dprsNextToken (\ s a -> s{_dprsNextToken = a});

-- | The scaling policies.
dprsScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dprsScalingPolicies = lens _dprsScalingPolicies (\ s a -> s{_dprsScalingPolicies = a}) . _Default;

-- | FIXME: Undocumented member.
dprsStatus :: Lens' DescribePoliciesResponse Int
dprsStatus = lens _dprsStatus (\ s a -> s{_dprsStatus = a});
