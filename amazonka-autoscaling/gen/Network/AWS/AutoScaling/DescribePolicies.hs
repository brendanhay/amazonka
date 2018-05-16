{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the policies for the specified Auto Scaling group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Creating a Request
      describePolicies
    , DescribePolicies
    -- * Request Lenses
    , dpsPolicyNames
    , dpsNextToken
    , dpsAutoScalingGroupName
    , dpsMaxRecords
    , dpsPolicyTypes

    -- * Destructuring the Response
    , describePoliciesResponse
    , DescribePoliciesResponse
    -- * Response Lenses
    , dprsNextToken
    , dprsScalingPolicies
    , dprsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePolicies' smart constructor.
data DescribePolicies = DescribePolicies'
  { _dpsPolicyNames          :: !(Maybe [Text])
  , _dpsNextToken            :: !(Maybe Text)
  , _dpsAutoScalingGroupName :: !(Maybe Text)
  , _dpsMaxRecords           :: !(Maybe Int)
  , _dpsPolicyTypes          :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpsPolicyNames' - The names of one or more policies. If you omit this parameter, all policies are described. If an group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
--
-- * 'dpsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dpsAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dpsMaxRecords' - The maximum number of items to be returned with each call. The default value is 50 and the maximum value is 100.
--
-- * 'dpsPolicyTypes' - One or more policy types. Valid values are @SimpleScaling@ and @StepScaling@ .
describePolicies
    :: DescribePolicies
describePolicies =
  DescribePolicies'
    { _dpsPolicyNames = Nothing
    , _dpsNextToken = Nothing
    , _dpsAutoScalingGroupName = Nothing
    , _dpsMaxRecords = Nothing
    , _dpsPolicyTypes = Nothing
    }


-- | The names of one or more policies. If you omit this parameter, all policies are described. If an group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
dpsPolicyNames :: Lens' DescribePolicies [Text]
dpsPolicyNames = lens _dpsPolicyNames (\ s a -> s{_dpsPolicyNames = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dpsNextToken :: Lens' DescribePolicies (Maybe Text)
dpsNextToken = lens _dpsNextToken (\ s a -> s{_dpsNextToken = a})

-- | The name of the Auto Scaling group.
dpsAutoScalingGroupName :: Lens' DescribePolicies (Maybe Text)
dpsAutoScalingGroupName = lens _dpsAutoScalingGroupName (\ s a -> s{_dpsAutoScalingGroupName = a})

-- | The maximum number of items to be returned with each call. The default value is 50 and the maximum value is 100.
dpsMaxRecords :: Lens' DescribePolicies (Maybe Int)
dpsMaxRecords = lens _dpsMaxRecords (\ s a -> s{_dpsMaxRecords = a})

-- | One or more policy types. Valid values are @SimpleScaling@ and @StepScaling@ .
dpsPolicyTypes :: Lens' DescribePolicies [Text]
dpsPolicyTypes = lens _dpsPolicyTypes (\ s a -> s{_dpsPolicyTypes = a}) . _Default . _Coerce

instance AWSPager DescribePolicies where
        page rq rs
          | stop (rs ^. dprsNextToken) = Nothing
          | stop (rs ^. dprsScalingPolicies) = Nothing
          | otherwise =
            Just $ rq & dpsNextToken .~ rs ^. dprsNextToken

instance AWSRequest DescribePolicies where
        type Rs DescribePolicies = DescribePoliciesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper "DescribePoliciesResult"
              (\ s h x ->
                 DescribePoliciesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "ScalingPolicies" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribePolicies where

instance NFData DescribePolicies where

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
               "AutoScalingGroupName" =: _dpsAutoScalingGroupName,
               "MaxRecords" =: _dpsMaxRecords,
               "PolicyTypes" =:
                 toQuery (toQueryList "member" <$> _dpsPolicyTypes)]

-- | /See:/ 'describePoliciesResponse' smart constructor.
data DescribePoliciesResponse = DescribePoliciesResponse'
  { _dprsNextToken       :: !(Maybe Text)
  , _dprsScalingPolicies :: !(Maybe [ScalingPolicy])
  , _dprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dprsScalingPolicies' - The scaling policies.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePoliciesResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribePoliciesResponse
describePoliciesResponse pResponseStatus_ =
  DescribePoliciesResponse'
    { _dprsNextToken = Nothing
    , _dprsScalingPolicies = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dprsNextToken :: Lens' DescribePoliciesResponse (Maybe Text)
dprsNextToken = lens _dprsNextToken (\ s a -> s{_dprsNextToken = a})

-- | The scaling policies.
dprsScalingPolicies :: Lens' DescribePoliciesResponse [ScalingPolicy]
dprsScalingPolicies = lens _dprsScalingPolicies (\ s a -> s{_dprsScalingPolicies = a}) . _Default . _Coerce

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePoliciesResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribePoliciesResponse where
