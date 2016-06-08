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
-- Module      : Network.AWS.GameLift.DescribeScalingPolicies
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all scaling policies applied to a fleet.
--
-- To get a fleet\'s scaling policies, specify the fleet ID. You can filter this request by policy status, such as to retrieve only active scaling policies. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, set of < ScalingPolicy> objects is returned for the fleet.
module Network.AWS.GameLift.DescribeScalingPolicies
    (
    -- * Creating a Request
      describeScalingPolicies
    , DescribeScalingPolicies
    -- * Request Lenses
    , dNextToken
    , dStatusFilter
    , dLimit
    , dFleetId

    -- * Destructuring the Response
    , describeScalingPoliciesResponse
    , DescribeScalingPoliciesResponse
    -- * Response Lenses
    , dsprsNextToken
    , dsprsScalingPolicies
    , dsprsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'describeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
    { _dNextToken    :: !(Maybe Text)
    , _dStatusFilter :: !(Maybe ScalingStatusType)
    , _dLimit        :: !(Maybe Nat)
    , _dFleetId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken'
--
-- * 'dStatusFilter'
--
-- * 'dLimit'
--
-- * 'dFleetId'
describeScalingPolicies
    :: Text -- ^ 'dFleetId'
    -> DescribeScalingPolicies
describeScalingPolicies pFleetId_ =
    DescribeScalingPolicies'
    { _dNextToken = Nothing
    , _dStatusFilter = Nothing
    , _dLimit = Nothing
    , _dFleetId = pFleetId_
    }

-- | Token indicating the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To specify the start of the result set, do not specify a value.
dNextToken :: Lens' DescribeScalingPolicies (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | Game session status to filter results on. A scaling policy is only in force when in an Active state.
--
-- -   ACTIVE: The scaling policy is currently in force.
-- -   UPDATEREQUESTED: A request to update the scaling policy has been received.
-- -   UPDATING: A change is being made to the scaling policy.
-- -   DELETEREQUESTED: A request to delete the scaling policy has been received.
-- -   DELETING: The scaling policy is being deleted.
-- -   DELETED: The scaling policy has been deleted.
-- -   ERROR: An error occurred in creating the policy. It should be removed and recreated.
dStatusFilter :: Lens' DescribeScalingPolicies (Maybe ScalingStatusType)
dStatusFilter = lens _dStatusFilter (\ s a -> s{_dStatusFilter = a});

-- | Maximum number of results to return. You can use this parameter with /NextToken/ to get results as a set of sequential pages.
dLimit :: Lens' DescribeScalingPolicies (Maybe Natural)
dLimit = lens _dLimit (\ s a -> s{_dLimit = a}) . mapping _Nat;

-- | Unique identifier for a fleet. Specify the fleet to retrieve scaling policies for.
dFleetId :: Lens' DescribeScalingPolicies Text
dFleetId = lens _dFleetId (\ s a -> s{_dFleetId = a});

instance AWSRequest DescribeScalingPolicies where
        type Rs DescribeScalingPolicies =
             DescribeScalingPoliciesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingPoliciesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScalingPolicies" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingPolicies

instance NFData DescribeScalingPolicies

instance ToHeaders DescribeScalingPolicies where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeScalingPolicies" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingPolicies where
        toJSON DescribeScalingPolicies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dNextToken,
                  ("StatusFilter" .=) <$> _dStatusFilter,
                  ("Limit" .=) <$> _dLimit,
                  Just ("FleetId" .= _dFleetId)])

instance ToPath DescribeScalingPolicies where
        toPath = const "/"

instance ToQuery DescribeScalingPolicies where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'describeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
    { _dsprsNextToken       :: !(Maybe Text)
    , _dsprsScalingPolicies :: !(Maybe [ScalingPolicy])
    , _dsprsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsNextToken'
--
-- * 'dsprsScalingPolicies'
--
-- * 'dsprsResponseStatus'
describeScalingPoliciesResponse
    :: Int -- ^ 'dsprsResponseStatus'
    -> DescribeScalingPoliciesResponse
describeScalingPoliciesResponse pResponseStatus_ =
    DescribeScalingPoliciesResponse'
    { _dsprsNextToken = Nothing
    , _dsprsScalingPolicies = Nothing
    , _dsprsResponseStatus = pResponseStatus_
    }

-- | Token indicating where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- If a request has a limit that exactly matches the number of remaining results, a token is returned even though there are no more results to retrieve.
dsprsNextToken :: Lens' DescribeScalingPoliciesResponse (Maybe Text)
dsprsNextToken = lens _dsprsNextToken (\ s a -> s{_dsprsNextToken = a});

-- | Collection of objects containing the scaling policies matching the request.
dsprsScalingPolicies :: Lens' DescribeScalingPoliciesResponse [ScalingPolicy]
dsprsScalingPolicies = lens _dsprsScalingPolicies (\ s a -> s{_dsprsScalingPolicies = a}) . _Default . _Coerce;

-- | The response status code.
dsprsResponseStatus :: Lens' DescribeScalingPoliciesResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\ s a -> s{_dsprsResponseStatus = a});

instance NFData DescribeScalingPoliciesResponse
