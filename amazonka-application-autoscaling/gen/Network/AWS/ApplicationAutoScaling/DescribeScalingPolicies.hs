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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides descriptive information for scaling policies with a specified
-- service namespace.
--
-- You can filter the results in a service namespace with the 'ResourceId',
-- 'ScalableDimension', and 'PolicyNames' parameters.
--
-- To create a new scaling policy or update an existing one, see
-- < PutScalingPolicy>. If you are no longer using a scaling policy, you
-- can delete it with < DeleteScalingPolicy>.
module Network.AWS.ApplicationAutoScaling.DescribeScalingPolicies
    (
    -- * Creating a Request
      describeScalingPolicies
    , DescribeScalingPolicies
    -- * Request Lenses
    , dPolicyNames
    , dScalableDimension
    , dResourceId
    , dNextToken
    , dMaxResults
    , dServiceNamespace

    -- * Destructuring the Response
    , describeScalingPoliciesResponse
    , DescribeScalingPoliciesResponse
    -- * Response Lenses
    , drsNextToken
    , drsScalingPolicies
    , drsResponseStatus
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingPolicies' smart constructor.
data DescribeScalingPolicies = DescribeScalingPolicies'
    { _dPolicyNames       :: !(Maybe [Text])
    , _dScalableDimension :: !(Maybe ScalableDimension)
    , _dResourceId        :: !(Maybe Text)
    , _dNextToken         :: !(Maybe Text)
    , _dMaxResults        :: !(Maybe Int)
    , _dServiceNamespace  :: !ServiceNamespace
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dPolicyNames'
--
-- * 'dScalableDimension'
--
-- * 'dResourceId'
--
-- * 'dNextToken'
--
-- * 'dMaxResults'
--
-- * 'dServiceNamespace'
describeScalingPolicies
    :: ServiceNamespace -- ^ 'dServiceNamespace'
    -> DescribeScalingPolicies
describeScalingPolicies pServiceNamespace_ =
    DescribeScalingPolicies'
    { _dPolicyNames = Nothing
    , _dScalableDimension = Nothing
    , _dResourceId = Nothing
    , _dNextToken = Nothing
    , _dMaxResults = Nothing
    , _dServiceNamespace = pServiceNamespace_
    }

-- | The names of the scaling policies to describe.
dPolicyNames :: Lens' DescribeScalingPolicies [Text]
dPolicyNames = lens _dPolicyNames (\ s a -> s{_dPolicyNames = a}) . _Default . _Coerce;

-- | The scalable dimension of the scalable target that the scaling policy is
-- associated with. The scalable dimension contains the service namespace,
-- resource type, and scaling property, such as 'ecs:service:DesiredCount'
-- for the desired task count of an Amazon ECS service. If you specify a
-- scalable dimension, you must also specify a resource ID.
dScalableDimension :: Lens' DescribeScalingPolicies (Maybe ScalableDimension)
dScalableDimension = lens _dScalableDimension (\ s a -> s{_dScalableDimension = a});

-- | The unique resource identifier string of the scalable target that the
-- scaling policy is associated with. For Amazon ECS services, this value
-- is the resource type, followed by the cluster name and service name,
-- such as 'service\/default\/sample-webapp'. If you specify a scalable
-- dimension, you must also specify a resource ID.
dResourceId :: Lens' DescribeScalingPolicies (Maybe Text)
dResourceId = lens _dResourceId (\ s a -> s{_dResourceId = a});

-- | The 'NextToken' value returned from a previous paginated
-- 'DescribeScalingPolicies' request. Pagination continues from the end of
-- the previous results that returned the 'NextToken' value. This value is
-- 'null' when there are no more results to return.
dNextToken :: Lens' DescribeScalingPolicies (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | The maximum number of scaling policy results returned by
-- 'DescribeScalingPolicies' in paginated output. When this parameter is
-- used, 'DescribeScalingPolicies' returns up to 'MaxResults' results in a
-- single page along with a 'NextToken' response element. The remaining
-- results of the initial request can be seen by sending another
-- 'DescribeScalingPolicies' request with the returned 'NextToken' value.
-- This value can be between 1 and 50. If this parameter is not used, then
-- 'DescribeScalingPolicies' returns up to 50 results and a 'NextToken'
-- value, if applicable.
dMaxResults :: Lens' DescribeScalingPolicies (Maybe Int)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a});

-- | The AWS service namespace of the scalable target that the scaling policy
-- is associated with. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
-- in the Amazon Web Services General Reference.
dServiceNamespace :: Lens' DescribeScalingPolicies ServiceNamespace
dServiceNamespace = lens _dServiceNamespace (\ s a -> s{_dServiceNamespace = a});

instance AWSRequest DescribeScalingPolicies where
        type Rs DescribeScalingPolicies =
             DescribeScalingPoliciesResponse
        request = postJSON applicationAutoScaling
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
                    ("AnyScaleFrontendService.DescribeScalingPolicies" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingPolicies where
        toJSON DescribeScalingPolicies'{..}
          = object
              (catMaybes
                 [("PolicyNames" .=) <$> _dPolicyNames,
                  ("ScalableDimension" .=) <$> _dScalableDimension,
                  ("ResourceId" .=) <$> _dResourceId,
                  ("NextToken" .=) <$> _dNextToken,
                  ("MaxResults" .=) <$> _dMaxResults,
                  Just ("ServiceNamespace" .= _dServiceNamespace)])

instance ToPath DescribeScalingPolicies where
        toPath = const "/"

instance ToQuery DescribeScalingPolicies where
        toQuery = const mempty

-- | /See:/ 'describeScalingPoliciesResponse' smart constructor.
data DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse'
    { _drsNextToken       :: !(Maybe Text)
    , _drsScalingPolicies :: !(Maybe [ScalingPolicy])
    , _drsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken'
--
-- * 'drsScalingPolicies'
--
-- * 'drsResponseStatus'
describeScalingPoliciesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeScalingPoliciesResponse
describeScalingPoliciesResponse pResponseStatus_ =
    DescribeScalingPoliciesResponse'
    { _drsNextToken = Nothing
    , _drsScalingPolicies = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The 'NextToken' value to include in a future 'DescribeScalingPolicies'
-- request. When the results of a 'DescribeScalingPolicies' request exceed
-- 'MaxResults', this value can be used to retrieve the next page of
-- results. This value is 'null' when there are no more results to return.
drsNextToken :: Lens' DescribeScalingPoliciesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a});

-- | A list of scaling policy objects.
drsScalingPolicies :: Lens' DescribeScalingPoliciesResponse [ScalingPolicy]
drsScalingPolicies = lens _drsScalingPolicies (\ s a -> s{_drsScalingPolicies = a}) . _Default . _Coerce;

-- | The response status code.
drsResponseStatus :: Lens' DescribeScalingPoliciesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DescribeScalingPoliciesResponse
