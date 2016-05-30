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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides descriptive information for scaling activities with a specified
-- service namespace.
--
-- You can filter the results in a service namespace with the 'ResourceId'
-- and 'ScalableDimension' parameters.
--
-- Scaling activities are triggered by CloudWatch alarms that are
-- associated with scaling policies. To view the existing scaling policies
-- for a service namespace, see < DescribeScalingPolicies>. To create a new
-- scaling policy or update an existing one, see < PutScalingPolicy>.
module Network.AWS.ApplicationAutoScaling.DescribeScalingActivities
    (
    -- * Creating a Request
      describeScalingActivities
    , DescribeScalingActivities
    -- * Request Lenses
    , dsaScalableDimension
    , dsaResourceId
    , dsaNextToken
    , dsaMaxResults
    , dsaServiceNamespace

    -- * Destructuring the Response
    , describeScalingActivitiesResponse
    , DescribeScalingActivitiesResponse
    -- * Response Lenses
    , dsarsScalingActivities
    , dsarsNextToken
    , dsarsResponseStatus
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
    { _dsaScalableDimension :: !(Maybe ScalableDimension)
    , _dsaResourceId        :: !(Maybe Text)
    , _dsaNextToken         :: !(Maybe Text)
    , _dsaMaxResults        :: !(Maybe Int)
    , _dsaServiceNamespace  :: !ServiceNamespace
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivities' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaScalableDimension'
--
-- * 'dsaResourceId'
--
-- * 'dsaNextToken'
--
-- * 'dsaMaxResults'
--
-- * 'dsaServiceNamespace'
describeScalingActivities
    :: ServiceNamespace -- ^ 'dsaServiceNamespace'
    -> DescribeScalingActivities
describeScalingActivities pServiceNamespace_ =
    DescribeScalingActivities'
    { _dsaScalableDimension = Nothing
    , _dsaResourceId = Nothing
    , _dsaNextToken = Nothing
    , _dsaMaxResults = Nothing
    , _dsaServiceNamespace = pServiceNamespace_
    }

-- | The scalable dimension associated with the scaling activity. The
-- scalable dimension contains the service namespace, resource type, and
-- scaling property, such as 'ecs:service:DesiredCount' for the desired
-- task count of an Amazon ECS service. If you specify a scalable
-- dimension, you must also specify a resource ID.
dsaScalableDimension :: Lens' DescribeScalingActivities (Maybe ScalableDimension)
dsaScalableDimension = lens _dsaScalableDimension (\ s a -> s{_dsaScalableDimension = a});

-- | The unique identifier string for the resource associated with the
-- scaling activity. For Amazon ECS services, this value is the resource
-- type, followed by the cluster name and service name, such as
-- 'service\/default\/sample-webapp'. If you specify a scalable dimension,
-- you must also specify a resource ID.
dsaResourceId :: Lens' DescribeScalingActivities (Maybe Text)
dsaResourceId = lens _dsaResourceId (\ s a -> s{_dsaResourceId = a});

-- | The 'NextToken' value returned from a previous paginated
-- 'DescribeScalingActivities' request. Pagination continues from the end
-- of the previous results that returned the 'NextToken' value. This value
-- is 'null' when there are no more results to return.
dsaNextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsaNextToken = lens _dsaNextToken (\ s a -> s{_dsaNextToken = a});

-- | The maximum number of scaling activity results returned by
-- 'DescribeScalingActivities' in paginated output. When this parameter is
-- used, 'DescribeScalingActivities' returns up to 'MaxResults' results in
-- a single page along with a 'NextToken' response element. The remaining
-- results of the initial request can be seen by sending another
-- 'DescribeScalingActivities' request with the returned 'NextToken' value.
-- This value can be between 1 and 50. If this parameter is not used, then
-- 'DescribeScalingActivities' returns up to 50 results and a 'NextToken'
-- value, if applicable.
dsaMaxResults :: Lens' DescribeScalingActivities (Maybe Int)
dsaMaxResults = lens _dsaMaxResults (\ s a -> s{_dsaMaxResults = a});

-- | The namespace for the AWS service that the scaling activity is
-- associated with. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
-- in the Amazon Web Services General Reference.
dsaServiceNamespace :: Lens' DescribeScalingActivities ServiceNamespace
dsaServiceNamespace = lens _dsaServiceNamespace (\ s a -> s{_dsaServiceNamespace = a});

instance AWSRequest DescribeScalingActivities where
        type Rs DescribeScalingActivities =
             DescribeScalingActivitiesResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalingActivitiesResponse' <$>
                   (x .?> "ScalingActivities" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalingActivities

instance NFData DescribeScalingActivities

instance ToHeaders DescribeScalingActivities where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScalingActivities"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalingActivities where
        toJSON DescribeScalingActivities'{..}
          = object
              (catMaybes
                 [("ScalableDimension" .=) <$> _dsaScalableDimension,
                  ("ResourceId" .=) <$> _dsaResourceId,
                  ("NextToken" .=) <$> _dsaNextToken,
                  ("MaxResults" .=) <$> _dsaMaxResults,
                  Just ("ServiceNamespace" .= _dsaServiceNamespace)])

instance ToPath DescribeScalingActivities where
        toPath = const "/"

instance ToQuery DescribeScalingActivities where
        toQuery = const mempty

-- | /See:/ 'describeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
    { _dsarsScalingActivities :: !(Maybe [ScalingActivity])
    , _dsarsNextToken         :: !(Maybe Text)
    , _dsarsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalingActivitiesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsarsScalingActivities'
--
-- * 'dsarsNextToken'
--
-- * 'dsarsResponseStatus'
describeScalingActivitiesResponse
    :: Int -- ^ 'dsarsResponseStatus'
    -> DescribeScalingActivitiesResponse
describeScalingActivitiesResponse pResponseStatus_ =
    DescribeScalingActivitiesResponse'
    { _dsarsScalingActivities = Nothing
    , _dsarsNextToken = Nothing
    , _dsarsResponseStatus = pResponseStatus_
    }

-- | A list of scaling activity objects.
dsarsScalingActivities :: Lens' DescribeScalingActivitiesResponse [ScalingActivity]
dsarsScalingActivities = lens _dsarsScalingActivities (\ s a -> s{_dsarsScalingActivities = a}) . _Default . _Coerce;

-- | The 'NextToken' value to include in a future 'DescribeScalingActivities'
-- request. When the results of a 'DescribeScalingActivities' request
-- exceed 'MaxResults', this value can be used to retrieve the next page of
-- results. This value is 'null' when there are no more results to return.
dsarsNextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsarsNextToken = lens _dsarsNextToken (\ s a -> s{_dsarsNextToken = a});

-- | The response status code.
dsarsResponseStatus :: Lens' DescribeScalingActivitiesResponse Int
dsarsResponseStatus = lens _dsarsResponseStatus (\ s a -> s{_dsarsResponseStatus = a});

instance NFData DescribeScalingActivitiesResponse
