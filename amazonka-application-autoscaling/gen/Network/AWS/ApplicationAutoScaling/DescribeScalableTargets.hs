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
-- Module      : Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides descriptive information for scalable targets with a specified service namespace.
--
-- You can filter the results in a service namespace with the 'ResourceIds' and 'ScalableDimension' parameters.
--
-- To create a new scalable target or update an existing one, see < RegisterScalableTarget>. If you are no longer using a scalable target, you can deregister it with < DeregisterScalableTarget>.
module Network.AWS.ApplicationAutoScaling.DescribeScalableTargets
    (
    -- * Creating a Request
      describeScalableTargets
    , DescribeScalableTargets
    -- * Request Lenses
    , dstResourceIds
    , dstScalableDimension
    , dstNextToken
    , dstMaxResults
    , dstServiceNamespace

    -- * Destructuring the Response
    , describeScalableTargetsResponse
    , DescribeScalableTargetsResponse
    -- * Response Lenses
    , dstsrsNextToken
    , dstsrsScalableTargets
    , dstsrsResponseStatus
    ) where

import           Network.AWS.ApplicationAutoScaling.Types
import           Network.AWS.ApplicationAutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeScalableTargets' smart constructor.
data DescribeScalableTargets = DescribeScalableTargets'
    { _dstResourceIds       :: !(Maybe [Text])
    , _dstScalableDimension :: !(Maybe ScalableDimension)
    , _dstNextToken         :: !(Maybe Text)
    , _dstMaxResults        :: !(Maybe Int)
    , _dstServiceNamespace  :: !ServiceNamespace
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalableTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstResourceIds'
--
-- * 'dstScalableDimension'
--
-- * 'dstNextToken'
--
-- * 'dstMaxResults'
--
-- * 'dstServiceNamespace'
describeScalableTargets
    :: ServiceNamespace -- ^ 'dstServiceNamespace'
    -> DescribeScalableTargets
describeScalableTargets pServiceNamespace_ =
    DescribeScalableTargets'
    { _dstResourceIds = Nothing
    , _dstScalableDimension = Nothing
    , _dstNextToken = Nothing
    , _dstMaxResults = Nothing
    , _dstServiceNamespace = pServiceNamespace_
    }

-- | The unique identifier string for the resource associated with the scalable target. For Amazon ECS services, this value is the resource type, followed by the cluster name and service name, such as 'service\/default\/sample-webapp'. If you specify a scalable dimension, you must also specify a resource ID.
dstResourceIds :: Lens' DescribeScalableTargets [Text]
dstResourceIds = lens _dstResourceIds (\ s a -> s{_dstResourceIds = a}) . _Default . _Coerce;

-- | The scalable dimension associated with the scalable target. The scalable dimension contains the service namespace, resource type, and scaling property, such as 'ecs:service:DesiredCount' for the desired task count of an Amazon ECS service. If you specify a scalable dimension, you must also specify a resource ID.
dstScalableDimension :: Lens' DescribeScalableTargets (Maybe ScalableDimension)
dstScalableDimension = lens _dstScalableDimension (\ s a -> s{_dstScalableDimension = a});

-- | The 'NextToken' value returned from a previous paginated 'DescribeScalableTargets' request. Pagination continues from the end of the previous results that returned the 'NextToken' value. This value is 'null' when there are no more results to return.
dstNextToken :: Lens' DescribeScalableTargets (Maybe Text)
dstNextToken = lens _dstNextToken (\ s a -> s{_dstNextToken = a});

-- | The maximum number of scalable target results returned by 'DescribeScalableTargets' in paginated output. When this parameter is used, 'DescribeScalableTargets' returns up to 'MaxResults' results in a single page along with a 'NextToken' response element. The remaining results of the initial request can be seen by sending another 'DescribeScalableTargets' request with the returned 'NextToken' value. This value can be between 1 and 50. If this parameter is not used, then 'DescribeScalableTargets' returns up to 50 results and a 'NextToken' value, if applicable.
dstMaxResults :: Lens' DescribeScalableTargets (Maybe Int)
dstMaxResults = lens _dstMaxResults (\ s a -> s{_dstMaxResults = a});

-- | The namespace for the AWS service that the scalable target is associated with. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the Amazon Web Services General Reference.
dstServiceNamespace :: Lens' DescribeScalableTargets ServiceNamespace
dstServiceNamespace = lens _dstServiceNamespace (\ s a -> s{_dstServiceNamespace = a});

instance AWSRequest DescribeScalableTargets where
        type Rs DescribeScalableTargets =
             DescribeScalableTargetsResponse
        request = postJSON applicationAutoScaling
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScalableTargetsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "ScalableTargets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeScalableTargets

instance NFData DescribeScalableTargets

instance ToHeaders DescribeScalableTargets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.DescribeScalableTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeScalableTargets where
        toJSON DescribeScalableTargets'{..}
          = object
              (catMaybes
                 [("ResourceIds" .=) <$> _dstResourceIds,
                  ("ScalableDimension" .=) <$> _dstScalableDimension,
                  ("NextToken" .=) <$> _dstNextToken,
                  ("MaxResults" .=) <$> _dstMaxResults,
                  Just ("ServiceNamespace" .= _dstServiceNamespace)])

instance ToPath DescribeScalableTargets where
        toPath = const "/"

instance ToQuery DescribeScalableTargets where
        toQuery = const mempty

-- | /See:/ 'describeScalableTargetsResponse' smart constructor.
data DescribeScalableTargetsResponse = DescribeScalableTargetsResponse'
    { _dstsrsNextToken       :: !(Maybe Text)
    , _dstsrsScalableTargets :: !(Maybe [ScalableTarget])
    , _dstsrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeScalableTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstsrsNextToken'
--
-- * 'dstsrsScalableTargets'
--
-- * 'dstsrsResponseStatus'
describeScalableTargetsResponse
    :: Int -- ^ 'dstsrsResponseStatus'
    -> DescribeScalableTargetsResponse
describeScalableTargetsResponse pResponseStatus_ =
    DescribeScalableTargetsResponse'
    { _dstsrsNextToken = Nothing
    , _dstsrsScalableTargets = Nothing
    , _dstsrsResponseStatus = pResponseStatus_
    }

-- | The 'NextToken' value to include in a future 'DescribeScalableTargets' request. When the results of a 'DescribeScalableTargets' request exceed 'MaxResults', this value can be used to retrieve the next page of results. This value is 'null' when there are no more results to return.
dstsrsNextToken :: Lens' DescribeScalableTargetsResponse (Maybe Text)
dstsrsNextToken = lens _dstsrsNextToken (\ s a -> s{_dstsrsNextToken = a});

-- | The list of scalable targets that matches the request parameters.
dstsrsScalableTargets :: Lens' DescribeScalableTargetsResponse [ScalableTarget]
dstsrsScalableTargets = lens _dstsrsScalableTargets (\ s a -> s{_dstsrsScalableTargets = a}) . _Default . _Coerce;

-- | The response status code.
dstsrsResponseStatus :: Lens' DescribeScalableTargetsResponse Int
dstsrsResponseStatus = lens _dstsrsResponseStatus (\ s a -> s{_dstsrsResponseStatus = a});

instance NFData DescribeScalableTargetsResponse
