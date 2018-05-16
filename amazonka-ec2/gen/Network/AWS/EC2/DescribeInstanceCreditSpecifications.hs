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
-- Module      : Network.AWS.EC2.DescribeInstanceCreditSpecifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the credit option for CPU usage of one or more of your T2 instances. The credit options are @standard@ and @unlimited@ .
--
--
-- If you do not specify an instance ID, Amazon EC2 returns only the T2 instances with the @unlimited@ credit option. If you specify one or more instance IDs, Amazon EC2 returns the credit option (@standard@ or @unlimited@ ) of those instances. If you specify an instance ID that is not valid, such as an instance that is not a T2 instance, an error is returned.
--
-- Recently terminated instances might appear in the returned results. This interval is usually less than one hour.
--
-- If an Availability Zone is experiencing a service disruption and you specify instance IDs in the affected zone, or do not specify any instance IDs at all, the call fails. If you specify only instance IDs in an unaffected zone, the call works normally.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/t2-instances.html T2 Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DescribeInstanceCreditSpecifications
    (
    -- * Creating a Request
      describeInstanceCreditSpecifications
    , DescribeInstanceCreditSpecifications
    -- * Request Lenses
    , dicsFilters
    , dicsNextToken
    , dicsInstanceIds
    , dicsDryRun
    , dicsMaxResults

    -- * Destructuring the Response
    , describeInstanceCreditSpecificationsResponse
    , DescribeInstanceCreditSpecificationsResponse
    -- * Response Lenses
    , dicsrsNextToken
    , dicsrsInstanceCreditSpecifications
    , dicsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInstanceCreditSpecifications' smart constructor.
data DescribeInstanceCreditSpecifications = DescribeInstanceCreditSpecifications'
  { _dicsFilters     :: !(Maybe [Filter])
  , _dicsNextToken   :: !(Maybe Text)
  , _dicsInstanceIds :: !(Maybe [Text])
  , _dicsDryRun      :: !(Maybe Bool)
  , _dicsMaxResults  :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceCreditSpecifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dicsFilters' - One or more filters.     * @instance-id@ - The ID of the instance.
--
-- * 'dicsNextToken' - The token to retrieve the next page of results.
--
-- * 'dicsInstanceIds' - One or more instance IDs. Default: Describes all your instances. Constraints: Maximum 1000 explicitly specified instance IDs.
--
-- * 'dicsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dicsMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
describeInstanceCreditSpecifications
    :: DescribeInstanceCreditSpecifications
describeInstanceCreditSpecifications =
  DescribeInstanceCreditSpecifications'
    { _dicsFilters = Nothing
    , _dicsNextToken = Nothing
    , _dicsInstanceIds = Nothing
    , _dicsDryRun = Nothing
    , _dicsMaxResults = Nothing
    }


-- | One or more filters.     * @instance-id@ - The ID of the instance.
dicsFilters :: Lens' DescribeInstanceCreditSpecifications [Filter]
dicsFilters = lens _dicsFilters (\ s a -> s{_dicsFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
dicsNextToken :: Lens' DescribeInstanceCreditSpecifications (Maybe Text)
dicsNextToken = lens _dicsNextToken (\ s a -> s{_dicsNextToken = a})

-- | One or more instance IDs. Default: Describes all your instances. Constraints: Maximum 1000 explicitly specified instance IDs.
dicsInstanceIds :: Lens' DescribeInstanceCreditSpecifications [Text]
dicsInstanceIds = lens _dicsInstanceIds (\ s a -> s{_dicsInstanceIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dicsDryRun :: Lens' DescribeInstanceCreditSpecifications (Maybe Bool)
dicsDryRun = lens _dicsDryRun (\ s a -> s{_dicsDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
dicsMaxResults :: Lens' DescribeInstanceCreditSpecifications (Maybe Int)
dicsMaxResults = lens _dicsMaxResults (\ s a -> s{_dicsMaxResults = a})

instance AWSRequest
           DescribeInstanceCreditSpecifications
         where
        type Rs DescribeInstanceCreditSpecifications =
             DescribeInstanceCreditSpecificationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstanceCreditSpecificationsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "instanceCreditSpecificationSet" .!@ mempty
                        >>= may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeInstanceCreditSpecifications
         where

instance NFData DescribeInstanceCreditSpecifications
         where

instance ToHeaders
           DescribeInstanceCreditSpecifications
         where
        toHeaders = const mempty

instance ToPath DescribeInstanceCreditSpecifications
         where
        toPath = const "/"

instance ToQuery DescribeInstanceCreditSpecifications
         where
        toQuery DescribeInstanceCreditSpecifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceCreditSpecifications" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dicsFilters),
               "NextToken" =: _dicsNextToken,
               toQuery
                 (toQueryList "InstanceId" <$> _dicsInstanceIds),
               "DryRun" =: _dicsDryRun,
               "MaxResults" =: _dicsMaxResults]

-- | /See:/ 'describeInstanceCreditSpecificationsResponse' smart constructor.
data DescribeInstanceCreditSpecificationsResponse = DescribeInstanceCreditSpecificationsResponse'
  { _dicsrsNextToken :: !(Maybe Text)
  , _dicsrsInstanceCreditSpecifications :: !(Maybe [InstanceCreditSpecification])
  , _dicsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceCreditSpecificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dicsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dicsrsInstanceCreditSpecifications' - Information about the credit option for CPU usage of an instance.
--
-- * 'dicsrsResponseStatus' - -- | The response status code.
describeInstanceCreditSpecificationsResponse
    :: Int -- ^ 'dicsrsResponseStatus'
    -> DescribeInstanceCreditSpecificationsResponse
describeInstanceCreditSpecificationsResponse pResponseStatus_ =
  DescribeInstanceCreditSpecificationsResponse'
    { _dicsrsNextToken = Nothing
    , _dicsrsInstanceCreditSpecifications = Nothing
    , _dicsrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dicsrsNextToken :: Lens' DescribeInstanceCreditSpecificationsResponse (Maybe Text)
dicsrsNextToken = lens _dicsrsNextToken (\ s a -> s{_dicsrsNextToken = a})

-- | Information about the credit option for CPU usage of an instance.
dicsrsInstanceCreditSpecifications :: Lens' DescribeInstanceCreditSpecificationsResponse [InstanceCreditSpecification]
dicsrsInstanceCreditSpecifications = lens _dicsrsInstanceCreditSpecifications (\ s a -> s{_dicsrsInstanceCreditSpecifications = a}) . _Default . _Coerce

-- | -- | The response status code.
dicsrsResponseStatus :: Lens' DescribeInstanceCreditSpecificationsResponse Int
dicsrsResponseStatus = lens _dicsrsResponseStatus (\ s a -> s{_dicsrsResponseStatus = a})

instance NFData
           DescribeInstanceCreditSpecificationsResponse
         where
