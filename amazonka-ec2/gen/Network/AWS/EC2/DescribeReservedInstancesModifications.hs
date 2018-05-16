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
-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the modifications made to your Reserved Instances. If no parameter is specified, information about all your Reserved Instances modification requests is returned. If a modification ID is specified, only information about the specific modification is returned.
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the Amazon Elastic Compute Cloud User Guide.
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeReservedInstancesModifications
    (
    -- * Creating a Request
      describeReservedInstancesModifications
    , DescribeReservedInstancesModifications
    -- * Request Lenses
    , drimFilters
    , drimReservedInstancesModificationIds
    , drimNextToken

    -- * Destructuring the Response
    , describeReservedInstancesModificationsResponse
    , DescribeReservedInstancesModificationsResponse
    -- * Response Lenses
    , drimrsNextToken
    , drimrsReservedInstancesModifications
    , drimrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeReservedInstancesModifications.
--
--
--
-- /See:/ 'describeReservedInstancesModifications' smart constructor.
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
  { _drimFilters                          :: !(Maybe [Filter])
  , _drimReservedInstancesModificationIds :: !(Maybe [Text])
  , _drimNextToken                        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drimFilters' - One or more filters.     * @client-token@ - The idempotency token for the modification request.     * @create-date@ - The time when the modification request was created.     * @effective-date@ - The time when the modification becomes effective.     * @modification-result.reserved-instances-id@ - The ID for the Reserved Instances created as part of the modification request. This ID is only available when the status of the modification is @fulfilled@ .     * @modification-result.target-configuration.availability-zone@ - The Availability Zone for the new Reserved Instances.     * @modification-result.target-configuration.instance-count @ - The number of new Reserved Instances.     * @modification-result.target-configuration.instance-type@ - The instance type of the new Reserved Instances.     * @modification-result.target-configuration.platform@ - The network platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@ ).     * @reserved-instances-id@ - The ID of the Reserved Instances modified.     * @reserved-instances-modification-id@ - The ID of the modification request.     * @status@ - The status of the Reserved Instances modification request (@processing@ | @fulfilled@ | @failed@ ).     * @status-message@ - The reason for the status.     * @update-date@ - The time when the modification request was last updated.
--
-- * 'drimReservedInstancesModificationIds' - IDs for the submitted modification request.
--
-- * 'drimNextToken' - The token to retrieve the next page of results.
describeReservedInstancesModifications
    :: DescribeReservedInstancesModifications
describeReservedInstancesModifications =
  DescribeReservedInstancesModifications'
    { _drimFilters = Nothing
    , _drimReservedInstancesModificationIds = Nothing
    , _drimNextToken = Nothing
    }


-- | One or more filters.     * @client-token@ - The idempotency token for the modification request.     * @create-date@ - The time when the modification request was created.     * @effective-date@ - The time when the modification becomes effective.     * @modification-result.reserved-instances-id@ - The ID for the Reserved Instances created as part of the modification request. This ID is only available when the status of the modification is @fulfilled@ .     * @modification-result.target-configuration.availability-zone@ - The Availability Zone for the new Reserved Instances.     * @modification-result.target-configuration.instance-count @ - The number of new Reserved Instances.     * @modification-result.target-configuration.instance-type@ - The instance type of the new Reserved Instances.     * @modification-result.target-configuration.platform@ - The network platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@ ).     * @reserved-instances-id@ - The ID of the Reserved Instances modified.     * @reserved-instances-modification-id@ - The ID of the modification request.     * @status@ - The status of the Reserved Instances modification request (@processing@ | @fulfilled@ | @failed@ ).     * @status-message@ - The reason for the status.     * @update-date@ - The time when the modification request was last updated.
drimFilters :: Lens' DescribeReservedInstancesModifications [Filter]
drimFilters = lens _drimFilters (\ s a -> s{_drimFilters = a}) . _Default . _Coerce

-- | IDs for the submitted modification request.
drimReservedInstancesModificationIds :: Lens' DescribeReservedInstancesModifications [Text]
drimReservedInstancesModificationIds = lens _drimReservedInstancesModificationIds (\ s a -> s{_drimReservedInstancesModificationIds = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
drimNextToken :: Lens' DescribeReservedInstancesModifications (Maybe Text)
drimNextToken = lens _drimNextToken (\ s a -> s{_drimNextToken = a})

instance AWSPager
           DescribeReservedInstancesModifications
         where
        page rq rs
          | stop (rs ^. drimrsNextToken) = Nothing
          | stop (rs ^. drimrsReservedInstancesModifications) =
            Nothing
          | otherwise =
            Just $ rq & drimNextToken .~ rs ^. drimrsNextToken

instance AWSRequest
           DescribeReservedInstancesModifications
         where
        type Rs DescribeReservedInstancesModifications =
             DescribeReservedInstancesModificationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesModificationsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "reservedInstancesModificationsSet" .!@ mempty
                        >>= may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeReservedInstancesModifications
         where

instance NFData
           DescribeReservedInstancesModifications
         where

instance ToHeaders
           DescribeReservedInstancesModifications
         where
        toHeaders = const mempty

instance ToPath
           DescribeReservedInstancesModifications
         where
        toPath = const "/"

instance ToQuery
           DescribeReservedInstancesModifications
         where
        toQuery DescribeReservedInstancesModifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedInstancesModifications" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _drimFilters),
               toQuery
                 (toQueryList "ReservedInstancesModificationId" <$>
                    _drimReservedInstancesModificationIds),
               "NextToken" =: _drimNextToken]

-- | Contains the output of DescribeReservedInstancesModifications.
--
--
--
-- /See:/ 'describeReservedInstancesModificationsResponse' smart constructor.
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse'
  { _drimrsNextToken :: !(Maybe Text)
  , _drimrsReservedInstancesModifications :: !(Maybe [ReservedInstancesModification])
  , _drimrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReservedInstancesModificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drimrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'drimrsReservedInstancesModifications' - The Reserved Instance modification information.
--
-- * 'drimrsResponseStatus' - -- | The response status code.
describeReservedInstancesModificationsResponse
    :: Int -- ^ 'drimrsResponseStatus'
    -> DescribeReservedInstancesModificationsResponse
describeReservedInstancesModificationsResponse pResponseStatus_ =
  DescribeReservedInstancesModificationsResponse'
    { _drimrsNextToken = Nothing
    , _drimrsReservedInstancesModifications = Nothing
    , _drimrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
drimrsNextToken :: Lens' DescribeReservedInstancesModificationsResponse (Maybe Text)
drimrsNextToken = lens _drimrsNextToken (\ s a -> s{_drimrsNextToken = a})

-- | The Reserved Instance modification information.
drimrsReservedInstancesModifications :: Lens' DescribeReservedInstancesModificationsResponse [ReservedInstancesModification]
drimrsReservedInstancesModifications = lens _drimrsReservedInstancesModifications (\ s a -> s{_drimrsReservedInstancesModifications = a}) . _Default . _Coerce

-- | -- | The response status code.
drimrsResponseStatus :: Lens' DescribeReservedInstancesModificationsResponse Int
drimrsResponseStatus = lens _drimrsResponseStatus (\ s a -> s{_drimrsResponseStatus = a})

instance NFData
           DescribeReservedInstancesModificationsResponse
         where
