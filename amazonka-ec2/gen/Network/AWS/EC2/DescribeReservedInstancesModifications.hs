{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeReservedInstancesModifications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the modifications made to your Reserved Instances. If no
-- parameter is specified, information about all your Reserved Instances
-- modification requests is returned. If a modification ID is specified,
-- only information about the specific modification is returned.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstancesModifications.html>
module Network.AWS.EC2.DescribeReservedInstancesModifications
    (
    -- * Request
      DescribeReservedInstancesModifications
    -- ** Request constructor
    , describeReservedInstancesModifications
    -- ** Request lenses
    , drimrqFilters
    , drimrqReservedInstancesModificationIds
    , drimrqNextToken

    -- * Response
    , DescribeReservedInstancesModificationsResponse
    -- ** Response constructor
    , describeReservedInstancesModificationsResponse
    -- ** Response lenses
    , drimrsNextToken
    , drimrsReservedInstancesModifications
    , drimrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeReservedInstancesModifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drimrqFilters'
--
-- * 'drimrqReservedInstancesModificationIds'
--
-- * 'drimrqNextToken'
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
    { _drimrqFilters                          :: !(Maybe [Filter])
    , _drimrqReservedInstancesModificationIds :: !(Maybe [Text])
    , _drimrqNextToken                        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesModifications' smart constructor.
describeReservedInstancesModifications :: DescribeReservedInstancesModifications
describeReservedInstancesModifications =
    DescribeReservedInstancesModifications'
    { _drimrqFilters = Nothing
    , _drimrqReservedInstancesModificationIds = Nothing
    , _drimrqNextToken = Nothing
    }

-- | One or more filters.
--
-- -   @client-token@ - The idempotency token for the modification request.
--
-- -   @create-date@ - The time when the modification request was created.
--
-- -   @effective-date@ - The time when the modification becomes effective.
--
-- -   @modification-result.reserved-instances-id@ - The ID for the
--     Reserved Instances created as part of the modification request. This
--     ID is only available when the status of the modification is
--     @fulfilled@.
--
-- -   @modification-result.target-configuration.availability-zone@ - The
--     Availability Zone for the new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-count @ - The
--     number of new Reserved Instances.
--
-- -   @modification-result.target-configuration.instance-type@ - The
--     instance type of the new Reserved Instances.
--
-- -   @modification-result.target-configuration.platform@ - The network
--     platform of the new Reserved Instances (@EC2-Classic@ | @EC2-VPC@).
--
-- -   @reserved-instances-id@ - The ID of the Reserved Instances modified.
--
-- -   @reserved-instances-modification-id@ - The ID of the modification
--     request.
--
-- -   @status@ - The status of the Reserved Instances modification request
--     (@processing@ | @fulfilled@ | @failed@).
--
-- -   @status-message@ - The reason for the status.
--
-- -   @update-date@ - The time when the modification request was last
--     updated.
--
drimrqFilters :: Lens' DescribeReservedInstancesModifications [Filter]
drimrqFilters = lens _drimrqFilters (\ s a -> s{_drimrqFilters = a}) . _Default;

-- | IDs for the submitted modification request.
drimrqReservedInstancesModificationIds :: Lens' DescribeReservedInstancesModifications [Text]
drimrqReservedInstancesModificationIds = lens _drimrqReservedInstancesModificationIds (\ s a -> s{_drimrqReservedInstancesModificationIds = a}) . _Default;

-- | The token to retrieve the next page of results.
drimrqNextToken :: Lens' DescribeReservedInstancesModifications (Maybe Text)
drimrqNextToken = lens _drimrqNextToken (\ s a -> s{_drimrqNextToken = a});

instance AWSPager
         DescribeReservedInstancesModifications where
        page rq rs
          | stop (rs ^. drimrsNextToken) = Nothing
          | stop (rs ^. drimrsReservedInstancesModifications) =
            Nothing
          | otherwise =
            Just $ rq & drimrqNextToken .~ rs ^. drimrsNextToken

instance AWSRequest
         DescribeReservedInstancesModifications where
        type Sv DescribeReservedInstancesModifications = EC2
        type Rs DescribeReservedInstancesModifications =
             DescribeReservedInstancesModificationsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeReservedInstancesModificationsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "reservedInstancesModificationsSet" .!@ mempty
                        >>= may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders
         DescribeReservedInstancesModifications where
        toHeaders = const mempty

instance ToPath
         DescribeReservedInstancesModifications where
        toPath = const "/"

instance ToQuery
         DescribeReservedInstancesModifications where
        toQuery DescribeReservedInstancesModifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeReservedInstancesModifications" ::
                    ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _drimrqFilters),
               toQuery
                 (toQueryList "ReservedInstancesModificationId" <$>
                    _drimrqReservedInstancesModificationIds),
               "NextToken" =: _drimrqNextToken]

-- | /See:/ 'describeReservedInstancesModificationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drimrsNextToken'
--
-- * 'drimrsReservedInstancesModifications'
--
-- * 'drimrsStatus'
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse'
    { _drimrsNextToken                      :: !(Maybe Text)
    , _drimrsReservedInstancesModifications :: !(Maybe [ReservedInstancesModification])
    , _drimrsStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesModificationsResponse' smart constructor.
describeReservedInstancesModificationsResponse :: Int -> DescribeReservedInstancesModificationsResponse
describeReservedInstancesModificationsResponse pStatus_ =
    DescribeReservedInstancesModificationsResponse'
    { _drimrsNextToken = Nothing
    , _drimrsReservedInstancesModifications = Nothing
    , _drimrsStatus = pStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
drimrsNextToken :: Lens' DescribeReservedInstancesModificationsResponse (Maybe Text)
drimrsNextToken = lens _drimrsNextToken (\ s a -> s{_drimrsNextToken = a});

-- | The Reserved Instance modification information.
drimrsReservedInstancesModifications :: Lens' DescribeReservedInstancesModificationsResponse [ReservedInstancesModification]
drimrsReservedInstancesModifications = lens _drimrsReservedInstancesModifications (\ s a -> s{_drimrsReservedInstancesModifications = a}) . _Default;

-- | FIXME: Undocumented member.
drimrsStatus :: Lens' DescribeReservedInstancesModificationsResponse Int
drimrsStatus = lens _drimrsStatus (\ s a -> s{_drimrsStatus = a});
