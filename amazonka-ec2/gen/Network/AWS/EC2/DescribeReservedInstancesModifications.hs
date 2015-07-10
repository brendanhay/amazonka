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
    , drimFilters
    , drimReservedInstancesModificationIds
    , drimNextToken

    -- * Response
    , DescribeReservedInstancesModificationsResponse
    -- ** Response constructor
    , describeReservedInstancesModificationsResponse
    -- ** Response lenses
    , drimrNextToken
    , drimrReservedInstancesModifications
    , drimrStatus
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
-- * 'drimFilters'
--
-- * 'drimReservedInstancesModificationIds'
--
-- * 'drimNextToken'
data DescribeReservedInstancesModifications = DescribeReservedInstancesModifications'
    { _drimFilters                          :: !(Maybe [Filter])
    , _drimReservedInstancesModificationIds :: !(Maybe [Text])
    , _drimNextToken                        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesModifications' smart constructor.
describeReservedInstancesModifications :: DescribeReservedInstancesModifications
describeReservedInstancesModifications =
    DescribeReservedInstancesModifications'
    { _drimFilters = Nothing
    , _drimReservedInstancesModificationIds = Nothing
    , _drimNextToken = Nothing
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
drimFilters :: Lens' DescribeReservedInstancesModifications [Filter]
drimFilters = lens _drimFilters (\ s a -> s{_drimFilters = a}) . _Default;

-- | IDs for the submitted modification request.
drimReservedInstancesModificationIds :: Lens' DescribeReservedInstancesModifications [Text]
drimReservedInstancesModificationIds = lens _drimReservedInstancesModificationIds (\ s a -> s{_drimReservedInstancesModificationIds = a}) . _Default;

-- | The token to retrieve the next page of results.
drimNextToken :: Lens' DescribeReservedInstancesModifications (Maybe Text)
drimNextToken = lens _drimNextToken (\ s a -> s{_drimNextToken = a});

instance AWSPager
         DescribeReservedInstancesModifications where
        page rq rs
          | stop (rs ^. drimrNextToken) = Nothing
          | stop (rs ^. drimrReservedInstancesModifications) =
            Nothing
          | otherwise =
            Just $ rq & drimNextToken .~ rs ^. drimrNextToken

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
               toQuery (toQueryList "Filter" <$> _drimFilters),
               toQuery
                 (toQueryList "ReservedInstancesModificationId" <$>
                    _drimReservedInstancesModificationIds),
               "NextToken" =: _drimNextToken]

-- | /See:/ 'describeReservedInstancesModificationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drimrNextToken'
--
-- * 'drimrReservedInstancesModifications'
--
-- * 'drimrStatus'
data DescribeReservedInstancesModificationsResponse = DescribeReservedInstancesModificationsResponse'
    { _drimrNextToken                      :: !(Maybe Text)
    , _drimrReservedInstancesModifications :: !(Maybe [ReservedInstancesModification])
    , _drimrStatus                         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeReservedInstancesModificationsResponse' smart constructor.
describeReservedInstancesModificationsResponse :: Int -> DescribeReservedInstancesModificationsResponse
describeReservedInstancesModificationsResponse pStatus =
    DescribeReservedInstancesModificationsResponse'
    { _drimrNextToken = Nothing
    , _drimrReservedInstancesModifications = Nothing
    , _drimrStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
drimrNextToken :: Lens' DescribeReservedInstancesModificationsResponse (Maybe Text)
drimrNextToken = lens _drimrNextToken (\ s a -> s{_drimrNextToken = a});

-- | The Reserved Instance modification information.
drimrReservedInstancesModifications :: Lens' DescribeReservedInstancesModificationsResponse [ReservedInstancesModification]
drimrReservedInstancesModifications = lens _drimrReservedInstancesModifications (\ s a -> s{_drimrReservedInstancesModifications = a}) . _Default;

-- | FIXME: Undocumented member.
drimrStatus :: Lens' DescribeReservedInstancesModificationsResponse Int
drimrStatus = lens _drimrStatus (\ s a -> s{_drimrStatus = a});
