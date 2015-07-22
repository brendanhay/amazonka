{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes available AWS services in a prefix list format, which includes
-- the prefix list name and prefix list ID of the service and the IP
-- address range for the service. A prefix list ID is required for creating
-- an outbound security group rule that allows traffic from a VPC to access
-- an AWS service through a VPC endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePrefixLists.html>
module Network.AWS.EC2.DescribePrefixLists
    (
    -- * Request
      DescribePrefixLists
    -- ** Request constructor
    , describePrefixLists
    -- ** Request lenses
    , dplrqFilters
    , dplrqNextToken
    , dplrqPrefixListIds
    , dplrqDryRun
    , dplrqMaxResults

    -- * Response
    , DescribePrefixListsResponse
    -- ** Response constructor
    , describePrefixListsResponse
    -- ** Response lenses
    , dplrsNextToken
    , dplrsPrefixLists
    , dplrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describePrefixLists' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dplrqFilters'
--
-- * 'dplrqNextToken'
--
-- * 'dplrqPrefixListIds'
--
-- * 'dplrqDryRun'
--
-- * 'dplrqMaxResults'
data DescribePrefixLists = DescribePrefixLists'
    { _dplrqFilters       :: !(Maybe [Filter])
    , _dplrqNextToken     :: !(Maybe Text)
    , _dplrqPrefixListIds :: !(Maybe [Text])
    , _dplrqDryRun        :: !(Maybe Bool)
    , _dplrqMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePrefixLists' smart constructor.
describePrefixLists :: DescribePrefixLists
describePrefixLists =
    DescribePrefixLists'
    { _dplrqFilters = Nothing
    , _dplrqNextToken = Nothing
    , _dplrqPrefixListIds = Nothing
    , _dplrqDryRun = Nothing
    , _dplrqMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
--
dplrqFilters :: Lens' DescribePrefixLists [Filter]
dplrqFilters = lens _dplrqFilters (\ s a -> s{_dplrqFilters = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
dplrqNextToken :: Lens' DescribePrefixLists (Maybe Text)
dplrqNextToken = lens _dplrqNextToken (\ s a -> s{_dplrqNextToken = a});

-- | One or more prefix list IDs.
dplrqPrefixListIds :: Lens' DescribePrefixLists [Text]
dplrqPrefixListIds = lens _dplrqPrefixListIds (\ s a -> s{_dplrqPrefixListIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dplrqDryRun :: Lens' DescribePrefixLists (Maybe Bool)
dplrqDryRun = lens _dplrqDryRun (\ s a -> s{_dplrqDryRun = a});

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value specified is greater than 1000, we return only
-- 1000 items.
dplrqMaxResults :: Lens' DescribePrefixLists (Maybe Int)
dplrqMaxResults = lens _dplrqMaxResults (\ s a -> s{_dplrqMaxResults = a});

instance AWSRequest DescribePrefixLists where
        type Sv DescribePrefixLists = EC2
        type Rs DescribePrefixLists =
             DescribePrefixListsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribePrefixListsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "prefixListSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribePrefixLists where
        toHeaders = const mempty

instance ToPath DescribePrefixLists where
        toPath = const "/"

instance ToQuery DescribePrefixLists where
        toQuery DescribePrefixLists'{..}
          = mconcat
              ["Action" =: ("DescribePrefixLists" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dplrqFilters),
               "NextToken" =: _dplrqNextToken,
               toQuery (toQueryList "item" <$> _dplrqPrefixListIds),
               "DryRun" =: _dplrqDryRun,
               "MaxResults" =: _dplrqMaxResults]

-- | /See:/ 'describePrefixListsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dplrsNextToken'
--
-- * 'dplrsPrefixLists'
--
-- * 'dplrsStatus'
data DescribePrefixListsResponse = DescribePrefixListsResponse'
    { _dplrsNextToken   :: !(Maybe Text)
    , _dplrsPrefixLists :: !(Maybe [PrefixList])
    , _dplrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePrefixListsResponse' smart constructor.
describePrefixListsResponse :: Int -> DescribePrefixListsResponse
describePrefixListsResponse pStatus =
    DescribePrefixListsResponse'
    { _dplrsNextToken = Nothing
    , _dplrsPrefixLists = Nothing
    , _dplrsStatus = pStatus
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dplrsNextToken :: Lens' DescribePrefixListsResponse (Maybe Text)
dplrsNextToken = lens _dplrsNextToken (\ s a -> s{_dplrsNextToken = a});

-- | All available prefix lists.
dplrsPrefixLists :: Lens' DescribePrefixListsResponse [PrefixList]
dplrsPrefixLists = lens _dplrsPrefixLists (\ s a -> s{_dplrsPrefixLists = a}) . _Default;

-- | FIXME: Undocumented member.
dplrsStatus :: Lens' DescribePrefixListsResponse Int
dplrsStatus = lens _dplrsStatus (\ s a -> s{_dplrsStatus = a});
