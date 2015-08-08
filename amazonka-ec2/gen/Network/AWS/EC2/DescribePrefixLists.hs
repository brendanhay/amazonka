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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePrefixLists.html AWS API Reference> for DescribePrefixLists.
module Network.AWS.EC2.DescribePrefixLists
    (
    -- * Creating a Request
      DescribePrefixLists
    , describePrefixLists
    -- * Request Lenses
    , dplFilters
    , dplNextToken
    , dplPrefixListIds
    , dplDryRun
    , dplMaxResults

    -- * Destructuring the Response
    , DescribePrefixListsResponse
    , describePrefixListsResponse
    -- * Response Lenses
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
-- * 'dplFilters'
--
-- * 'dplNextToken'
--
-- * 'dplPrefixListIds'
--
-- * 'dplDryRun'
--
-- * 'dplMaxResults'
data DescribePrefixLists = DescribePrefixLists'
    { _dplFilters       :: !(Maybe [Filter])
    , _dplNextToken     :: !(Maybe Text)
    , _dplPrefixListIds :: !(Maybe [Text])
    , _dplDryRun        :: !(Maybe Bool)
    , _dplMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribePrefixLists' smart constructor.
describePrefixLists :: DescribePrefixLists
describePrefixLists =
    DescribePrefixLists'
    { _dplFilters = Nothing
    , _dplNextToken = Nothing
    , _dplPrefixListIds = Nothing
    , _dplDryRun = Nothing
    , _dplMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @prefix-list-id@: The ID of a prefix list.
--
-- -   @prefix-list-name@: The name of a prefix list.
--
dplFilters :: Lens' DescribePrefixLists [Filter]
dplFilters = lens _dplFilters (\ s a -> s{_dplFilters = a}) . _Default . _Coerce;

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
dplNextToken :: Lens' DescribePrefixLists (Maybe Text)
dplNextToken = lens _dplNextToken (\ s a -> s{_dplNextToken = a});

-- | One or more prefix list IDs.
dplPrefixListIds :: Lens' DescribePrefixLists [Text]
dplPrefixListIds = lens _dplPrefixListIds (\ s a -> s{_dplPrefixListIds = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dplDryRun :: Lens' DescribePrefixLists (Maybe Bool)
dplDryRun = lens _dplDryRun (\ s a -> s{_dplDryRun = a});

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value specified is greater than 1000, we return only
-- 1000 items.
dplMaxResults :: Lens' DescribePrefixLists (Maybe Int)
dplMaxResults = lens _dplMaxResults (\ s a -> s{_dplMaxResults = a});

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
               toQuery (toQueryList "Filter" <$> _dplFilters),
               "NextToken" =: _dplNextToken,
               toQuery (toQueryList "item" <$> _dplPrefixListIds),
               "DryRun" =: _dplDryRun,
               "MaxResults" =: _dplMaxResults]

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
describePrefixListsResponse pStatus_ =
    DescribePrefixListsResponse'
    { _dplrsNextToken = Nothing
    , _dplrsPrefixLists = Nothing
    , _dplrsStatus = pStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dplrsNextToken :: Lens' DescribePrefixListsResponse (Maybe Text)
dplrsNextToken = lens _dplrsNextToken (\ s a -> s{_dplrsNextToken = a});

-- | All available prefix lists.
dplrsPrefixLists :: Lens' DescribePrefixListsResponse [PrefixList]
dplrsPrefixLists = lens _dplrsPrefixLists (\ s a -> s{_dplrsPrefixLists = a}) . _Default . _Coerce;

-- | Undocumented member.
dplrsStatus :: Lens' DescribePrefixListsResponse Int
dplrsStatus = lens _dplrsStatus (\ s a -> s{_dplrsStatus = a});
