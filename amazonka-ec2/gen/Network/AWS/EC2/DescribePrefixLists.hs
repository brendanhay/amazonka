{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribePrefixLists
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes available AWS services in a prefix list format, which includes the
-- prefix list name and prefix list ID of the service and the IP address range
-- for the service. A prefix list ID is required for creating an outbound
-- security group rule that allows traffic from a VPC to access an AWS service
-- through a VPC endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribePrefixLists.html>
module Network.AWS.EC2.DescribePrefixLists
    (
    -- * Request
      DescribePrefixLists
    -- ** Request constructor
    , describePrefixLists
    -- ** Request lenses
    , dplDryRun
    , dplFilters
    , dplMaxResults
    , dplNextToken
    , dplPrefixListIds

    -- * Response
    , DescribePrefixListsResponse
    -- ** Response constructor
    , describePrefixListsResponse
    -- ** Response lenses
    , dplrNextToken
    , dplrPrefixLists
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribePrefixLists = DescribePrefixLists
    { _dplDryRun        :: Maybe Bool
    , _dplFilters       :: List "Filter" Filter
    , _dplMaxResults    :: Maybe Int
    , _dplNextToken     :: Maybe Text
    , _dplPrefixListIds :: List "item" Text
    } deriving (Eq, Read, Show)

-- | 'DescribePrefixLists' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dplDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dplFilters' @::@ ['Filter']
--
-- * 'dplMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dplNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dplPrefixListIds' @::@ ['Text']
--
describePrefixLists :: DescribePrefixLists
describePrefixLists = DescribePrefixLists
    { _dplDryRun        = Nothing
    , _dplPrefixListIds = mempty
    , _dplFilters       = mempty
    , _dplMaxResults    = Nothing
    , _dplNextToken     = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dplDryRun :: Lens' DescribePrefixLists (Maybe Bool)
dplDryRun = lens _dplDryRun (\s a -> s { _dplDryRun = a })

-- | One or more filters.
--
-- 'prefix-list-id': The ID of a prefix list.
--
-- 'prefix-list-name': The name of a prefix list.
--
--
dplFilters :: Lens' DescribePrefixLists [Filter]
dplFilters = lens _dplFilters (\s a -> s { _dplFilters = a }) . _List

-- | The maximum number of items to return for this request. The request returns a
-- token that you can specify in a subsequent call to get the next set of
-- results.
--
-- Constraint: If the value specified is greater than 1000, we return only 1000
-- items.
dplMaxResults :: Lens' DescribePrefixLists (Maybe Int)
dplMaxResults = lens _dplMaxResults (\s a -> s { _dplMaxResults = a })

-- | The token for the next set of items to return. (You received this token from
-- a prior call.)
dplNextToken :: Lens' DescribePrefixLists (Maybe Text)
dplNextToken = lens _dplNextToken (\s a -> s { _dplNextToken = a })

-- | One or more prefix list IDs.
dplPrefixListIds :: Lens' DescribePrefixLists [Text]
dplPrefixListIds = lens _dplPrefixListIds (\s a -> s { _dplPrefixListIds = a }) . _List

data DescribePrefixListsResponse = DescribePrefixListsResponse
    { _dplrNextToken   :: Maybe Text
    , _dplrPrefixLists :: List "item" PrefixList
    } deriving (Eq, Read, Show)

-- | 'DescribePrefixListsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dplrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dplrPrefixLists' @::@ ['PrefixList']
--
describePrefixListsResponse :: DescribePrefixListsResponse
describePrefixListsResponse = DescribePrefixListsResponse
    { _dplrPrefixLists = mempty
    , _dplrNextToken   = Nothing
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dplrNextToken :: Lens' DescribePrefixListsResponse (Maybe Text)
dplrNextToken = lens _dplrNextToken (\s a -> s { _dplrNextToken = a })

-- | All available prefix lists.
dplrPrefixLists :: Lens' DescribePrefixListsResponse [PrefixList]
dplrPrefixLists = lens _dplrPrefixLists (\s a -> s { _dplrPrefixLists = a }) . _List

instance ToPath DescribePrefixLists where
    toPath = const "/"

instance ToQuery DescribePrefixLists where
    toQuery DescribePrefixLists{..} = mconcat
        [ "DryRun"       =? _dplDryRun
        , "Filter"       `toQueryList` _dplFilters
        , "MaxResults"   =? _dplMaxResults
        , "NextToken"    =? _dplNextToken
        , "PrefixListId" `toQueryList` _dplPrefixListIds
        ]

instance ToHeaders DescribePrefixLists

instance AWSRequest DescribePrefixLists where
    type Sv DescribePrefixLists = EC2
    type Rs DescribePrefixLists = DescribePrefixListsResponse

    request  = post "DescribePrefixLists"
    response = xmlResponse

instance FromXML DescribePrefixListsResponse where
    parseXML x = DescribePrefixListsResponse
        <$> x .@? "nextToken"
        <*> x .@? "prefixListSet" .!@ mempty
