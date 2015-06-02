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

-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
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

-- | Describes your import snapshot tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportSnapshotTasks.html>
module Network.AWS.EC2.DescribeImportSnapshotTasks
    (
    -- * Request
      DescribeImportSnapshotTasks
    -- ** Request constructor
    , describeImportSnapshotTasks
    -- ** Request lenses
    , distDryRun
    , distFilters
    , distImportTaskIds
    , distMaxResults
    , distNextToken

    -- * Response
    , DescribeImportSnapshotTasksResponse
    -- ** Response constructor
    , describeImportSnapshotTasksResponse
    -- ** Response lenses
    , distrImportSnapshotTasks
    , distrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks
    { _distDryRun        :: Maybe Bool
    , _distFilters       :: List "Filter" Filter
    , _distImportTaskIds :: List "ImportTaskId" Text
    , _distMaxResults    :: Maybe Int
    , _distNextToken     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeImportSnapshotTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'distFilters' @::@ ['Filter']
--
-- * 'distImportTaskIds' @::@ ['Text']
--
-- * 'distMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'distNextToken' @::@ 'Maybe' 'Text'
--
describeImportSnapshotTasks :: DescribeImportSnapshotTasks
describeImportSnapshotTasks = DescribeImportSnapshotTasks
    { _distDryRun        = Nothing
    , _distImportTaskIds = mempty
    , _distNextToken     = Nothing
    , _distMaxResults    = Nothing
    , _distFilters       = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
distDryRun :: Lens' DescribeImportSnapshotTasks (Maybe Bool)
distDryRun = lens _distDryRun (\s a -> s { _distDryRun = a })

-- | One or more filters.
distFilters :: Lens' DescribeImportSnapshotTasks [Filter]
distFilters = lens _distFilters (\s a -> s { _distFilters = a }) . _List

-- | A list of import snapshot task IDs.
distImportTaskIds :: Lens' DescribeImportSnapshotTasks [Text]
distImportTaskIds =
    lens _distImportTaskIds (\s a -> s { _distImportTaskIds = a })
        . _List

-- | The maximum number of results to return in a single request.
distMaxResults :: Lens' DescribeImportSnapshotTasks (Maybe Int)
distMaxResults = lens _distMaxResults (\s a -> s { _distMaxResults = a })

-- | A token that indicates the next page of results.
distNextToken :: Lens' DescribeImportSnapshotTasks (Maybe Text)
distNextToken = lens _distNextToken (\s a -> s { _distNextToken = a })

data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse
    { _distrImportSnapshotTasks :: List "item" ImportSnapshotTask
    , _distrNextToken           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeImportSnapshotTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distrImportSnapshotTasks' @::@ ['ImportSnapshotTask']
--
-- * 'distrNextToken' @::@ 'Maybe' 'Text'
--
describeImportSnapshotTasksResponse :: DescribeImportSnapshotTasksResponse
describeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse
    { _distrImportSnapshotTasks = mempty
    , _distrNextToken           = Nothing
    }

-- | A list of zero or more import snapshot tasks that are currently active or
-- were completed or canceled in the previous 7 days.
distrImportSnapshotTasks :: Lens' DescribeImportSnapshotTasksResponse [ImportSnapshotTask]
distrImportSnapshotTasks =
    lens _distrImportSnapshotTasks
        (\s a -> s { _distrImportSnapshotTasks = a })
            . _List

-- | The token to use to get the next page of results. This value is 'null' when
-- there are no more results to return.
distrNextToken :: Lens' DescribeImportSnapshotTasksResponse (Maybe Text)
distrNextToken = lens _distrNextToken (\s a -> s { _distrNextToken = a })

instance ToPath DescribeImportSnapshotTasks where
    toPath = const "/"

instance ToQuery DescribeImportSnapshotTasks where
    toQuery DescribeImportSnapshotTasks{..} = mconcat
        [ "DryRun"       =? _distDryRun
        , "Filters"      `toQueryList` _distFilters
        , "ImportTaskId" `toQueryList` _distImportTaskIds
        , "MaxResults"   =? _distMaxResults
        , "NextToken"    =? _distNextToken
        ]

instance ToHeaders DescribeImportSnapshotTasks

instance AWSRequest DescribeImportSnapshotTasks where
    type Sv DescribeImportSnapshotTasks = EC2
    type Rs DescribeImportSnapshotTasks = DescribeImportSnapshotTasksResponse

    request  = post "DescribeImportSnapshotTasks"
    response = xmlResponse

instance FromXML DescribeImportSnapshotTasksResponse where
    parseXML x = DescribeImportSnapshotTasksResponse
        <$> x .@? "importSnapshotTaskSet" .!@ mempty
        <*> x .@? "nextToken"
