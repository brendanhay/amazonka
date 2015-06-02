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

-- Module      : Network.AWS.EC2.DescribeImportImageTasks
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

-- | Displays details about an import virtual machine or import snapshot tasks
-- that are already created.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportImageTasks.html>
module Network.AWS.EC2.DescribeImportImageTasks
    (
    -- * Request
      DescribeImportImageTasks
    -- ** Request constructor
    , describeImportImageTasks
    -- ** Request lenses
    , diitDryRun
    , diitFilters
    , diitImportTaskIds
    , diitMaxResults
    , diitNextToken

    -- * Response
    , DescribeImportImageTasksResponse
    -- ** Response constructor
    , describeImportImageTasksResponse
    -- ** Response lenses
    , diitrImportImageTasks
    , diitrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeImportImageTasks = DescribeImportImageTasks
    { _diitDryRun        :: Maybe Bool
    , _diitFilters       :: List "Filter" Filter
    , _diitImportTaskIds :: List "ImportTaskId" Text
    , _diitMaxResults    :: Maybe Int
    , _diitNextToken     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeImportImageTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'diitFilters' @::@ ['Filter']
--
-- * 'diitImportTaskIds' @::@ ['Text']
--
-- * 'diitMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'diitNextToken' @::@ 'Maybe' 'Text'
--
describeImportImageTasks :: DescribeImportImageTasks
describeImportImageTasks = DescribeImportImageTasks
    { _diitDryRun        = Nothing
    , _diitImportTaskIds = mempty
    , _diitNextToken     = Nothing
    , _diitMaxResults    = Nothing
    , _diitFilters       = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
diitDryRun :: Lens' DescribeImportImageTasks (Maybe Bool)
diitDryRun = lens _diitDryRun (\s a -> s { _diitDryRun = a })

-- | One or more filters.
diitFilters :: Lens' DescribeImportImageTasks [Filter]
diitFilters = lens _diitFilters (\s a -> s { _diitFilters = a }) . _List

-- | A list of import image task IDs.
diitImportTaskIds :: Lens' DescribeImportImageTasks [Text]
diitImportTaskIds =
    lens _diitImportTaskIds (\s a -> s { _diitImportTaskIds = a })
        . _List

-- | The maximum number of results to return in a single request.
diitMaxResults :: Lens' DescribeImportImageTasks (Maybe Int)
diitMaxResults = lens _diitMaxResults (\s a -> s { _diitMaxResults = a })

-- | A token that indicates the next page of results.
diitNextToken :: Lens' DescribeImportImageTasks (Maybe Text)
diitNextToken = lens _diitNextToken (\s a -> s { _diitNextToken = a })

data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse
    { _diitrImportImageTasks :: List "item" ImportImageTask
    , _diitrNextToken        :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeImportImageTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitrImportImageTasks' @::@ ['ImportImageTask']
--
-- * 'diitrNextToken' @::@ 'Maybe' 'Text'
--
describeImportImageTasksResponse :: DescribeImportImageTasksResponse
describeImportImageTasksResponse = DescribeImportImageTasksResponse
    { _diitrImportImageTasks = mempty
    , _diitrNextToken        = Nothing
    }

-- | A list of zero or more import image tasks that are currently active or were
-- completed or canceled in the previous 7 days.
diitrImportImageTasks :: Lens' DescribeImportImageTasksResponse [ImportImageTask]
diitrImportImageTasks =
    lens _diitrImportImageTasks (\s a -> s { _diitrImportImageTasks = a })
        . _List

-- | The token to use to get the next page of results. This value is 'null' when
-- there are no more results to return.
diitrNextToken :: Lens' DescribeImportImageTasksResponse (Maybe Text)
diitrNextToken = lens _diitrNextToken (\s a -> s { _diitrNextToken = a })

instance ToPath DescribeImportImageTasks where
    toPath = const "/"

instance ToQuery DescribeImportImageTasks where
    toQuery DescribeImportImageTasks{..} = mconcat
        [ "DryRun"       =? _diitDryRun
        , "Filters"      `toQueryList` _diitFilters
        , "ImportTaskId" `toQueryList` _diitImportTaskIds
        , "MaxResults"   =? _diitMaxResults
        , "NextToken"    =? _diitNextToken
        ]

instance ToHeaders DescribeImportImageTasks

instance AWSRequest DescribeImportImageTasks where
    type Sv DescribeImportImageTasks = EC2
    type Rs DescribeImportImageTasks = DescribeImportImageTasksResponse

    request  = post "DescribeImportImageTasks"
    response = xmlResponse

instance FromXML DescribeImportImageTasksResponse where
    parseXML x = DescribeImportImageTasksResponse
        <$> x .@? "importImageTaskSet" .!@ mempty
        <*> x .@? "nextToken"
