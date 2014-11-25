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

-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your bundling tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeBundleTasks.html>
module Network.AWS.EC2.DescribeBundleTasks
    (
    -- * Request
      DescribeBundleTasks
    -- ** Request constructor
    , describeBundleTasks
    -- ** Request lenses
    , dbtBundleIds
    , dbtDryRun
    , dbtFilters

    -- * Response
    , DescribeBundleTasksResponse
    -- ** Response constructor
    , describeBundleTasksResponse
    -- ** Response lenses
    , dbtrBundleTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeBundleTasks = DescribeBundleTasks
    { _dbtBundleIds :: List "BundleId" Text
    , _dbtDryRun    :: Maybe Bool
    , _dbtFilters   :: List "Filter" Filter
    } deriving (Eq, Show)

-- | 'DescribeBundleTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtBundleIds' @::@ ['Text']
--
-- * 'dbtDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dbtFilters' @::@ ['Filter']
--
describeBundleTasks :: DescribeBundleTasks
describeBundleTasks = DescribeBundleTasks
    { _dbtDryRun    = Nothing
    , _dbtBundleIds = mempty
    , _dbtFilters   = mempty
    }

-- | One or more bundle task IDs.
--
-- Default: Describes all your bundle tasks.
dbtBundleIds :: Lens' DescribeBundleTasks [Text]
dbtBundleIds = lens _dbtBundleIds (\s a -> s { _dbtBundleIds = a }) . _List

dbtDryRun :: Lens' DescribeBundleTasks (Maybe Bool)
dbtDryRun = lens _dbtDryRun (\s a -> s { _dbtDryRun = a })

-- | One or more filters.
--
-- 'bundle-id' - The ID of the bundle task.
--
-- 'error-code' - If the task failed, the error code returned.
--
-- 'error-message' - If the task failed, the error message returned.
--
-- 'instance-id' - The ID of the instance.
--
-- 'progress' - The level of task completion, as a percentage (for example,
-- 20%).
--
-- 's3-bucket' - The Amazon S3 bucket to store the AMI.
--
-- 's3-prefix' - The beginning of the AMI name.
--
-- 'start-time' - The time the task started (for example,
-- 2013-09-15T17:15:20.000Z).
--
-- 'state' - The state of the task ('pending' | 'waiting-for-shutdown' | 'bundling' | 'storing' | 'cancelling' | 'complete' | 'failed').
--
-- 'update-time' - The time of the most recent update for the task.
--
--
dbtFilters :: Lens' DescribeBundleTasks [Filter]
dbtFilters = lens _dbtFilters (\s a -> s { _dbtFilters = a }) . _List

newtype DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { _dbtrBundleTasks :: List "item" BundleTask
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeBundleTasksResponse where
    type Item DescribeBundleTasksResponse = BundleTask

    fromList = DescribeBundleTasksResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dbtrBundleTasks

-- | 'DescribeBundleTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtrBundleTasks' @::@ ['BundleTask']
--
describeBundleTasksResponse :: DescribeBundleTasksResponse
describeBundleTasksResponse = DescribeBundleTasksResponse
    { _dbtrBundleTasks = mempty
    }

-- | Information about one or more bundle tasks.
dbtrBundleTasks :: Lens' DescribeBundleTasksResponse [BundleTask]
dbtrBundleTasks = lens _dbtrBundleTasks (\s a -> s { _dbtrBundleTasks = a }) . _List

instance ToPath DescribeBundleTasks where
    toPath = const "/"

instance ToQuery DescribeBundleTasks where
    toQuery DescribeBundleTasks{..} = mconcat
        [ "BundleId" =? _dbtBundleIds
        , "dryRun"   =? _dbtDryRun
        , "Filter"   =? _dbtFilters
        ]

instance ToHeaders DescribeBundleTasks

instance AWSRequest DescribeBundleTasks where
    type Sv DescribeBundleTasks = EC2
    type Rs DescribeBundleTasks = DescribeBundleTasksResponse

    request  = post "DescribeBundleTasks"
    response = xmlResponse

instance FromXML DescribeBundleTasksResponse where
    parseXML x = DescribeBundleTasksResponse
        <$> x .@  "bundleInstanceTasksSet"
