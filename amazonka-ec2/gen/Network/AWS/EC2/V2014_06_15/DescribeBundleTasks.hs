{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeBundleTasks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of your bundling tasks. Completed bundle tasks are
-- listed for only a limited time. If your bundle task is no longer in the
-- list, you can still register an AMI from it. Just use RegisterImage with
-- the Amazon S3 bucket name and image manifest name you provided to the
-- bundle task. Example 1 This example describes the status of the specified
-- bundle task. https://ec2.amazonaws.com/?Action=DescribeBundleTasks
-- &amp;bundleId.1=bun-c1a540a8 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE i-12345678 bun-c1a540a8 cancelling
-- 2008-10-07T11:41:50.000Z 2008-10-07T11:51:50.000Z myawsbucket winami 20%
-- Example 2 This example filters the response to include only bundle tasks
-- whose state is either complete or failed, and in addition are targeted for
-- the Amazon S3 bucket named myawsbucket.
-- https://ec2.amazonaws.com/?Action=DescribeBundleTasks
-- &amp;Filter.1.Name=s3-bucket &amp;Filter.1.Value.1=myawsbucket
-- &amp;Filter.2.Name=state &amp;Filter.2.Name.1=complete
-- &amp;Filter.2.Name.2=failed &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.DescribeBundleTasks
    (
    -- * Request
      DescribeBundleTasks
    -- ** Request constructor
    , describeBundleTasks
    -- ** Request lenses
    , dbtrBundleIds
    , dbtrFilters

    -- * Response
    , DescribeBundleTasksResponse
    -- ** Response lenses
    , dbtsBundleTasks
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeBundleTasks' request.
describeBundleTasks :: DescribeBundleTasks
describeBundleTasks = DescribeBundleTasks
    { _dbtrBundleIds = mempty
    , _dbtrFilters = mempty
    }

data DescribeBundleTasks = DescribeBundleTasks
    { _dbtrBundleIds :: [Text]
      -- ^ One or more bundle task IDs. Default: Describes all your bundle
      -- tasks.
    , _dbtrFilters :: [Filter]
      -- ^ One or more filters. bundle-id - The ID of the bundle task.
      -- error-code - If the task failed, the error code returned.
      -- error-message - If the task failed, the error message returned.
      -- instance-id - The ID of the instance. progress - The level of
      -- task completion, as a percentage (for example, 20%). s3-bucket -
      -- The Amazon S3 bucket to store the AMI. s3-prefix - The beginning
      -- of the AMI name. start-time - The time the task started (for
      -- example, 2013-09-15T17:15:20.000Z). state - The state of the task
      -- (pending | waiting-for-shutdown | bundling | storing | cancelling
      -- | complete | failed). update-time - The time of the most recent
      -- update for the task.
    } deriving (Show, Generic)

-- | One or more bundle task IDs. Default: Describes all your bundle tasks.
dbtrBundleIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeBundleTasks
    -> f DescribeBundleTasks
dbtrBundleIds f x =
    (\y -> x { _dbtrBundleIds = y })
       <$> f (_dbtrBundleIds x)
{-# INLINE dbtrBundleIds #-}

-- | One or more filters. bundle-id - The ID of the bundle task. error-code - If
-- the task failed, the error code returned. error-message - If the task
-- failed, the error message returned. instance-id - The ID of the instance.
-- progress - The level of task completion, as a percentage (for example,
-- 20%). s3-bucket - The Amazon S3 bucket to store the AMI. s3-prefix - The
-- beginning of the AMI name. start-time - The time the task started (for
-- example, 2013-09-15T17:15:20.000Z). state - The state of the task (pending
-- | waiting-for-shutdown | bundling | storing | cancelling | complete |
-- failed). update-time - The time of the most recent update for the task.
dbtrFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeBundleTasks
    -> f DescribeBundleTasks
dbtrFilters f x =
    (\y -> x { _dbtrFilters = y })
       <$> f (_dbtrFilters x)
{-# INLINE dbtrFilters #-}

instance ToQuery DescribeBundleTasks where
    toQuery = genericQuery def

data DescribeBundleTasksResponse = DescribeBundleTasksResponse
    { _dbtsBundleTasks :: [BundleTask]
      -- ^ Information about one or more bundle tasks.
    } deriving (Show, Generic)

-- | Information about one or more bundle tasks.
dbtsBundleTasks
    :: Functor f
    => ([BundleTask]
    -> f ([BundleTask]))
    -> DescribeBundleTasksResponse
    -> f DescribeBundleTasksResponse
dbtsBundleTasks f x =
    (\y -> x { _dbtsBundleTasks = y })
       <$> f (_dbtsBundleTasks x)
{-# INLINE dbtsBundleTasks #-}

instance FromXML DescribeBundleTasksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeBundleTasks where
    type Sv DescribeBundleTasks = EC2
    type Rs DescribeBundleTasks = DescribeBundleTasksResponse

    request = post "DescribeBundleTasks"
    response _ = xmlResponse
