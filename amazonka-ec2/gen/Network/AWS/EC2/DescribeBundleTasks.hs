{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more of your bundling tasks.
--
-- Completed bundle tasks are listed for only a limited time. If your
-- bundle task is no longer in the list, you can still register an AMI from
-- it. Just use @RegisterImage@ with the Amazon S3 bucket name and image
-- manifest name you provided to the bundle task.
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
    , dbtFilters
    , dbtDryRun

    -- * Response
    , DescribeBundleTasksResponse
    -- ** Response constructor
    , describeBundleTasksResponse
    -- ** Response lenses
    , dbtrBundleTasks
    , dbtrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeBundleTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtBundleIds'
--
-- * 'dbtFilters'
--
-- * 'dbtDryRun'
data DescribeBundleTasks = DescribeBundleTasks'
    { _dbtBundleIds :: !(Maybe [Text])
    , _dbtFilters   :: !(Maybe [Filter])
    , _dbtDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeBundleTasks' smart constructor.
describeBundleTasks :: DescribeBundleTasks
describeBundleTasks =
    DescribeBundleTasks'
    { _dbtBundleIds = Nothing
    , _dbtFilters = Nothing
    , _dbtDryRun = Nothing
    }

-- | One or more bundle task IDs.
--
-- Default: Describes all your bundle tasks.
dbtBundleIds :: Lens' DescribeBundleTasks [Text]
dbtBundleIds = lens _dbtBundleIds (\ s a -> s{_dbtBundleIds = a}) . _Default;

-- | One or more filters.
--
-- -   @bundle-id@ - The ID of the bundle task.
--
-- -   @error-code@ - If the task failed, the error code returned.
--
-- -   @error-message@ - If the task failed, the error message returned.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @progress@ - The level of task completion, as a percentage (for
--     example, 20%).
--
-- -   @s3-bucket@ - The Amazon S3 bucket to store the AMI.
--
-- -   @s3-prefix@ - The beginning of the AMI name.
--
-- -   @start-time@ - The time the task started (for example,
--     2013-09-15T17:15:20.000Z).
--
-- -   @state@ - The state of the task (@pending@ | @waiting-for-shutdown@
--     | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@).
--
-- -   @update-time@ - The time of the most recent update for the task.
--
dbtFilters :: Lens' DescribeBundleTasks [Filter]
dbtFilters = lens _dbtFilters (\ s a -> s{_dbtFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dbtDryRun :: Lens' DescribeBundleTasks (Maybe Bool)
dbtDryRun = lens _dbtDryRun (\ s a -> s{_dbtDryRun = a});

instance AWSRequest DescribeBundleTasks where
        type Sv DescribeBundleTasks = EC2
        type Rs DescribeBundleTasks =
             DescribeBundleTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeBundleTasksResponse' <$>
                   (x .@? "bundleInstanceTasksSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeBundleTasks where
        toHeaders = const mempty

instance ToPath DescribeBundleTasks where
        toPath = const "/"

instance ToQuery DescribeBundleTasks where
        toQuery DescribeBundleTasks'{..}
          = mconcat
              ["Action" =: ("DescribeBundleTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "BundleId" <$> _dbtBundleIds),
               toQuery (toQueryList "Filter" <$> _dbtFilters),
               "DryRun" =: _dbtDryRun]

-- | /See:/ 'describeBundleTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtrBundleTasks'
--
-- * 'dbtrStatus'
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
    { _dbtrBundleTasks :: !(Maybe [BundleTask])
    , _dbtrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeBundleTasksResponse' smart constructor.
describeBundleTasksResponse :: Int -> DescribeBundleTasksResponse
describeBundleTasksResponse pStatus =
    DescribeBundleTasksResponse'
    { _dbtrBundleTasks = Nothing
    , _dbtrStatus = pStatus
    }

-- | Information about one or more bundle tasks.
dbtrBundleTasks :: Lens' DescribeBundleTasksResponse [BundleTask]
dbtrBundleTasks = lens _dbtrBundleTasks (\ s a -> s{_dbtrBundleTasks = a}) . _Default;

-- | FIXME: Undocumented member.
dbtrStatus :: Lens' DescribeBundleTasksResponse Int
dbtrStatus = lens _dbtrStatus (\ s a -> s{_dbtrStatus = a});
