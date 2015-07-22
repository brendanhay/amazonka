{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your bundling tasks.
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
    , dbtrqBundleIds
    , dbtrqFilters
    , dbtrqDryRun

    -- * Response
    , DescribeBundleTasksResponse
    -- ** Response constructor
    , describeBundleTasksResponse
    -- ** Response lenses
    , dbtrsBundleTasks
    , dbtrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeBundleTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtrqBundleIds'
--
-- * 'dbtrqFilters'
--
-- * 'dbtrqDryRun'
data DescribeBundleTasks = DescribeBundleTasks'
    { _dbtrqBundleIds :: !(Maybe [Text])
    , _dbtrqFilters   :: !(Maybe [Filter])
    , _dbtrqDryRun    :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeBundleTasks' smart constructor.
describeBundleTasks :: DescribeBundleTasks
describeBundleTasks =
    DescribeBundleTasks'
    { _dbtrqBundleIds = Nothing
    , _dbtrqFilters = Nothing
    , _dbtrqDryRun = Nothing
    }

-- | One or more bundle task IDs.
--
-- Default: Describes all your bundle tasks.
dbtrqBundleIds :: Lens' DescribeBundleTasks [Text]
dbtrqBundleIds = lens _dbtrqBundleIds (\ s a -> s{_dbtrqBundleIds = a}) . _Default;

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
dbtrqFilters :: Lens' DescribeBundleTasks [Filter]
dbtrqFilters = lens _dbtrqFilters (\ s a -> s{_dbtrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dbtrqDryRun :: Lens' DescribeBundleTasks (Maybe Bool)
dbtrqDryRun = lens _dbtrqDryRun (\ s a -> s{_dbtrqDryRun = a});

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
               toQuery (toQueryList "BundleId" <$> _dbtrqBundleIds),
               toQuery (toQueryList "Filter" <$> _dbtrqFilters),
               "DryRun" =: _dbtrqDryRun]

-- | /See:/ 'describeBundleTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtrsBundleTasks'
--
-- * 'dbtrsStatus'
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
    { _dbtrsBundleTasks :: !(Maybe [BundleTask])
    , _dbtrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeBundleTasksResponse' smart constructor.
describeBundleTasksResponse :: Int -> DescribeBundleTasksResponse
describeBundleTasksResponse pStatus =
    DescribeBundleTasksResponse'
    { _dbtrsBundleTasks = Nothing
    , _dbtrsStatus = pStatus
    }

-- | Information about one or more bundle tasks.
dbtrsBundleTasks :: Lens' DescribeBundleTasksResponse [BundleTask]
dbtrsBundleTasks = lens _dbtrsBundleTasks (\ s a -> s{_dbtrsBundleTasks = a}) . _Default;

-- | FIXME: Undocumented member.
dbtrsStatus :: Lens' DescribeBundleTasksResponse Int
dbtrsStatus = lens _dbtrsStatus (\ s a -> s{_dbtrsStatus = a});
