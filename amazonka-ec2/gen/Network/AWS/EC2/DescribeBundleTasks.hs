{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeBundleTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your bundling tasks.
--
--
module Network.AWS.EC2.DescribeBundleTasks
    (
    -- * Creating a Request
      describeBundleTasks
    , DescribeBundleTasks
    -- * Request Lenses
    , dbtBundleIds
    , dbtFilters
    , dbtDryRun

    -- * Destructuring the Response
    , describeBundleTasksResponse
    , DescribeBundleTasksResponse
    -- * Response Lenses
    , dbtrsBundleTasks
    , dbtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeBundleTasks.
--
--
--
-- /See:/ 'describeBundleTasks' smart constructor.
data DescribeBundleTasks = DescribeBundleTasks'
  { _dbtBundleIds :: !(Maybe [Text])
  , _dbtFilters   :: !(Maybe [Filter])
  , _dbtDryRun    :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBundleTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbtBundleIds' - One or more bundle task IDs. Default: Describes all your bundle tasks.
--
-- * 'dbtFilters' - One or more filters.     * @bundle-id@ - The ID of the bundle task.     * @error-code@ - If the task failed, the error code returned.     * @error-message@ - If the task failed, the error message returned.     * @instance-id@ - The ID of the instance.     * @progress@ - The level of task completion, as a percentage (for example, 20%).     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.     * @s3-prefix@ - The beginning of the AMI name.     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).     * @update-time@ - The time of the most recent update for the task.
--
-- * 'dbtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeBundleTasks
    :: DescribeBundleTasks
describeBundleTasks =
  DescribeBundleTasks'
    {_dbtBundleIds = Nothing, _dbtFilters = Nothing, _dbtDryRun = Nothing}


-- | One or more bundle task IDs. Default: Describes all your bundle tasks.
dbtBundleIds :: Lens' DescribeBundleTasks [Text]
dbtBundleIds = lens _dbtBundleIds (\ s a -> s{_dbtBundleIds = a}) . _Default . _Coerce

-- | One or more filters.     * @bundle-id@ - The ID of the bundle task.     * @error-code@ - If the task failed, the error code returned.     * @error-message@ - If the task failed, the error message returned.     * @instance-id@ - The ID of the instance.     * @progress@ - The level of task completion, as a percentage (for example, 20%).     * @s3-bucket@ - The Amazon S3 bucket to store the AMI.     * @s3-prefix@ - The beginning of the AMI name.     * @start-time@ - The time the task started (for example, 2013-09-15T17:15:20.000Z).     * @state@ - The state of the task (@pending@ | @waiting-for-shutdown@ | @bundling@ | @storing@ | @cancelling@ | @complete@ | @failed@ ).     * @update-time@ - The time of the most recent update for the task.
dbtFilters :: Lens' DescribeBundleTasks [Filter]
dbtFilters = lens _dbtFilters (\ s a -> s{_dbtFilters = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dbtDryRun :: Lens' DescribeBundleTasks (Maybe Bool)
dbtDryRun = lens _dbtDryRun (\ s a -> s{_dbtDryRun = a})

instance AWSRequest DescribeBundleTasks where
        type Rs DescribeBundleTasks =
             DescribeBundleTasksResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeBundleTasksResponse' <$>
                   (x .@? "bundleInstanceTasksSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBundleTasks where

instance NFData DescribeBundleTasks where

instance ToHeaders DescribeBundleTasks where
        toHeaders = const mempty

instance ToPath DescribeBundleTasks where
        toPath = const "/"

instance ToQuery DescribeBundleTasks where
        toQuery DescribeBundleTasks'{..}
          = mconcat
              ["Action" =: ("DescribeBundleTasks" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "BundleId" <$> _dbtBundleIds),
               toQuery (toQueryList "Filter" <$> _dbtFilters),
               "DryRun" =: _dbtDryRun]

-- | Contains the output of DescribeBundleTasks.
--
--
--
-- /See:/ 'describeBundleTasksResponse' smart constructor.
data DescribeBundleTasksResponse = DescribeBundleTasksResponse'
  { _dbtrsBundleTasks    :: !(Maybe [BundleTask])
  , _dbtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBundleTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbtrsBundleTasks' - Information about one or more bundle tasks.
--
-- * 'dbtrsResponseStatus' - -- | The response status code.
describeBundleTasksResponse
    :: Int -- ^ 'dbtrsResponseStatus'
    -> DescribeBundleTasksResponse
describeBundleTasksResponse pResponseStatus_ =
  DescribeBundleTasksResponse'
    {_dbtrsBundleTasks = Nothing, _dbtrsResponseStatus = pResponseStatus_}


-- | Information about one or more bundle tasks.
dbtrsBundleTasks :: Lens' DescribeBundleTasksResponse [BundleTask]
dbtrsBundleTasks = lens _dbtrsBundleTasks (\ s a -> s{_dbtrsBundleTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
dbtrsResponseStatus :: Lens' DescribeBundleTasksResponse Int
dbtrsResponseStatus = lens _dbtrsResponseStatus (\ s a -> s{_dbtrsResponseStatus = a})

instance NFData DescribeBundleTasksResponse where
