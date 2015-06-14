{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeConversionTasks
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

-- | Describes one or more of your conversion tasks. For more information,
-- see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeConversionTasks.html>
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Request
      DescribeConversionTasks
    -- ** Request constructor
    , describeConversionTasks
    -- ** Request lenses
    , dctConversionTaskIds
    , dctFilters
    , dctDryRun

    -- * Response
    , DescribeConversionTasksResponse
    -- ** Response constructor
    , describeConversionTasksResponse
    -- ** Response lenses
    , dctrConversionTasks
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeConversionTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctConversionTaskIds'
--
-- * 'dctFilters'
--
-- * 'dctDryRun'
data DescribeConversionTasks = DescribeConversionTasks'{_dctConversionTaskIds :: Maybe [Text], _dctFilters :: Maybe [Filter], _dctDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DescribeConversionTasks' smart constructor.
describeConversionTasks :: DescribeConversionTasks
describeConversionTasks = DescribeConversionTasks'{_dctConversionTaskIds = Nothing, _dctFilters = Nothing, _dctDryRun = Nothing};

-- | One or more conversion task IDs.
dctConversionTaskIds :: Lens' DescribeConversionTasks (Maybe [Text])
dctConversionTaskIds = lens _dctConversionTaskIds (\ s a -> s{_dctConversionTaskIds = a});

-- | One or more filters.
dctFilters :: Lens' DescribeConversionTasks (Maybe [Filter])
dctFilters = lens _dctFilters (\ s a -> s{_dctFilters = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dctDryRun :: Lens' DescribeConversionTasks (Maybe Bool)
dctDryRun = lens _dctDryRun (\ s a -> s{_dctDryRun = a});

instance AWSRequest DescribeConversionTasks where
        type Sv DescribeConversionTasks = EC2
        type Rs DescribeConversionTasks =
             DescribeConversionTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeConversionTasksResponse' <$>
                   parseXMLList "item" x)

instance ToHeaders DescribeConversionTasks where
        toHeaders = const mempty

instance ToPath DescribeConversionTasks where
        toPath = const "/"

instance ToQuery DescribeConversionTasks where
        toQuery DescribeConversionTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConversionTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "item" =: _dctConversionTaskIds,
               "Filter" =: _dctFilters, "DryRun" =: _dctDryRun]

-- | /See:/ 'describeConversionTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dctrConversionTasks'
newtype DescribeConversionTasksResponse = DescribeConversionTasksResponse'{_dctrConversionTasks :: Maybe [ConversionTask]} deriving (Eq, Read, Show)

-- | 'DescribeConversionTasksResponse' smart constructor.
describeConversionTasksResponse :: DescribeConversionTasksResponse
describeConversionTasksResponse = DescribeConversionTasksResponse'{_dctrConversionTasks = Nothing};

-- | Information about the conversion tasks.
dctrConversionTasks :: Lens' DescribeConversionTasksResponse (Maybe [ConversionTask])
dctrConversionTasks = lens _dctrConversionTasks (\ s a -> s{_dctrConversionTasks = a});
