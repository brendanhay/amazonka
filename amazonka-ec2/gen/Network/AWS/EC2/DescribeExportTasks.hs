{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeExportTasks
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

-- | Describes one or more of your export tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Request
      DescribeExportTasks
    -- ** Request constructor
    , describeExportTasks
    -- ** Request lenses
    , detExportTaskIds

    -- * Response
    , DescribeExportTasksResponse
    -- ** Response constructor
    , describeExportTasksResponse
    -- ** Response lenses
    , detrExportTasks
    , detrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detExportTaskIds'
newtype DescribeExportTasks = DescribeExportTasks'
    { _detExportTaskIds :: Maybe [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeExportTasks' smart constructor.
describeExportTasks :: DescribeExportTasks
describeExportTasks =
    DescribeExportTasks'
    { _detExportTaskIds = Nothing
    }

-- | One or more export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds = lens _detExportTaskIds (\ s a -> s{_detExportTaskIds = a}) . _Default;

instance AWSRequest DescribeExportTasks where
        type Sv DescribeExportTasks = EC2
        type Rs DescribeExportTasks =
             DescribeExportTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeExportTasksResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeExportTasks where
        toHeaders = const mempty

instance ToPath DescribeExportTasks where
        toPath = const "/"

instance ToQuery DescribeExportTasks where
        toQuery DescribeExportTasks'{..}
          = mconcat
              ["Action" =: ("DescribeExportTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "ExportTaskId" <$> _detExportTaskIds)]

-- | /See:/ 'describeExportTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detrExportTasks'
--
-- * 'detrStatus'
data DescribeExportTasksResponse = DescribeExportTasksResponse'
    { _detrExportTasks :: Maybe [ExportTask]
    , _detrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeExportTasksResponse' smart constructor.
describeExportTasksResponse :: Int -> DescribeExportTasksResponse
describeExportTasksResponse pStatus =
    DescribeExportTasksResponse'
    { _detrExportTasks = Nothing
    , _detrStatus = pStatus
    }

-- | Information about the export tasks.
detrExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrExportTasks = lens _detrExportTasks (\ s a -> s{_detrExportTasks = a}) . _Default;

-- | FIXME: Undocumented member.
detrStatus :: Lens' DescribeExportTasksResponse Int
detrStatus = lens _detrStatus (\ s a -> s{_detrStatus = a});
