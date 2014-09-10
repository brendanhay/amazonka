-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudWatch Logs enables you to monitor, store, and access your
-- system, application, and custom log files.
module Network.AWS.CloudWatchLogs
    ( module Network.AWS.CloudWatchLogs.CreateLogGroup
    , module Network.AWS.CloudWatchLogs.CreateLogStream
    , module Network.AWS.CloudWatchLogs.DeleteLogGroup
    , module Network.AWS.CloudWatchLogs.DeleteLogStream
    , module Network.AWS.CloudWatchLogs.DeleteMetricFilter
    , module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
    , module Network.AWS.CloudWatchLogs.DescribeLogGroups
    , module Network.AWS.CloudWatchLogs.DescribeLogStreams
    , module Network.AWS.CloudWatchLogs.DescribeMetricFilters
    , module Network.AWS.CloudWatchLogs.GetLogEvents
    , module Network.AWS.CloudWatchLogs.Monadic
    , module Network.AWS.CloudWatchLogs.PutLogEvents
    , module Network.AWS.CloudWatchLogs.PutMetricFilter
    , module Network.AWS.CloudWatchLogs.PutRetentionPolicy
    , module Network.AWS.CloudWatchLogs.TestMetricFilter
    , module Network.AWS.CloudWatchLogs.Types
    ) where

import Network.AWS.CloudWatchLogs.CreateLogGroup
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DeleteLogGroup
import Network.AWS.CloudWatchLogs.DeleteLogStream
import Network.AWS.CloudWatchLogs.DeleteMetricFilter
import Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
import Network.AWS.CloudWatchLogs.DescribeLogGroups
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.DescribeMetricFilters
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.Monadic
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.PutMetricFilter
import Network.AWS.CloudWatchLogs.PutRetentionPolicy
import Network.AWS.CloudWatchLogs.TestMetricFilter
import Network.AWS.CloudWatchLogs.Types
