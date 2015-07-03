-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon CloudWatch Logs API Reference
--
-- This is the /Amazon CloudWatch Logs API Reference/. Amazon CloudWatch
-- Logs enables you to monitor, store, and access your system, application,
-- and custom log files. This guide provides detailed information about
-- Amazon CloudWatch Logs actions, data types, parameters, and errors. For
-- detailed information about Amazon CloudWatch Logs features and their
-- associated API calls, go to the
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.
--
-- Use the following links to get started using the /Amazon CloudWatch Logs
-- API Reference/:
--
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Amazon CloudWatch Logs actions.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_Types.html Data Types>:
--     An alphabetical list of all Amazon CloudWatch Logs data types.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/CommonParameters.html Common Parameters>:
--     Parameters that all Query actions can use.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/CommonErrors.html Common Errors>:
--     Client and server errors that all actions can return.
-- -   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html Regions and Endpoints>:
--     Itemized regions and endpoints for all AWS products.
--
-- In addition to using the Amazon CloudWatch Logs API, you can also use
-- the following SDKs and third-party libraries to access Amazon CloudWatch
-- Logs programmatically.
--
-- -   <http://aws.amazon.com/documentation/sdkforjava/ AWS SDK for Java Documentation>
-- -   <http://aws.amazon.com/documentation/sdkfornet/ AWS SDK for .NET Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforphp/ AWS SDK for PHP Documentation>
-- -   <http://aws.amazon.com/documentation/sdkforruby/ AWS SDK for Ruby Documentation>
--
-- Developers in the AWS developer community also provide their own
-- libraries, which you can find at the following AWS developer centers:
--
-- -   <http://aws.amazon.com/java/ AWS Java Developer Center>
-- -   <http://aws.amazon.com/php/ AWS PHP Developer Center>
-- -   <http://aws.amazon.com/python/ AWS Python Developer Center>
-- -   <http://aws.amazon.com/ruby/ AWS Ruby Developer Center>
-- -   <http://aws.amazon.com/net/ AWS Windows and .NET Developer Center>
module Network.AWS.CloudWatchLogs
    ( module Export
    ) where

import           Network.AWS.CloudWatchLogs.CreateLogGroup              as Export
import           Network.AWS.CloudWatchLogs.CreateLogStream             as Export
import           Network.AWS.CloudWatchLogs.DeleteLogGroup              as Export
import           Network.AWS.CloudWatchLogs.DeleteLogStream             as Export
import           Network.AWS.CloudWatchLogs.DeleteMetricFilter          as Export
import           Network.AWS.CloudWatchLogs.DeleteRetentionPolicy       as Export
import           Network.AWS.CloudWatchLogs.DeleteSubscriptionFilter    as Export
import           Network.AWS.CloudWatchLogs.DescribeLogGroups           as Export
import           Network.AWS.CloudWatchLogs.DescribeLogStreams          as Export
import           Network.AWS.CloudWatchLogs.DescribeMetricFilters       as Export
import           Network.AWS.CloudWatchLogs.DescribeSubscriptionFilters as Export
import           Network.AWS.CloudWatchLogs.FilterLogEvents             as Export
import           Network.AWS.CloudWatchLogs.GetLogEvents                as Export
import           Network.AWS.CloudWatchLogs.PutLogEvents                as Export
import           Network.AWS.CloudWatchLogs.PutMetricFilter             as Export
import           Network.AWS.CloudWatchLogs.PutRetentionPolicy          as Export
import           Network.AWS.CloudWatchLogs.PutSubscriptionFilter       as Export
import           Network.AWS.CloudWatchLogs.TestMetricFilter            as Export
import           Network.AWS.CloudWatchLogs.Types                       as Export
import           Network.AWS.CloudWatchLogs.Waiters                     as Export
