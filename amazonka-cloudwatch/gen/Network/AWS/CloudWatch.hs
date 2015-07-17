{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This is the /Amazon CloudWatch API Reference/. This guide provides
-- detailed information about Amazon CloudWatch actions, data types,
-- parameters, and errors. For detailed information about Amazon CloudWatch
-- features and their associated API calls, go to the
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide Amazon CloudWatch Developer Guide>.
--
-- Amazon CloudWatch is a web service that enables you to publish, monitor,
-- and manage various metrics, as well as configure alarm actions based on
-- data from metrics. For more information about this product go to
-- <http://aws.amazon.com/cloudwatch>.
--
-- For information about the namespace, metric names, and dimensions that
-- other Amazon Web Services products use to send metrics to Cloudwatch, go
-- to
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Metrics, Namespaces, and Dimensions Reference>
-- in the /Amazon CloudWatch Developer Guide/.
--
-- Use the following links to get started using the /Amazon CloudWatch API
-- Reference/:
--
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Amazon CloudWatch actions.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_Types.html Data Types>:
--     An alphabetical list of all Amazon CloudWatch data types.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonParameters.html Common Parameters>:
--     Parameters that all Query actions can use.
-- -   <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/CommonErrors.html Common Errors>:
--     Client and server errors that all actions can return.
-- -   <http://docs.aws.amazon.com/general/latest/gr/index.html?rande.html Regions and Endpoints>:
--     Itemized regions and endpoints for all AWS products.
-- -   <http://monitoring.amazonaws.com/doc/2010-08-01/CloudWatch.wsdl WSDL Location>:
--     http:\/\/monitoring.amazonaws.com\/doc\/2010-08-01\/CloudWatch.wsdl
--
-- In addition to using the Amazon CloudWatch API, you can also use the
-- following SDKs and third-party libraries to access Amazon CloudWatch
-- programmatically.
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
module Network.AWS.CloudWatch
    ( module Export
    ) where

import           Network.AWS.CloudWatch.DeleteAlarms            as Export
import           Network.AWS.CloudWatch.DescribeAlarmHistory    as Export
import           Network.AWS.CloudWatch.DescribeAlarms          as Export
import           Network.AWS.CloudWatch.DescribeAlarmsForMetric as Export
import           Network.AWS.CloudWatch.DisableAlarmActions     as Export
import           Network.AWS.CloudWatch.EnableAlarmActions      as Export
import           Network.AWS.CloudWatch.GetMetricStatistics     as Export
import           Network.AWS.CloudWatch.ListMetrics             as Export
import           Network.AWS.CloudWatch.PutMetricAlarm          as Export
import           Network.AWS.CloudWatch.PutMetricData           as Export
import           Network.AWS.CloudWatch.SetAlarmState           as Export
import           Network.AWS.CloudWatch.Types                   as Export
import           Network.AWS.CloudWatch.Types.Product           as Export
import           Network.AWS.CloudWatch.Types.Sum               as Export
import           Network.AWS.CloudWatch.Waiters                 as Export
