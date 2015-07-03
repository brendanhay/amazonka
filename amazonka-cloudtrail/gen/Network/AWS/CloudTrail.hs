-- Module      : Network.AWS.CloudTrail
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

-- | AWS CloudTrail
--
-- This is the CloudTrail API Reference. It provides descriptions of
-- actions, data types, common parameters, and common errors for
-- CloudTrail.
--
-- CloudTrail is a web service that records AWS API calls for your AWS
-- account and delivers log files to an Amazon S3 bucket. The recorded
-- information includes the identity of the user, the start time of the AWS
-- API call, the source IP address, the request parameters, and the
-- response elements returned by the service.
--
-- As an alternative to using the API, you can use one of the AWS SDKs,
-- which consist of libraries and sample code for various programming
-- languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs
-- provide a convenient way to create programmatic access to AWSCloudTrail.
-- For example, the SDKs take care of cryptographically signing requests,
-- managing errors, and retrying requests automatically. For information
-- about the AWS SDKs, including how to download and install them, see the
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services page>.
--
-- See the CloudTrail User Guide for information about the data that is
-- included with each AWS API call listed in the log files.
module Network.AWS.CloudTrail
    ( module Export
    ) where

import           Network.AWS.CloudTrail.CreateTrail    as Export
import           Network.AWS.CloudTrail.DeleteTrail    as Export
import           Network.AWS.CloudTrail.DescribeTrails as Export
import           Network.AWS.CloudTrail.GetTrailStatus as Export
import           Network.AWS.CloudTrail.LookupEvents   as Export
import           Network.AWS.CloudTrail.StartLogging   as Export
import           Network.AWS.CloudTrail.StopLogging    as Export
import           Network.AWS.CloudTrail.Types          as Export
import           Network.AWS.CloudTrail.UpdateTrail    as Export
import           Network.AWS.CloudTrail.Waiters        as Export
