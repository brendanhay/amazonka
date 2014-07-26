{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.ReportInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Submits feedback about the status of an instance. The instance must be in
-- the running state. If your experience with the instance differs from the
-- instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
-- Example 1 This example reports instance health state for two instances.
-- https://ec2.amazonaws.com/?Action=ReportInstanceStatus &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63 &amp;AUTHPARAMS
-- Example 2 This example reports instance health state for two instances with
-- reason codes. https://ec2.amazonaws.com/?Action=ReportInstanceStatus
-- &amp;Description=Description+of+my+issue. &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63
-- &amp;ReasonCode.0=instance-performance-network
-- &amp;ReasonCode.1=instance-performance-disk &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_05_01.ReportInstanceStatus where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data ReportInstanceStatus = ReportInstanceStatus
    { _risrDryRun :: Bool
      -- ^ 
    , _risrStartTime :: ISO8601
      -- ^ The time at which the reported instance health state began.
    , _risrEndTime :: ISO8601
      -- ^ The time at which the reported instance health state ended.
    , _risrInstances :: [Text]
      -- ^ One or more instances.
    , _risrReasonCodes :: [ReportInstanceReasonCodes]
      -- ^ One or more reason codes that describes the health state of your
      -- instance. instance-stuck-in-state: My instance is stuck in a
      -- state. unresponsive: My instance is unresponsive.
      -- not-accepting-credentials: My instance is not accepting my
      -- credentials. password-not-available: A password is not available
      -- for my instance. performance-network: My instance is experiencing
      -- performance problems which I believe are network related.
      -- performance-instance-store: My instance is experiencing
      -- performance problems which I believe are related to the instance
      -- stores. performance-ebs-volume: My instance is experiencing
      -- performance problems which I believe are related to an EBS
      -- volume. performance-other: My instance is experiencing
      -- performance problems. other: [explain using the description
      -- parameter].
    , _risrStatus :: ReportStatusType
      -- ^ The status of all instances listed.
    , _risrDescription :: Text
      -- ^ Descriptive text about the health state of your instance.
    } deriving (Generic)

instance ToQuery ReportInstanceStatus where
    toQuery = genericToQuery def

instance AWSRequest ReportInstanceStatus where
    type Sv ReportInstanceStatus = EC2
    type Rs ReportInstanceStatus = ReportInstanceStatusResponse

    request = post "ReportInstanceStatus"

    response _ _ = return (Right ReportInstanceStatusResponse)

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    deriving (Eq, Show, Generic)
