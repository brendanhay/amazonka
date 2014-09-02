{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the status of one or more instances, including any scheduled
-- events. Instance status has two main components: System Status reports
-- impaired functionality that stems from issues related to the systems that
-- support an instance, such as such as hardware failures and network
-- connectivity problems. This call reports such problems as impaired
-- reachability. Instance Status reports impaired functionality that arises
-- from problems internal to the instance. This call reports such problems as
-- impaired reachability. Instance status provides information about four
-- types of scheduled events for an instance that may require your attention:
-- Scheduled Reboot: When Amazon EC2 determines that an instance must be
-- rebooted, the instances status returns one of two event codes:
-- system-reboot or instance-reboot. System reboot commonly occurs if certain
-- maintenance or upgrade operations require a reboot of the underlying host
-- that supports an instance. Instance reboot commonly occurs if the instance
-- must be rebooted, rather than the underlying host. Rebooting events include
-- a scheduled start and end time. System Maintenance: When Amazon EC2
-- determines that an instance requires maintenance that requires power or
-- network impact, the instance status is the event code system-maintenance.
-- System maintenance is either power maintenance or network maintenance. For
-- power maintenance, your instance will be unavailable for a brief period of
-- time and then rebooted. For network maintenance, your instance will
-- experience a brief loss of network connectivity. System maintenance events
-- include a scheduled start and end time. You will also be notified by email
-- if one of your instances is set for system maintenance. The email message
-- indicates when your instance is scheduled for maintenance. Scheduled
-- Retirement: When Amazon EC2 determines that an instance must be shut down,
-- the instance status is the event code instance-retirement. Retirement
-- commonly occurs when the underlying host is degraded and must be replaced.
-- Retirement events include a scheduled start and end time. You will also be
-- notified by email if one of your instances is set to retiring. The email
-- message indicates when your instance will be permanently retired. Scheduled
-- Stop: When Amazon EC2 determines that an instance must be shut down, the
-- instances status returns an event code called instance-stop. Stop events
-- include a scheduled start and end time. You will also be notified by email
-- if one of your instances is set to stop. The email message indicates when
-- your instance will be stopped. When your instance is retired, it will
-- either be terminated (if its root device type is the instance-store) or
-- stopped (if its root device type is an EBS volume). Instances stopped due
-- to retirement will not be restarted, but you can do so manually. You can
-- also avoid retirement of EBS-backed instances by manually restarting your
-- instance when its event code is instance-retirement. This ensures that your
-- instance is started on a different underlying host. For more information
-- about failed status checks, see Troubleshooting Instances with Failed
-- Status Checks in the Amazon Elastic Compute Cloud User Guide. For more
-- information about working with scheduled events, see Working with an
-- Instance That Has a Scheduled Event in the Amazon Elastic Compute Cloud
-- User Guide. Example 1 This example returns instance status descriptions for
-- all instances. https://ec2.amazonaws.com/? Action=DescribeInstanceStatus
-- &amp;AUTHPARAMS Example 2 This example returns instance status descriptions
-- for the specified instances. https://ec2.amazonaws.com/?
-- Action=DescribeInstanceStatus &amp;InstanceId.0=i-1a2b3c4d
-- &amp;InstanceId.1=i-2a2b3c4d &amp;AUTHPARAMS Example 3 This example returns
-- instance status descriptions for all instances specified by supported
-- DescribeInstanceStatus filters. https://ec2.amazonaws.com/?
-- Action=DescribeInstanceStatus &amp;Filter.0.Name=system-status.reachability
-- &amp;Filter.0.Value.failed &amp;AUTHPARAMS
-- 3be1508e-c444-4fef-89cc-0b1223c4f02fEXAMPLE i-1a2b3c4d us-east-1d 16
-- running impaired reachability failed YYYY-MM-DDTHH:MM:SS.000Z impaired
-- reachability failed YYYY-MM-DDTHH:MM:SS.000Z instance-retirement The
-- instance is running on degraded hardware YYYY-MM-DDTHH:MM:SS+0000
-- YYYY-MM-DDTHH:MM:SS+0000 i-2a2b3c4d us-east-1d 16 running ok reachability
-- passed ok reachability passed instance-reboot The instance is scheduled for
-- a reboot YYYY-MM-DDTHH:MM:SS+0000 YYYY-MM-DDTHH:MM:SS+0000 i-3a2b3c4d
-- us-east-1d 16 running ok reachability passed ok reachability passed
-- i-4a2b3c4d us-east-1d 16 running ok reachability passed insufficient-data
-- reachability insufficient-data.
module Network.AWS.EC2.V2014_06_15.DescribeInstanceStatus where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeInstanceStatus' request.
describeInstanceStatus :: DescribeInstanceStatus
describeInstanceStatus = DescribeInstanceStatus
    { _disrDryRun = Nothing
    , _disrIncludeAllInstances = Nothing
    , _disrFilters = mempty
    , _disrInstanceIds = mempty
    , _disrMaxResults = Nothing
    , _disrNextToken = Nothing
    }

data DescribeInstanceStatus = DescribeInstanceStatus
    { _disrDryRun :: Maybe Bool
      -- ^ 
    , _disrIncludeAllInstances :: Maybe Bool
      -- ^ When true, includes the health status for all instances. When
      -- false, includes the health status for running instances only.
      -- Default: false.
    , _disrFilters :: [Filter]
      -- ^ One or more filters. availability-zone - The Availability Zone of
      -- the instance. event.code - The code identifying the type of event
      -- (instance-reboot | system-reboot | system-maintenance |
      -- instance-retirement | instance-stop). event.description - A
      -- description of the event. event.not-after - The latest end time
      -- for the scheduled event. event.not-before - The earliest start
      -- time for the scheduled event. instance-state-code - A code
      -- representing the state of the instance, as a 16-bit unsigned
      -- integer. The high byte is an opaque internal value and should be
      -- ignored. The low byte is set based on the state represented. The
      -- valid values are 0 (pending), 16 (running), 32 (shutting-down),
      -- 48 (terminated), 64 (stopping), and 80 (stopped).
      -- instance-state-name - The state of the instance (pending |
      -- running | shutting-down | terminated | stopping | stopped).
      -- instance-status.reachability - Filters on instance status where
      -- the name is reachability (passed | failed | initializing |
      -- insufficient-data). instance-status.status - The status of the
      -- instance (ok | impaired | initializing | insufficient-data |
      -- not-applicable). system-status.reachability - Filters on system
      -- status where the name is reachability (passed | failed |
      -- initializing | insufficient-data). system-status.status - The
      -- system status of the instance (ok | impaired | initializing |
      -- insufficient-data | not-applicable).
    , _disrInstanceIds :: [Text]
      -- ^ One or more instance IDs. Default: Describes all your instances.
      -- Constraints: Maximum 100 explicitly specified instance IDs.
    , _disrMaxResults :: Maybe Integer
      -- ^ The maximum number of paginated instance items per response.
      -- Default: 1000.
    , _disrNextToken :: Maybe Text
      -- ^ The next paginated set of results to return.
    } deriving (Show, Generic)

makeLenses ''DescribeInstanceStatus

instance ToQuery DescribeInstanceStatus where
    toQuery = genericQuery def

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { _dissInstanceStatuses :: [InstanceStatus]
      -- ^ One or more instance status descriptions.
    , _dissNextToken :: Maybe Text
      -- ^ The next paginated set of results to return.
    } deriving (Show, Generic)

makeLenses ''DescribeInstanceStatusResponse

instance FromXML DescribeInstanceStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeInstanceStatus where
    type Sv DescribeInstanceStatus = EC2
    type Rs DescribeInstanceStatus = DescribeInstanceStatusResponse

    request = post "DescribeInstanceStatus"
    response _ = xmlResponse

instance AWSPager DescribeInstanceStatus where
    next rq rs = (\x -> rq { _disrNextToken = Just x })
        <$> (_dissNextToken rs)
