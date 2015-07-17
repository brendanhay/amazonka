{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS Config
--
-- AWS Config provides a way to keep track of the configurations of all the
-- AWS resources associated with your AWS account. You can use AWS Config
-- to get the current and historical configurations of each AWS resource
-- and also to get information about the relationship between the
-- resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2)
-- instance, an Elastic Block Store (EBS) volume, an Elastic network
-- Interface (ENI), or a security group. For a complete list of resources
-- currently supported by AWS Config, see
-- <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resources>.
--
-- You can access and manage AWS Config through the AWS Management Console,
-- the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS
-- SDKs for AWS Config
--
-- This reference guide contains documentation for the AWS Config API and
-- the AWS CLI commands that you can use to manage AWS Config.
--
-- The AWS Config API uses the Signature Version 4 protocol for signing
-- requests. For more information about how to sign a request with this
-- protocol, see
-- <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- For detailed information about AWS Config features and their associated
-- actions or commands, as well as how to work with AWS Management Console,
-- see
-- <http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html What Is AWS Config?>
-- in the /AWS Config Developer Guide/.
module Network.AWS.Config
    ( module Export
    ) where

import           Network.AWS.Config.DeleteDeliveryChannel               as Export
import           Network.AWS.Config.DeliverConfigSnapshot               as Export
import           Network.AWS.Config.DescribeConfigurationRecorders      as Export
import           Network.AWS.Config.DescribeConfigurationRecorderStatus as Export
import           Network.AWS.Config.DescribeDeliveryChannels            as Export
import           Network.AWS.Config.DescribeDeliveryChannelStatus       as Export
import           Network.AWS.Config.GetResourceConfigHistory            as Export
import           Network.AWS.Config.PutConfigurationRecorder            as Export
import           Network.AWS.Config.PutDeliveryChannel                  as Export
import           Network.AWS.Config.StartConfigurationRecorder          as Export
import           Network.AWS.Config.StopConfigurationRecorder           as Export
import           Network.AWS.Config.Types                               as Export
import           Network.AWS.Config.Types.Product                       as Export
import           Network.AWS.Config.Types.Sum                           as Export
import           Network.AWS.Config.Waiters                             as Export
