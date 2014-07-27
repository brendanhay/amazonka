{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified attribute of the specified instance. You can specify
-- only one attribute at a time. To modify some attributes, the instance must
-- be stopped. For more information, see Modifying Attributes of a Stopped
-- Instance in the Amazon Elastic Compute Cloud User Guide. Example 1 This
-- example changes the instance type of the specified instance. The instance
-- must be in the stopped state.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;InstanceType.Value=m1.small &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 2 This example changes the InstanceInitiatedShutdownBehavior
-- attribute of the specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379
-- &amp;InstanceInitiatedShutdownBehavior.Value=terminate &amp;AUTHPARAMS
-- &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;
-- Example 3 This example changes the DisableApiTermination attribute of the
-- specified instance.
-- https://ec2.amazonaws.com/?Action=ModifyInstanceAttribute
-- &amp;InstanceId=i-10a64379 &amp;DisableApiTermination.Value=true
-- &amp;AUTHPARAMS &lt;ModifyInstanceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyInstanceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_05_01.ModifyInstanceAttribute where

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
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data ModifyInstanceAttribute = ModifyInstanceAttribute
    { _miasInstanceId :: Text
      -- ^ The ID of the instance.
    , _miasSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Specifies whether source/destination checking is enabled. A value
      -- of true means that checking is enabled, and false means checking
      -- is disabled. This value must be false for a NAT instance to
      -- perform NAT.
    , _miasDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ Specifies whether to disable the ability to terminate the
      -- instance using the Amazon EC2 console, CLI, and API.
    , _miasEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Specifies whether the instance is optimized for EBS I/O. This
      -- optimization provides dedicated throughput to Amazon EBS and an
      -- optimized configuration stack to provide optimal EBS I/O
      -- performance. This optimization isn't available with all instance
      -- types. Additional usage charges apply when using an EBS Optimized
      -- instance.
    , _miasKernel :: Maybe AttributeValue
      -- ^ Changes the instance's kernel to the specified value.
    , _miasRamdisk :: Maybe AttributeValue
      -- ^ Changes the instance's RAM disk to the specified value.
    , _miasInstanceType :: Maybe AttributeValue
      -- ^ Changes the instance type to the specified value. For more
      -- information, see Instance Types. If the instance type is not
      -- valid, the error returned is InvalidInstanceAttributeValue.
    , _miasSriovNetSupport :: Maybe AttributeValue
      -- ^ Set to simple to enable enhanced networking for the instance.
      -- There is no way to disable enhanced networking at this time. This
      -- option is supported only for HVM instances. Specifying this
      -- option with a PV instance can make it unreachable.
    , _miasUserData :: Maybe AttributeValue
      -- ^ Changes the instance's user data to the specified value.
    , _miasInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ Specifies whether an instance stops or terminates when you
      -- initiate shutdown from the instance (using the operating system
      -- command for system shutdown).
    , _miasDryRun :: Maybe Bool
      -- ^ 
    , _miasGroups :: [Text]
      -- ^ [EC2-VPC] Changes the security groups of the instance. You must
      -- specify at least one security group, even if it's just the
      -- default security group for the VPC. You must specify the security
      -- group ID, not the security group name. For example, if you want
      -- the instance to be in sg-1a1a1a1a and sg-9b9b9b9b, specify
      -- GroupId.1=sg-1a1a1a1a and GroupId.2=sg-9b9b9b9b.
    , _miasAttribute :: Maybe InstanceAttributeName
      -- ^ The name of the attribute.
    , _miasBlockDeviceMappings :: [InstanceBlockDeviceMappingSpecification]
      -- ^ Modifies the DeleteOnTermination attribute for volumes that are
      -- currently attached. The volume must be owned by the caller. If no
      -- value is specified for DeleteOnTermination, the default is true
      -- and the volume is deleted when the instance is terminated. To add
      -- instance store volumes to an Amazon EBS-backed instance, you must
      -- add them when you launch the instance. For more information, see
      -- Updating the Block Device Mapping when Launching an Instance in
      -- the Amazon Elastic Compute Cloud User Guide.
    , _miasValue :: Maybe Text
      -- ^ A new value for the attribute. Use only with the kernel, ramdisk,
      -- userData, disableApiTermination, or
      -- intanceInitiateShutdownBehavior attribute.
    } deriving (Generic)

instance ToQuery ModifyInstanceAttribute where
    toQuery = genericToQuery def

instance AWSRequest ModifyInstanceAttribute where
    type Sv ModifyInstanceAttribute = EC2
    type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse

    request = post "ModifyInstanceAttribute"
    response _ _ = return (Right ModifyInstanceAttributeResponse)

data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse
    deriving (Eq, Show, Generic)
