{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Types.NodegroupIssueCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.NodegroupIssueCode
  ( NodegroupIssueCode
      ( ..,
        NodegroupIssueCode_AccessDenied,
        NodegroupIssueCode_AsgInstanceLaunchFailures,
        NodegroupIssueCode_AutoScalingGroupInvalidConfiguration,
        NodegroupIssueCode_AutoScalingGroupNotFound,
        NodegroupIssueCode_ClusterUnreachable,
        NodegroupIssueCode_Ec2LaunchTemplateNotFound,
        NodegroupIssueCode_Ec2LaunchTemplateVersionMismatch,
        NodegroupIssueCode_Ec2SecurityGroupDeletionFailure,
        NodegroupIssueCode_Ec2SecurityGroupNotFound,
        NodegroupIssueCode_Ec2SubnetInvalidConfiguration,
        NodegroupIssueCode_Ec2SubnetMissingIpv6Assignment,
        NodegroupIssueCode_Ec2SubnetNotFound,
        NodegroupIssueCode_IamInstanceProfileNotFound,
        NodegroupIssueCode_IamLimitExceeded,
        NodegroupIssueCode_IamNodeRoleNotFound,
        NodegroupIssueCode_InstanceLimitExceeded,
        NodegroupIssueCode_InsufficientFreeAddresses,
        NodegroupIssueCode_InternalFailure,
        NodegroupIssueCode_NodeCreationFailure
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodegroupIssueCode = NodegroupIssueCode'
  { fromNodegroupIssueCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern NodegroupIssueCode_AccessDenied :: NodegroupIssueCode
pattern NodegroupIssueCode_AccessDenied = NodegroupIssueCode' "AccessDenied"

pattern NodegroupIssueCode_AsgInstanceLaunchFailures :: NodegroupIssueCode
pattern NodegroupIssueCode_AsgInstanceLaunchFailures = NodegroupIssueCode' "AsgInstanceLaunchFailures"

pattern NodegroupIssueCode_AutoScalingGroupInvalidConfiguration :: NodegroupIssueCode
pattern NodegroupIssueCode_AutoScalingGroupInvalidConfiguration = NodegroupIssueCode' "AutoScalingGroupInvalidConfiguration"

pattern NodegroupIssueCode_AutoScalingGroupNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_AutoScalingGroupNotFound = NodegroupIssueCode' "AutoScalingGroupNotFound"

pattern NodegroupIssueCode_ClusterUnreachable :: NodegroupIssueCode
pattern NodegroupIssueCode_ClusterUnreachable = NodegroupIssueCode' "ClusterUnreachable"

pattern NodegroupIssueCode_Ec2LaunchTemplateNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2LaunchTemplateNotFound = NodegroupIssueCode' "Ec2LaunchTemplateNotFound"

pattern NodegroupIssueCode_Ec2LaunchTemplateVersionMismatch :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2LaunchTemplateVersionMismatch = NodegroupIssueCode' "Ec2LaunchTemplateVersionMismatch"

pattern NodegroupIssueCode_Ec2SecurityGroupDeletionFailure :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2SecurityGroupDeletionFailure = NodegroupIssueCode' "Ec2SecurityGroupDeletionFailure"

pattern NodegroupIssueCode_Ec2SecurityGroupNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2SecurityGroupNotFound = NodegroupIssueCode' "Ec2SecurityGroupNotFound"

pattern NodegroupIssueCode_Ec2SubnetInvalidConfiguration :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2SubnetInvalidConfiguration = NodegroupIssueCode' "Ec2SubnetInvalidConfiguration"

pattern NodegroupIssueCode_Ec2SubnetMissingIpv6Assignment :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2SubnetMissingIpv6Assignment = NodegroupIssueCode' "Ec2SubnetMissingIpv6Assignment"

pattern NodegroupIssueCode_Ec2SubnetNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_Ec2SubnetNotFound = NodegroupIssueCode' "Ec2SubnetNotFound"

pattern NodegroupIssueCode_IamInstanceProfileNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_IamInstanceProfileNotFound = NodegroupIssueCode' "IamInstanceProfileNotFound"

pattern NodegroupIssueCode_IamLimitExceeded :: NodegroupIssueCode
pattern NodegroupIssueCode_IamLimitExceeded = NodegroupIssueCode' "IamLimitExceeded"

pattern NodegroupIssueCode_IamNodeRoleNotFound :: NodegroupIssueCode
pattern NodegroupIssueCode_IamNodeRoleNotFound = NodegroupIssueCode' "IamNodeRoleNotFound"

pattern NodegroupIssueCode_InstanceLimitExceeded :: NodegroupIssueCode
pattern NodegroupIssueCode_InstanceLimitExceeded = NodegroupIssueCode' "InstanceLimitExceeded"

pattern NodegroupIssueCode_InsufficientFreeAddresses :: NodegroupIssueCode
pattern NodegroupIssueCode_InsufficientFreeAddresses = NodegroupIssueCode' "InsufficientFreeAddresses"

pattern NodegroupIssueCode_InternalFailure :: NodegroupIssueCode
pattern NodegroupIssueCode_InternalFailure = NodegroupIssueCode' "InternalFailure"

pattern NodegroupIssueCode_NodeCreationFailure :: NodegroupIssueCode
pattern NodegroupIssueCode_NodeCreationFailure = NodegroupIssueCode' "NodeCreationFailure"

{-# COMPLETE
  NodegroupIssueCode_AccessDenied,
  NodegroupIssueCode_AsgInstanceLaunchFailures,
  NodegroupIssueCode_AutoScalingGroupInvalidConfiguration,
  NodegroupIssueCode_AutoScalingGroupNotFound,
  NodegroupIssueCode_ClusterUnreachable,
  NodegroupIssueCode_Ec2LaunchTemplateNotFound,
  NodegroupIssueCode_Ec2LaunchTemplateVersionMismatch,
  NodegroupIssueCode_Ec2SecurityGroupDeletionFailure,
  NodegroupIssueCode_Ec2SecurityGroupNotFound,
  NodegroupIssueCode_Ec2SubnetInvalidConfiguration,
  NodegroupIssueCode_Ec2SubnetMissingIpv6Assignment,
  NodegroupIssueCode_Ec2SubnetNotFound,
  NodegroupIssueCode_IamInstanceProfileNotFound,
  NodegroupIssueCode_IamLimitExceeded,
  NodegroupIssueCode_IamNodeRoleNotFound,
  NodegroupIssueCode_InstanceLimitExceeded,
  NodegroupIssueCode_InsufficientFreeAddresses,
  NodegroupIssueCode_InternalFailure,
  NodegroupIssueCode_NodeCreationFailure,
  NodegroupIssueCode'
  #-}
