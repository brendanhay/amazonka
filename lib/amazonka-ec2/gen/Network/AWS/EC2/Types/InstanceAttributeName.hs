{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceAttributeName
  ( InstanceAttributeName
      ( InstanceAttributeName',
        IANInstanceType,
        IANKernel,
        IANRAMDisk,
        IANUserData,
        IANDisableAPITermination,
        IANInstanceInitiatedShutdownBehavior,
        IANRootDeviceName,
        IANBlockDeviceMapping,
        IANProductCodes,
        IANSourceDestCheck,
        IANGroupSet,
        IANEBSOptimized,
        IANSRIOVNetSupport,
        IANEnaSupport,
        IANEnclaveOptions
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceAttributeName = InstanceAttributeName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern IANInstanceType :: InstanceAttributeName
pattern IANInstanceType = InstanceAttributeName' "instanceType"

pattern IANKernel :: InstanceAttributeName
pattern IANKernel = InstanceAttributeName' "kernel"

pattern IANRAMDisk :: InstanceAttributeName
pattern IANRAMDisk = InstanceAttributeName' "ramdisk"

pattern IANUserData :: InstanceAttributeName
pattern IANUserData = InstanceAttributeName' "userData"

pattern IANDisableAPITermination :: InstanceAttributeName
pattern IANDisableAPITermination = InstanceAttributeName' "disableApiTermination"

pattern IANInstanceInitiatedShutdownBehavior :: InstanceAttributeName
pattern IANInstanceInitiatedShutdownBehavior = InstanceAttributeName' "instanceInitiatedShutdownBehavior"

pattern IANRootDeviceName :: InstanceAttributeName
pattern IANRootDeviceName = InstanceAttributeName' "rootDeviceName"

pattern IANBlockDeviceMapping :: InstanceAttributeName
pattern IANBlockDeviceMapping = InstanceAttributeName' "blockDeviceMapping"

pattern IANProductCodes :: InstanceAttributeName
pattern IANProductCodes = InstanceAttributeName' "productCodes"

pattern IANSourceDestCheck :: InstanceAttributeName
pattern IANSourceDestCheck = InstanceAttributeName' "sourceDestCheck"

pattern IANGroupSet :: InstanceAttributeName
pattern IANGroupSet = InstanceAttributeName' "groupSet"

pattern IANEBSOptimized :: InstanceAttributeName
pattern IANEBSOptimized = InstanceAttributeName' "ebsOptimized"

pattern IANSRIOVNetSupport :: InstanceAttributeName
pattern IANSRIOVNetSupport = InstanceAttributeName' "sriovNetSupport"

pattern IANEnaSupport :: InstanceAttributeName
pattern IANEnaSupport = InstanceAttributeName' "enaSupport"

pattern IANEnclaveOptions :: InstanceAttributeName
pattern IANEnclaveOptions = InstanceAttributeName' "enclaveOptions"

{-# COMPLETE
  IANInstanceType,
  IANKernel,
  IANRAMDisk,
  IANUserData,
  IANDisableAPITermination,
  IANInstanceInitiatedShutdownBehavior,
  IANRootDeviceName,
  IANBlockDeviceMapping,
  IANProductCodes,
  IANSourceDestCheck,
  IANGroupSet,
  IANEBSOptimized,
  IANSRIOVNetSupport,
  IANEnaSupport,
  IANEnclaveOptions,
  InstanceAttributeName'
  #-}
