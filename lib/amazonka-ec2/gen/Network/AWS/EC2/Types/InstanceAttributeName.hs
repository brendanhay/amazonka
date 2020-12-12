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
        IANBlockDeviceMapping,
        IANDisableAPITermination,
        IANEBSOptimized,
        IANEnaSupport,
        IANEnclaveOptions,
        IANGroupSet,
        IANInstanceInitiatedShutdownBehavior,
        IANInstanceType,
        IANKernel,
        IANProductCodes,
        IANRAMDisk,
        IANRootDeviceName,
        IANSRIOVNetSupport,
        IANSourceDestCheck,
        IANUserData
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

pattern IANBlockDeviceMapping :: InstanceAttributeName
pattern IANBlockDeviceMapping = InstanceAttributeName' "blockDeviceMapping"

pattern IANDisableAPITermination :: InstanceAttributeName
pattern IANDisableAPITermination = InstanceAttributeName' "disableApiTermination"

pattern IANEBSOptimized :: InstanceAttributeName
pattern IANEBSOptimized = InstanceAttributeName' "ebsOptimized"

pattern IANEnaSupport :: InstanceAttributeName
pattern IANEnaSupport = InstanceAttributeName' "enaSupport"

pattern IANEnclaveOptions :: InstanceAttributeName
pattern IANEnclaveOptions = InstanceAttributeName' "enclaveOptions"

pattern IANGroupSet :: InstanceAttributeName
pattern IANGroupSet = InstanceAttributeName' "groupSet"

pattern IANInstanceInitiatedShutdownBehavior :: InstanceAttributeName
pattern IANInstanceInitiatedShutdownBehavior = InstanceAttributeName' "instanceInitiatedShutdownBehavior"

pattern IANInstanceType :: InstanceAttributeName
pattern IANInstanceType = InstanceAttributeName' "instanceType"

pattern IANKernel :: InstanceAttributeName
pattern IANKernel = InstanceAttributeName' "kernel"

pattern IANProductCodes :: InstanceAttributeName
pattern IANProductCodes = InstanceAttributeName' "productCodes"

pattern IANRAMDisk :: InstanceAttributeName
pattern IANRAMDisk = InstanceAttributeName' "ramdisk"

pattern IANRootDeviceName :: InstanceAttributeName
pattern IANRootDeviceName = InstanceAttributeName' "rootDeviceName"

pattern IANSRIOVNetSupport :: InstanceAttributeName
pattern IANSRIOVNetSupport = InstanceAttributeName' "sriovNetSupport"

pattern IANSourceDestCheck :: InstanceAttributeName
pattern IANSourceDestCheck = InstanceAttributeName' "sourceDestCheck"

pattern IANUserData :: InstanceAttributeName
pattern IANUserData = InstanceAttributeName' "userData"

{-# COMPLETE
  IANBlockDeviceMapping,
  IANDisableAPITermination,
  IANEBSOptimized,
  IANEnaSupport,
  IANEnclaveOptions,
  IANGroupSet,
  IANInstanceInitiatedShutdownBehavior,
  IANInstanceType,
  IANKernel,
  IANProductCodes,
  IANRAMDisk,
  IANRootDeviceName,
  IANSRIOVNetSupport,
  IANSourceDestCheck,
  IANUserData,
  InstanceAttributeName'
  #-}
