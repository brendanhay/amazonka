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
-- Module      : Network.AWS.EC2.Types.InstanceAttributeName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceAttributeName
  ( InstanceAttributeName
      ( ..,
        InstanceAttributeName_BlockDeviceMapping,
        InstanceAttributeName_DisableApiTermination,
        InstanceAttributeName_EbsOptimized,
        InstanceAttributeName_EnaSupport,
        InstanceAttributeName_EnclaveOptions,
        InstanceAttributeName_GroupSet,
        InstanceAttributeName_InstanceInitiatedShutdownBehavior,
        InstanceAttributeName_InstanceType,
        InstanceAttributeName_Kernel,
        InstanceAttributeName_ProductCodes,
        InstanceAttributeName_Ramdisk,
        InstanceAttributeName_RootDeviceName,
        InstanceAttributeName_SourceDestCheck,
        InstanceAttributeName_SriovNetSupport,
        InstanceAttributeName_UserData
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype InstanceAttributeName = InstanceAttributeName'
  { fromInstanceAttributeName ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern InstanceAttributeName_BlockDeviceMapping :: InstanceAttributeName
pattern InstanceAttributeName_BlockDeviceMapping = InstanceAttributeName' "blockDeviceMapping"

pattern InstanceAttributeName_DisableApiTermination :: InstanceAttributeName
pattern InstanceAttributeName_DisableApiTermination = InstanceAttributeName' "disableApiTermination"

pattern InstanceAttributeName_EbsOptimized :: InstanceAttributeName
pattern InstanceAttributeName_EbsOptimized = InstanceAttributeName' "ebsOptimized"

pattern InstanceAttributeName_EnaSupport :: InstanceAttributeName
pattern InstanceAttributeName_EnaSupport = InstanceAttributeName' "enaSupport"

pattern InstanceAttributeName_EnclaveOptions :: InstanceAttributeName
pattern InstanceAttributeName_EnclaveOptions = InstanceAttributeName' "enclaveOptions"

pattern InstanceAttributeName_GroupSet :: InstanceAttributeName
pattern InstanceAttributeName_GroupSet = InstanceAttributeName' "groupSet"

pattern InstanceAttributeName_InstanceInitiatedShutdownBehavior :: InstanceAttributeName
pattern InstanceAttributeName_InstanceInitiatedShutdownBehavior = InstanceAttributeName' "instanceInitiatedShutdownBehavior"

pattern InstanceAttributeName_InstanceType :: InstanceAttributeName
pattern InstanceAttributeName_InstanceType = InstanceAttributeName' "instanceType"

pattern InstanceAttributeName_Kernel :: InstanceAttributeName
pattern InstanceAttributeName_Kernel = InstanceAttributeName' "kernel"

pattern InstanceAttributeName_ProductCodes :: InstanceAttributeName
pattern InstanceAttributeName_ProductCodes = InstanceAttributeName' "productCodes"

pattern InstanceAttributeName_Ramdisk :: InstanceAttributeName
pattern InstanceAttributeName_Ramdisk = InstanceAttributeName' "ramdisk"

pattern InstanceAttributeName_RootDeviceName :: InstanceAttributeName
pattern InstanceAttributeName_RootDeviceName = InstanceAttributeName' "rootDeviceName"

pattern InstanceAttributeName_SourceDestCheck :: InstanceAttributeName
pattern InstanceAttributeName_SourceDestCheck = InstanceAttributeName' "sourceDestCheck"

pattern InstanceAttributeName_SriovNetSupport :: InstanceAttributeName
pattern InstanceAttributeName_SriovNetSupport = InstanceAttributeName' "sriovNetSupport"

pattern InstanceAttributeName_UserData :: InstanceAttributeName
pattern InstanceAttributeName_UserData = InstanceAttributeName' "userData"

{-# COMPLETE
  InstanceAttributeName_BlockDeviceMapping,
  InstanceAttributeName_DisableApiTermination,
  InstanceAttributeName_EbsOptimized,
  InstanceAttributeName_EnaSupport,
  InstanceAttributeName_EnclaveOptions,
  InstanceAttributeName_GroupSet,
  InstanceAttributeName_InstanceInitiatedShutdownBehavior,
  InstanceAttributeName_InstanceType,
  InstanceAttributeName_Kernel,
  InstanceAttributeName_ProductCodes,
  InstanceAttributeName_Ramdisk,
  InstanceAttributeName_RootDeviceName,
  InstanceAttributeName_SourceDestCheck,
  InstanceAttributeName_SriovNetSupport,
  InstanceAttributeName_UserData,
  InstanceAttributeName'
  #-}
