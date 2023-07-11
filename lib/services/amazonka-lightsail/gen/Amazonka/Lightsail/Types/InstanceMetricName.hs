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
-- Module      : Amazonka.Lightsail.Types.InstanceMetricName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.InstanceMetricName
  ( InstanceMetricName
      ( ..,
        InstanceMetricName_BurstCapacityPercentage,
        InstanceMetricName_BurstCapacityTime,
        InstanceMetricName_CPUUtilization,
        InstanceMetricName_MetadataNoToken,
        InstanceMetricName_NetworkIn,
        InstanceMetricName_NetworkOut,
        InstanceMetricName_StatusCheckFailed,
        InstanceMetricName_StatusCheckFailed_Instance,
        InstanceMetricName_StatusCheckFailed_System
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceMetricName = InstanceMetricName'
  { fromInstanceMetricName ::
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

pattern InstanceMetricName_BurstCapacityPercentage :: InstanceMetricName
pattern InstanceMetricName_BurstCapacityPercentage = InstanceMetricName' "BurstCapacityPercentage"

pattern InstanceMetricName_BurstCapacityTime :: InstanceMetricName
pattern InstanceMetricName_BurstCapacityTime = InstanceMetricName' "BurstCapacityTime"

pattern InstanceMetricName_CPUUtilization :: InstanceMetricName
pattern InstanceMetricName_CPUUtilization = InstanceMetricName' "CPUUtilization"

pattern InstanceMetricName_MetadataNoToken :: InstanceMetricName
pattern InstanceMetricName_MetadataNoToken = InstanceMetricName' "MetadataNoToken"

pattern InstanceMetricName_NetworkIn :: InstanceMetricName
pattern InstanceMetricName_NetworkIn = InstanceMetricName' "NetworkIn"

pattern InstanceMetricName_NetworkOut :: InstanceMetricName
pattern InstanceMetricName_NetworkOut = InstanceMetricName' "NetworkOut"

pattern InstanceMetricName_StatusCheckFailed :: InstanceMetricName
pattern InstanceMetricName_StatusCheckFailed = InstanceMetricName' "StatusCheckFailed"

pattern InstanceMetricName_StatusCheckFailed_Instance :: InstanceMetricName
pattern InstanceMetricName_StatusCheckFailed_Instance = InstanceMetricName' "StatusCheckFailed_Instance"

pattern InstanceMetricName_StatusCheckFailed_System :: InstanceMetricName
pattern InstanceMetricName_StatusCheckFailed_System = InstanceMetricName' "StatusCheckFailed_System"

{-# COMPLETE
  InstanceMetricName_BurstCapacityPercentage,
  InstanceMetricName_BurstCapacityTime,
  InstanceMetricName_CPUUtilization,
  InstanceMetricName_MetadataNoToken,
  InstanceMetricName_NetworkIn,
  InstanceMetricName_NetworkOut,
  InstanceMetricName_StatusCheckFailed,
  InstanceMetricName_StatusCheckFailed_Instance,
  InstanceMetricName_StatusCheckFailed_System,
  InstanceMetricName'
  #-}
