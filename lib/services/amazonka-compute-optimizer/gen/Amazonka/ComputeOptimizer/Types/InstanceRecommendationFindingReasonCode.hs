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
-- Module      : Amazonka.ComputeOptimizer.Types.InstanceRecommendationFindingReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.InstanceRecommendationFindingReasonCode
  ( InstanceRecommendationFindingReasonCode
      ( ..,
        InstanceRecommendationFindingReasonCode_CPUOverprovisioned,
        InstanceRecommendationFindingReasonCode_CPUUnderprovisioned,
        InstanceRecommendationFindingReasonCode_DiskIOPSOverprovisioned,
        InstanceRecommendationFindingReasonCode_DiskIOPSUnderprovisioned,
        InstanceRecommendationFindingReasonCode_DiskThroughputOverprovisioned,
        InstanceRecommendationFindingReasonCode_DiskThroughputUnderprovisioned,
        InstanceRecommendationFindingReasonCode_EBSIOPSOverprovisioned,
        InstanceRecommendationFindingReasonCode_EBSIOPSUnderprovisioned,
        InstanceRecommendationFindingReasonCode_EBSThroughputOverprovisioned,
        InstanceRecommendationFindingReasonCode_EBSThroughputUnderprovisioned,
        InstanceRecommendationFindingReasonCode_MemoryOverprovisioned,
        InstanceRecommendationFindingReasonCode_MemoryUnderprovisioned,
        InstanceRecommendationFindingReasonCode_NetworkBandwidthOverprovisioned,
        InstanceRecommendationFindingReasonCode_NetworkBandwidthUnderprovisioned,
        InstanceRecommendationFindingReasonCode_NetworkPPSOverprovisioned,
        InstanceRecommendationFindingReasonCode_NetworkPPSUnderprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceRecommendationFindingReasonCode = InstanceRecommendationFindingReasonCode'
  { fromInstanceRecommendationFindingReasonCode ::
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

pattern InstanceRecommendationFindingReasonCode_CPUOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_CPUOverprovisioned = InstanceRecommendationFindingReasonCode' "CPUOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_CPUUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_CPUUnderprovisioned = InstanceRecommendationFindingReasonCode' "CPUUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_DiskIOPSOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_DiskIOPSOverprovisioned = InstanceRecommendationFindingReasonCode' "DiskIOPSOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_DiskIOPSUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_DiskIOPSUnderprovisioned = InstanceRecommendationFindingReasonCode' "DiskIOPSUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_DiskThroughputOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_DiskThroughputOverprovisioned = InstanceRecommendationFindingReasonCode' "DiskThroughputOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_DiskThroughputUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_DiskThroughputUnderprovisioned = InstanceRecommendationFindingReasonCode' "DiskThroughputUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_EBSIOPSOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_EBSIOPSOverprovisioned = InstanceRecommendationFindingReasonCode' "EBSIOPSOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_EBSIOPSUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_EBSIOPSUnderprovisioned = InstanceRecommendationFindingReasonCode' "EBSIOPSUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_EBSThroughputOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_EBSThroughputOverprovisioned = InstanceRecommendationFindingReasonCode' "EBSThroughputOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_EBSThroughputUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_EBSThroughputUnderprovisioned = InstanceRecommendationFindingReasonCode' "EBSThroughputUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_MemoryOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_MemoryOverprovisioned = InstanceRecommendationFindingReasonCode' "MemoryOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_MemoryUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_MemoryUnderprovisioned = InstanceRecommendationFindingReasonCode' "MemoryUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_NetworkBandwidthOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_NetworkBandwidthOverprovisioned = InstanceRecommendationFindingReasonCode' "NetworkBandwidthOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_NetworkBandwidthUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_NetworkBandwidthUnderprovisioned = InstanceRecommendationFindingReasonCode' "NetworkBandwidthUnderprovisioned"

pattern InstanceRecommendationFindingReasonCode_NetworkPPSOverprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_NetworkPPSOverprovisioned = InstanceRecommendationFindingReasonCode' "NetworkPPSOverprovisioned"

pattern InstanceRecommendationFindingReasonCode_NetworkPPSUnderprovisioned :: InstanceRecommendationFindingReasonCode
pattern InstanceRecommendationFindingReasonCode_NetworkPPSUnderprovisioned = InstanceRecommendationFindingReasonCode' "NetworkPPSUnderprovisioned"

{-# COMPLETE
  InstanceRecommendationFindingReasonCode_CPUOverprovisioned,
  InstanceRecommendationFindingReasonCode_CPUUnderprovisioned,
  InstanceRecommendationFindingReasonCode_DiskIOPSOverprovisioned,
  InstanceRecommendationFindingReasonCode_DiskIOPSUnderprovisioned,
  InstanceRecommendationFindingReasonCode_DiskThroughputOverprovisioned,
  InstanceRecommendationFindingReasonCode_DiskThroughputUnderprovisioned,
  InstanceRecommendationFindingReasonCode_EBSIOPSOverprovisioned,
  InstanceRecommendationFindingReasonCode_EBSIOPSUnderprovisioned,
  InstanceRecommendationFindingReasonCode_EBSThroughputOverprovisioned,
  InstanceRecommendationFindingReasonCode_EBSThroughputUnderprovisioned,
  InstanceRecommendationFindingReasonCode_MemoryOverprovisioned,
  InstanceRecommendationFindingReasonCode_MemoryUnderprovisioned,
  InstanceRecommendationFindingReasonCode_NetworkBandwidthOverprovisioned,
  InstanceRecommendationFindingReasonCode_NetworkBandwidthUnderprovisioned,
  InstanceRecommendationFindingReasonCode_NetworkPPSOverprovisioned,
  InstanceRecommendationFindingReasonCode_NetworkPPSUnderprovisioned,
  InstanceRecommendationFindingReasonCode'
  #-}
