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
-- Module      : Network.AWS.AutoScaling.Types.PredefinedMetricPairType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.PredefinedMetricPairType
  ( PredefinedMetricPairType
      ( ..,
        PredefinedMetricPairType_ALBRequestCount,
        PredefinedMetricPairType_ASGCPUUtilization,
        PredefinedMetricPairType_ASGNetworkIn,
        PredefinedMetricPairType_ASGNetworkOut
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PredefinedMetricPairType = PredefinedMetricPairType'
  { fromPredefinedMetricPairType ::
      Core.Text
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

pattern PredefinedMetricPairType_ALBRequestCount :: PredefinedMetricPairType
pattern PredefinedMetricPairType_ALBRequestCount = PredefinedMetricPairType' "ALBRequestCount"

pattern PredefinedMetricPairType_ASGCPUUtilization :: PredefinedMetricPairType
pattern PredefinedMetricPairType_ASGCPUUtilization = PredefinedMetricPairType' "ASGCPUUtilization"

pattern PredefinedMetricPairType_ASGNetworkIn :: PredefinedMetricPairType
pattern PredefinedMetricPairType_ASGNetworkIn = PredefinedMetricPairType' "ASGNetworkIn"

pattern PredefinedMetricPairType_ASGNetworkOut :: PredefinedMetricPairType
pattern PredefinedMetricPairType_ASGNetworkOut = PredefinedMetricPairType' "ASGNetworkOut"

{-# COMPLETE
  PredefinedMetricPairType_ALBRequestCount,
  PredefinedMetricPairType_ASGCPUUtilization,
  PredefinedMetricPairType_ASGNetworkIn,
  PredefinedMetricPairType_ASGNetworkOut,
  PredefinedMetricPairType'
  #-}
