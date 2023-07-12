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
-- Module      : Amazonka.AutoScaling.Types.PredefinedMetricPairType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredefinedMetricPairType
  ( PredefinedMetricPairType
      ( ..,
        PredefinedMetricPairType_ALBRequestCount,
        PredefinedMetricPairType_ASGCPUUtilization,
        PredefinedMetricPairType_ASGNetworkIn,
        PredefinedMetricPairType_ASGNetworkOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PredefinedMetricPairType = PredefinedMetricPairType'
  { fromPredefinedMetricPairType ::
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
