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
-- Module      : Amazonka.AutoScalingPlans.Types.LoadMetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScalingPlans.Types.LoadMetricType
  ( LoadMetricType
      ( ..,
        LoadMetricType_ALBTargetGroupRequestCount,
        LoadMetricType_ASGTotalCPUUtilization,
        LoadMetricType_ASGTotalNetworkIn,
        LoadMetricType_ASGTotalNetworkOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadMetricType = LoadMetricType'
  { fromLoadMetricType ::
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

pattern LoadMetricType_ALBTargetGroupRequestCount :: LoadMetricType
pattern LoadMetricType_ALBTargetGroupRequestCount = LoadMetricType' "ALBTargetGroupRequestCount"

pattern LoadMetricType_ASGTotalCPUUtilization :: LoadMetricType
pattern LoadMetricType_ASGTotalCPUUtilization = LoadMetricType' "ASGTotalCPUUtilization"

pattern LoadMetricType_ASGTotalNetworkIn :: LoadMetricType
pattern LoadMetricType_ASGTotalNetworkIn = LoadMetricType' "ASGTotalNetworkIn"

pattern LoadMetricType_ASGTotalNetworkOut :: LoadMetricType
pattern LoadMetricType_ASGTotalNetworkOut = LoadMetricType' "ASGTotalNetworkOut"

{-# COMPLETE
  LoadMetricType_ALBTargetGroupRequestCount,
  LoadMetricType_ASGTotalCPUUtilization,
  LoadMetricType_ASGTotalNetworkIn,
  LoadMetricType_ASGTotalNetworkOut,
  LoadMetricType'
  #-}
