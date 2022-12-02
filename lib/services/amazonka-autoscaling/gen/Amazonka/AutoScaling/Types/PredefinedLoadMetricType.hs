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
-- Module      : Amazonka.AutoScaling.Types.PredefinedLoadMetricType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredefinedLoadMetricType
  ( PredefinedLoadMetricType
      ( ..,
        PredefinedLoadMetricType_ALBTargetGroupRequestCount,
        PredefinedLoadMetricType_ASGTotalCPUUtilization,
        PredefinedLoadMetricType_ASGTotalNetworkIn,
        PredefinedLoadMetricType_ASGTotalNetworkOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PredefinedLoadMetricType = PredefinedLoadMetricType'
  { fromPredefinedLoadMetricType ::
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

pattern PredefinedLoadMetricType_ALBTargetGroupRequestCount :: PredefinedLoadMetricType
pattern PredefinedLoadMetricType_ALBTargetGroupRequestCount = PredefinedLoadMetricType' "ALBTargetGroupRequestCount"

pattern PredefinedLoadMetricType_ASGTotalCPUUtilization :: PredefinedLoadMetricType
pattern PredefinedLoadMetricType_ASGTotalCPUUtilization = PredefinedLoadMetricType' "ASGTotalCPUUtilization"

pattern PredefinedLoadMetricType_ASGTotalNetworkIn :: PredefinedLoadMetricType
pattern PredefinedLoadMetricType_ASGTotalNetworkIn = PredefinedLoadMetricType' "ASGTotalNetworkIn"

pattern PredefinedLoadMetricType_ASGTotalNetworkOut :: PredefinedLoadMetricType
pattern PredefinedLoadMetricType_ASGTotalNetworkOut = PredefinedLoadMetricType' "ASGTotalNetworkOut"

{-# COMPLETE
  PredefinedLoadMetricType_ALBTargetGroupRequestCount,
  PredefinedLoadMetricType_ASGTotalCPUUtilization,
  PredefinedLoadMetricType_ASGTotalNetworkIn,
  PredefinedLoadMetricType_ASGTotalNetworkOut,
  PredefinedLoadMetricType'
  #-}
