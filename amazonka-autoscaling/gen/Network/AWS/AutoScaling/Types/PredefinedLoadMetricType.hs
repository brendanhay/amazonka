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
-- Module      : Network.AWS.AutoScaling.Types.PredefinedLoadMetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.PredefinedLoadMetricType
  ( PredefinedLoadMetricType
      ( ..,
        PredefinedLoadMetricType_ALBTargetGroupRequestCount,
        PredefinedLoadMetricType_ASGTotalCPUUtilization,
        PredefinedLoadMetricType_ASGTotalNetworkIn,
        PredefinedLoadMetricType_ASGTotalNetworkOut
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PredefinedLoadMetricType = PredefinedLoadMetricType'
  { fromPredefinedLoadMetricType ::
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
