{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.LoadMetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.LoadMetricType
  ( LoadMetricType
      ( ..,
        LoadMetricType_ALBTargetGroupRequestCount,
        LoadMetricType_ASGTotalCPUUtilization,
        LoadMetricType_ASGTotalNetworkIn,
        LoadMetricType_ASGTotalNetworkOut
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LoadMetricType = LoadMetricType'
  { fromLoadMetricType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
