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
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
  ( MetricAggregationType
      ( ..,
        MetricAggregationType_Average,
        MetricAggregationType_Maximum,
        MetricAggregationType_Minimum
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MetricAggregationType = MetricAggregationType'
  { fromMetricAggregationType ::
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

pattern MetricAggregationType_Average :: MetricAggregationType
pattern MetricAggregationType_Average = MetricAggregationType' "Average"

pattern MetricAggregationType_Maximum :: MetricAggregationType
pattern MetricAggregationType_Maximum = MetricAggregationType' "Maximum"

pattern MetricAggregationType_Minimum :: MetricAggregationType
pattern MetricAggregationType_Minimum = MetricAggregationType' "Minimum"

{-# COMPLETE
  MetricAggregationType_Average,
  MetricAggregationType_Maximum,
  MetricAggregationType_Minimum,
  MetricAggregationType'
  #-}
