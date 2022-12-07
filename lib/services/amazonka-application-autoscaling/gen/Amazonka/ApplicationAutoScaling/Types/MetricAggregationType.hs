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
-- Module      : Amazonka.ApplicationAutoScaling.Types.MetricAggregationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.MetricAggregationType
  ( MetricAggregationType
      ( ..,
        MetricAggregationType_Average,
        MetricAggregationType_Maximum,
        MetricAggregationType_Minimum
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricAggregationType = MetricAggregationType'
  { fromMetricAggregationType ::
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
