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
-- Module      : Amazonka.CloudWatch.Types.MetricStreamOutputFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MetricStreamOutputFormat
  ( MetricStreamOutputFormat
      ( ..,
        MetricStreamOutputFormat_Json,
        MetricStreamOutputFormat_Opentelemetry0_7
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricStreamOutputFormat = MetricStreamOutputFormat'
  { fromMetricStreamOutputFormat ::
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

pattern MetricStreamOutputFormat_Json :: MetricStreamOutputFormat
pattern MetricStreamOutputFormat_Json = MetricStreamOutputFormat' "json"

pattern MetricStreamOutputFormat_Opentelemetry0_7 :: MetricStreamOutputFormat
pattern MetricStreamOutputFormat_Opentelemetry0_7 = MetricStreamOutputFormat' "opentelemetry0.7"

{-# COMPLETE
  MetricStreamOutputFormat_Json,
  MetricStreamOutputFormat_Opentelemetry0_7,
  MetricStreamOutputFormat'
  #-}
