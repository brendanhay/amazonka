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
-- Module      : Amazonka.SageMaker.Types.MetricSetSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MetricSetSource
  ( MetricSetSource
      ( ..,
        MetricSetSource_Test,
        MetricSetSource_Train,
        MetricSetSource_Validation
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MetricSetSource = MetricSetSource'
  { fromMetricSetSource ::
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

pattern MetricSetSource_Test :: MetricSetSource
pattern MetricSetSource_Test = MetricSetSource' "Test"

pattern MetricSetSource_Train :: MetricSetSource
pattern MetricSetSource_Train = MetricSetSource' "Train"

pattern MetricSetSource_Validation :: MetricSetSource
pattern MetricSetSource_Validation = MetricSetSource' "Validation"

{-# COMPLETE
  MetricSetSource_Test,
  MetricSetSource_Train,
  MetricSetSource_Validation,
  MetricSetSource'
  #-}
