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
-- Module      : Amazonka.Forecast.Types.AutoMLOverrideStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.AutoMLOverrideStrategy
  ( AutoMLOverrideStrategy
      ( ..,
        AutoMLOverrideStrategy_AccuracyOptimized,
        AutoMLOverrideStrategy_LatencyOptimized
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLOverrideStrategy = AutoMLOverrideStrategy'
  { fromAutoMLOverrideStrategy ::
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

pattern AutoMLOverrideStrategy_AccuracyOptimized :: AutoMLOverrideStrategy
pattern AutoMLOverrideStrategy_AccuracyOptimized = AutoMLOverrideStrategy' "AccuracyOptimized"

pattern AutoMLOverrideStrategy_LatencyOptimized :: AutoMLOverrideStrategy
pattern AutoMLOverrideStrategy_LatencyOptimized = AutoMLOverrideStrategy' "LatencyOptimized"

{-# COMPLETE
  AutoMLOverrideStrategy_AccuracyOptimized,
  AutoMLOverrideStrategy_LatencyOptimized,
  AutoMLOverrideStrategy'
  #-}
