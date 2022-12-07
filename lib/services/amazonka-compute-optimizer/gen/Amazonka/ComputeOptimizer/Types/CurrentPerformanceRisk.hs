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
-- Module      : Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.CurrentPerformanceRisk
  ( CurrentPerformanceRisk
      ( ..,
        CurrentPerformanceRisk_High,
        CurrentPerformanceRisk_Low,
        CurrentPerformanceRisk_Medium,
        CurrentPerformanceRisk_VeryLow
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CurrentPerformanceRisk = CurrentPerformanceRisk'
  { fromCurrentPerformanceRisk ::
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

pattern CurrentPerformanceRisk_High :: CurrentPerformanceRisk
pattern CurrentPerformanceRisk_High = CurrentPerformanceRisk' "High"

pattern CurrentPerformanceRisk_Low :: CurrentPerformanceRisk
pattern CurrentPerformanceRisk_Low = CurrentPerformanceRisk' "Low"

pattern CurrentPerformanceRisk_Medium :: CurrentPerformanceRisk
pattern CurrentPerformanceRisk_Medium = CurrentPerformanceRisk' "Medium"

pattern CurrentPerformanceRisk_VeryLow :: CurrentPerformanceRisk
pattern CurrentPerformanceRisk_VeryLow = CurrentPerformanceRisk' "VeryLow"

{-# COMPLETE
  CurrentPerformanceRisk_High,
  CurrentPerformanceRisk_Low,
  CurrentPerformanceRisk_Medium,
  CurrentPerformanceRisk_VeryLow,
  CurrentPerformanceRisk'
  #-}
