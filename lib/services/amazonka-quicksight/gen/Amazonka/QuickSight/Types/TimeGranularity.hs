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
-- Module      : Amazonka.QuickSight.Types.TimeGranularity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeGranularity
  ( TimeGranularity
      ( ..,
        TimeGranularity_DAY,
        TimeGranularity_HOUR,
        TimeGranularity_MILLISECOND,
        TimeGranularity_MINUTE,
        TimeGranularity_MONTH,
        TimeGranularity_QUARTER,
        TimeGranularity_SECOND,
        TimeGranularity_WEEK,
        TimeGranularity_YEAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TimeGranularity = TimeGranularity'
  { fromTimeGranularity ::
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

pattern TimeGranularity_DAY :: TimeGranularity
pattern TimeGranularity_DAY = TimeGranularity' "DAY"

pattern TimeGranularity_HOUR :: TimeGranularity
pattern TimeGranularity_HOUR = TimeGranularity' "HOUR"

pattern TimeGranularity_MILLISECOND :: TimeGranularity
pattern TimeGranularity_MILLISECOND = TimeGranularity' "MILLISECOND"

pattern TimeGranularity_MINUTE :: TimeGranularity
pattern TimeGranularity_MINUTE = TimeGranularity' "MINUTE"

pattern TimeGranularity_MONTH :: TimeGranularity
pattern TimeGranularity_MONTH = TimeGranularity' "MONTH"

pattern TimeGranularity_QUARTER :: TimeGranularity
pattern TimeGranularity_QUARTER = TimeGranularity' "QUARTER"

pattern TimeGranularity_SECOND :: TimeGranularity
pattern TimeGranularity_SECOND = TimeGranularity' "SECOND"

pattern TimeGranularity_WEEK :: TimeGranularity
pattern TimeGranularity_WEEK = TimeGranularity' "WEEK"

pattern TimeGranularity_YEAR :: TimeGranularity
pattern TimeGranularity_YEAR = TimeGranularity' "YEAR"

{-# COMPLETE
  TimeGranularity_DAY,
  TimeGranularity_HOUR,
  TimeGranularity_MILLISECOND,
  TimeGranularity_MINUTE,
  TimeGranularity_MONTH,
  TimeGranularity_QUARTER,
  TimeGranularity_SECOND,
  TimeGranularity_WEEK,
  TimeGranularity_YEAR,
  TimeGranularity'
  #-}
