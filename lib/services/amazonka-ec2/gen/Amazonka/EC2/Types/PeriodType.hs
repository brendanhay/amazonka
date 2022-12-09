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
-- Module      : Amazonka.EC2.Types.PeriodType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PeriodType
  ( PeriodType
      ( ..,
        PeriodType_Fifteen_minutes,
        PeriodType_Five_minutes,
        PeriodType_One_day,
        PeriodType_One_hour,
        PeriodType_One_week,
        PeriodType_Three_hours
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype PeriodType = PeriodType'
  { fromPeriodType ::
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

pattern PeriodType_Fifteen_minutes :: PeriodType
pattern PeriodType_Fifteen_minutes = PeriodType' "fifteen-minutes"

pattern PeriodType_Five_minutes :: PeriodType
pattern PeriodType_Five_minutes = PeriodType' "five-minutes"

pattern PeriodType_One_day :: PeriodType
pattern PeriodType_One_day = PeriodType' "one-day"

pattern PeriodType_One_hour :: PeriodType
pattern PeriodType_One_hour = PeriodType' "one-hour"

pattern PeriodType_One_week :: PeriodType
pattern PeriodType_One_week = PeriodType' "one-week"

pattern PeriodType_Three_hours :: PeriodType
pattern PeriodType_Three_hours = PeriodType' "three-hours"

{-# COMPLETE
  PeriodType_Fifteen_minutes,
  PeriodType_Five_minutes,
  PeriodType_One_day,
  PeriodType_One_hour,
  PeriodType_One_week,
  PeriodType_Three_hours,
  PeriodType'
  #-}
