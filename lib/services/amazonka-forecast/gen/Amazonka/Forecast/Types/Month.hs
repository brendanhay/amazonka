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
-- Module      : Amazonka.Forecast.Types.Month
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Month
  ( Month
      ( ..,
        Month_APRIL,
        Month_AUGUST,
        Month_DECEMBER,
        Month_FEBRUARY,
        Month_JANUARY,
        Month_JULY,
        Month_JUNE,
        Month_MARCH,
        Month_MAY,
        Month_NOVEMBER,
        Month_OCTOBER,
        Month_SEPTEMBER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Month = Month' {fromMonth :: Data.Text}
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

pattern Month_APRIL :: Month
pattern Month_APRIL = Month' "APRIL"

pattern Month_AUGUST :: Month
pattern Month_AUGUST = Month' "AUGUST"

pattern Month_DECEMBER :: Month
pattern Month_DECEMBER = Month' "DECEMBER"

pattern Month_FEBRUARY :: Month
pattern Month_FEBRUARY = Month' "FEBRUARY"

pattern Month_JANUARY :: Month
pattern Month_JANUARY = Month' "JANUARY"

pattern Month_JULY :: Month
pattern Month_JULY = Month' "JULY"

pattern Month_JUNE :: Month
pattern Month_JUNE = Month' "JUNE"

pattern Month_MARCH :: Month
pattern Month_MARCH = Month' "MARCH"

pattern Month_MAY :: Month
pattern Month_MAY = Month' "MAY"

pattern Month_NOVEMBER :: Month
pattern Month_NOVEMBER = Month' "NOVEMBER"

pattern Month_OCTOBER :: Month
pattern Month_OCTOBER = Month' "OCTOBER"

pattern Month_SEPTEMBER :: Month
pattern Month_SEPTEMBER = Month' "SEPTEMBER"

{-# COMPLETE
  Month_APRIL,
  Month_AUGUST,
  Month_DECEMBER,
  Month_FEBRUARY,
  Month_JANUARY,
  Month_JULY,
  Month_JUNE,
  Month_MARCH,
  Month_MAY,
  Month_NOVEMBER,
  Month_OCTOBER,
  Month_SEPTEMBER,
  Month'
  #-}
