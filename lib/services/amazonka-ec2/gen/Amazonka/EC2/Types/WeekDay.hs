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
-- Module      : Amazonka.EC2.Types.WeekDay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.WeekDay
  ( WeekDay
      ( ..,
        WeekDay_Friday,
        WeekDay_Monday,
        WeekDay_Saturday,
        WeekDay_Sunday,
        WeekDay_Thursday,
        WeekDay_Tuesday,
        WeekDay_Wednesday
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype WeekDay = WeekDay' {fromWeekDay :: Data.Text}
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

pattern WeekDay_Friday :: WeekDay
pattern WeekDay_Friday = WeekDay' "friday"

pattern WeekDay_Monday :: WeekDay
pattern WeekDay_Monday = WeekDay' "monday"

pattern WeekDay_Saturday :: WeekDay
pattern WeekDay_Saturday = WeekDay' "saturday"

pattern WeekDay_Sunday :: WeekDay
pattern WeekDay_Sunday = WeekDay' "sunday"

pattern WeekDay_Thursday :: WeekDay
pattern WeekDay_Thursday = WeekDay' "thursday"

pattern WeekDay_Tuesday :: WeekDay
pattern WeekDay_Tuesday = WeekDay' "tuesday"

pattern WeekDay_Wednesday :: WeekDay
pattern WeekDay_Wednesday = WeekDay' "wednesday"

{-# COMPLETE
  WeekDay_Friday,
  WeekDay_Monday,
  WeekDay_Saturday,
  WeekDay_Sunday,
  WeekDay_Thursday,
  WeekDay_Tuesday,
  WeekDay_Wednesday,
  WeekDay'
  #-}
