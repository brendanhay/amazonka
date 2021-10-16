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
-- Module      : Network.AWS.EC2.Types.WeekDay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.WeekDay
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype WeekDay = WeekDay' {fromWeekDay :: Core.Text}
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
