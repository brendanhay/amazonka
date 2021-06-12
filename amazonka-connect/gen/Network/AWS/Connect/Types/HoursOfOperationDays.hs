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
-- Module      : Network.AWS.Connect.Types.HoursOfOperationDays
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperationDays
  ( HoursOfOperationDays
      ( ..,
        HoursOfOperationDays_FRIDAY,
        HoursOfOperationDays_MONDAY,
        HoursOfOperationDays_SATURDAY,
        HoursOfOperationDays_SUNDAY,
        HoursOfOperationDays_THURSDAY,
        HoursOfOperationDays_TUESDAY,
        HoursOfOperationDays_WEDNESDAY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HoursOfOperationDays = HoursOfOperationDays'
  { fromHoursOfOperationDays ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern HoursOfOperationDays_FRIDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_FRIDAY = HoursOfOperationDays' "FRIDAY"

pattern HoursOfOperationDays_MONDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_MONDAY = HoursOfOperationDays' "MONDAY"

pattern HoursOfOperationDays_SATURDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_SATURDAY = HoursOfOperationDays' "SATURDAY"

pattern HoursOfOperationDays_SUNDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_SUNDAY = HoursOfOperationDays' "SUNDAY"

pattern HoursOfOperationDays_THURSDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_THURSDAY = HoursOfOperationDays' "THURSDAY"

pattern HoursOfOperationDays_TUESDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_TUESDAY = HoursOfOperationDays' "TUESDAY"

pattern HoursOfOperationDays_WEDNESDAY :: HoursOfOperationDays
pattern HoursOfOperationDays_WEDNESDAY = HoursOfOperationDays' "WEDNESDAY"

{-# COMPLETE
  HoursOfOperationDays_FRIDAY,
  HoursOfOperationDays_MONDAY,
  HoursOfOperationDays_SATURDAY,
  HoursOfOperationDays_SUNDAY,
  HoursOfOperationDays_THURSDAY,
  HoursOfOperationDays_TUESDAY,
  HoursOfOperationDays_WEDNESDAY,
  HoursOfOperationDays'
  #-}
