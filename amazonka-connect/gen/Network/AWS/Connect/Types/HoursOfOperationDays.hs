{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype HoursOfOperationDays = HoursOfOperationDays'
  { fromHoursOfOperationDays ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
