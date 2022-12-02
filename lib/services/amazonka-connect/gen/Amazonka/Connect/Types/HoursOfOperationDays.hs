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
-- Module      : Amazonka.Connect.Types.HoursOfOperationDays
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationDays
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HoursOfOperationDays = HoursOfOperationDays'
  { fromHoursOfOperationDays ::
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
