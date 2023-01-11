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
-- Module      : Amazonka.MediaLive.Types.MaintenanceDay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MaintenanceDay
  ( MaintenanceDay
      ( ..,
        MaintenanceDay_FRIDAY,
        MaintenanceDay_MONDAY,
        MaintenanceDay_SATURDAY,
        MaintenanceDay_SUNDAY,
        MaintenanceDay_THURSDAY,
        MaintenanceDay_TUESDAY,
        MaintenanceDay_WEDNESDAY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The currently selected maintenance day.
newtype MaintenanceDay = MaintenanceDay'
  { fromMaintenanceDay ::
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

pattern MaintenanceDay_FRIDAY :: MaintenanceDay
pattern MaintenanceDay_FRIDAY = MaintenanceDay' "FRIDAY"

pattern MaintenanceDay_MONDAY :: MaintenanceDay
pattern MaintenanceDay_MONDAY = MaintenanceDay' "MONDAY"

pattern MaintenanceDay_SATURDAY :: MaintenanceDay
pattern MaintenanceDay_SATURDAY = MaintenanceDay' "SATURDAY"

pattern MaintenanceDay_SUNDAY :: MaintenanceDay
pattern MaintenanceDay_SUNDAY = MaintenanceDay' "SUNDAY"

pattern MaintenanceDay_THURSDAY :: MaintenanceDay
pattern MaintenanceDay_THURSDAY = MaintenanceDay' "THURSDAY"

pattern MaintenanceDay_TUESDAY :: MaintenanceDay
pattern MaintenanceDay_TUESDAY = MaintenanceDay' "TUESDAY"

pattern MaintenanceDay_WEDNESDAY :: MaintenanceDay
pattern MaintenanceDay_WEDNESDAY = MaintenanceDay' "WEDNESDAY"

{-# COMPLETE
  MaintenanceDay_FRIDAY,
  MaintenanceDay_MONDAY,
  MaintenanceDay_SATURDAY,
  MaintenanceDay_SUNDAY,
  MaintenanceDay_THURSDAY,
  MaintenanceDay_TUESDAY,
  MaintenanceDay_WEDNESDAY,
  MaintenanceDay'
  #-}
