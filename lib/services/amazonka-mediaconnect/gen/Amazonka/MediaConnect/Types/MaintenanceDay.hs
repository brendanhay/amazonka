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
-- Module      : Amazonka.MediaConnect.Types.MaintenanceDay
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MaintenanceDay
  ( MaintenanceDay
      ( ..,
        MaintenanceDay_Friday,
        MaintenanceDay_Monday,
        MaintenanceDay_Saturday,
        MaintenanceDay_Sunday,
        MaintenanceDay_Thursday,
        MaintenanceDay_Tuesday,
        MaintenanceDay_Wednesday
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

pattern MaintenanceDay_Friday :: MaintenanceDay
pattern MaintenanceDay_Friday = MaintenanceDay' "Friday"

pattern MaintenanceDay_Monday :: MaintenanceDay
pattern MaintenanceDay_Monday = MaintenanceDay' "Monday"

pattern MaintenanceDay_Saturday :: MaintenanceDay
pattern MaintenanceDay_Saturday = MaintenanceDay' "Saturday"

pattern MaintenanceDay_Sunday :: MaintenanceDay
pattern MaintenanceDay_Sunday = MaintenanceDay' "Sunday"

pattern MaintenanceDay_Thursday :: MaintenanceDay
pattern MaintenanceDay_Thursday = MaintenanceDay' "Thursday"

pattern MaintenanceDay_Tuesday :: MaintenanceDay
pattern MaintenanceDay_Tuesday = MaintenanceDay' "Tuesday"

pattern MaintenanceDay_Wednesday :: MaintenanceDay
pattern MaintenanceDay_Wednesday = MaintenanceDay' "Wednesday"

{-# COMPLETE
  MaintenanceDay_Friday,
  MaintenanceDay_Monday,
  MaintenanceDay_Saturday,
  MaintenanceDay_Sunday,
  MaintenanceDay_Thursday,
  MaintenanceDay_Tuesday,
  MaintenanceDay_Wednesday,
  MaintenanceDay'
  #-}
