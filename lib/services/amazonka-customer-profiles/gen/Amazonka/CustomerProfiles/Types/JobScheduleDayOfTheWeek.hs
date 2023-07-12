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
-- Module      : Amazonka.CustomerProfiles.Types.JobScheduleDayOfTheWeek
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.JobScheduleDayOfTheWeek
  ( JobScheduleDayOfTheWeek
      ( ..,
        JobScheduleDayOfTheWeek_FRIDAY,
        JobScheduleDayOfTheWeek_MONDAY,
        JobScheduleDayOfTheWeek_SATURDAY,
        JobScheduleDayOfTheWeek_SUNDAY,
        JobScheduleDayOfTheWeek_THURSDAY,
        JobScheduleDayOfTheWeek_TUESDAY,
        JobScheduleDayOfTheWeek_WEDNESDAY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobScheduleDayOfTheWeek = JobScheduleDayOfTheWeek'
  { fromJobScheduleDayOfTheWeek ::
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

pattern JobScheduleDayOfTheWeek_FRIDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_FRIDAY = JobScheduleDayOfTheWeek' "FRIDAY"

pattern JobScheduleDayOfTheWeek_MONDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_MONDAY = JobScheduleDayOfTheWeek' "MONDAY"

pattern JobScheduleDayOfTheWeek_SATURDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_SATURDAY = JobScheduleDayOfTheWeek' "SATURDAY"

pattern JobScheduleDayOfTheWeek_SUNDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_SUNDAY = JobScheduleDayOfTheWeek' "SUNDAY"

pattern JobScheduleDayOfTheWeek_THURSDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_THURSDAY = JobScheduleDayOfTheWeek' "THURSDAY"

pattern JobScheduleDayOfTheWeek_TUESDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_TUESDAY = JobScheduleDayOfTheWeek' "TUESDAY"

pattern JobScheduleDayOfTheWeek_WEDNESDAY :: JobScheduleDayOfTheWeek
pattern JobScheduleDayOfTheWeek_WEDNESDAY = JobScheduleDayOfTheWeek' "WEDNESDAY"

{-# COMPLETE
  JobScheduleDayOfTheWeek_FRIDAY,
  JobScheduleDayOfTheWeek_MONDAY,
  JobScheduleDayOfTheWeek_SATURDAY,
  JobScheduleDayOfTheWeek_SUNDAY,
  JobScheduleDayOfTheWeek_THURSDAY,
  JobScheduleDayOfTheWeek_TUESDAY,
  JobScheduleDayOfTheWeek_WEDNESDAY,
  JobScheduleDayOfTheWeek'
  #-}
