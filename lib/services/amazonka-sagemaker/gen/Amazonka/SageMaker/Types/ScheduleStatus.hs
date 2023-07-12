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
-- Module      : Amazonka.SageMaker.Types.ScheduleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ScheduleStatus
  ( ScheduleStatus
      ( ..,
        ScheduleStatus_Failed,
        ScheduleStatus_Pending,
        ScheduleStatus_Scheduled,
        ScheduleStatus_Stopped
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleStatus = ScheduleStatus'
  { fromScheduleStatus ::
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

pattern ScheduleStatus_Failed :: ScheduleStatus
pattern ScheduleStatus_Failed = ScheduleStatus' "Failed"

pattern ScheduleStatus_Pending :: ScheduleStatus
pattern ScheduleStatus_Pending = ScheduleStatus' "Pending"

pattern ScheduleStatus_Scheduled :: ScheduleStatus
pattern ScheduleStatus_Scheduled = ScheduleStatus' "Scheduled"

pattern ScheduleStatus_Stopped :: ScheduleStatus
pattern ScheduleStatus_Stopped = ScheduleStatus' "Stopped"

{-# COMPLETE
  ScheduleStatus_Failed,
  ScheduleStatus_Pending,
  ScheduleStatus_Scheduled,
  ScheduleStatus_Stopped,
  ScheduleStatus'
  #-}
