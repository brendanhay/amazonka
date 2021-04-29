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
-- Module      : Network.AWS.SageMaker.Types.ScheduleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleStatus
  ( ScheduleStatus
      ( ..,
        ScheduleStatus_Failed,
        ScheduleStatus_Pending,
        ScheduleStatus_Scheduled,
        ScheduleStatus_Stopped
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ScheduleStatus = ScheduleStatus'
  { fromScheduleStatus ::
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
