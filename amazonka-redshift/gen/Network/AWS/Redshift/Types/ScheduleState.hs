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
-- Module      : Network.AWS.Redshift.Types.ScheduleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduleState
  ( ScheduleState
      ( ..,
        ScheduleState_ACTIVE,
        ScheduleState_FAILED,
        ScheduleState_MODIFYING
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Redshift.Internal

newtype ScheduleState = ScheduleState'
  { fromScheduleState ::
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

pattern ScheduleState_ACTIVE :: ScheduleState
pattern ScheduleState_ACTIVE = ScheduleState' "ACTIVE"

pattern ScheduleState_FAILED :: ScheduleState
pattern ScheduleState_FAILED = ScheduleState' "FAILED"

pattern ScheduleState_MODIFYING :: ScheduleState
pattern ScheduleState_MODIFYING = ScheduleState' "MODIFYING"

{-# COMPLETE
  ScheduleState_ACTIVE,
  ScheduleState_FAILED,
  ScheduleState_MODIFYING,
  ScheduleState'
  #-}
