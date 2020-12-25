{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduleState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduleState
  ( ScheduleState
      ( ScheduleState',
        ScheduleStateModifying,
        ScheduleStateActive,
        ScheduleStateFailed,
        fromScheduleState
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

newtype ScheduleState = ScheduleState'
  { fromScheduleState ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScheduleStateModifying :: ScheduleState
pattern ScheduleStateModifying = ScheduleState' "MODIFYING"

pattern ScheduleStateActive :: ScheduleState
pattern ScheduleStateActive = ScheduleState' "ACTIVE"

pattern ScheduleStateFailed :: ScheduleState
pattern ScheduleStateFailed = ScheduleState' "FAILED"

{-# COMPLETE
  ScheduleStateModifying,
  ScheduleStateActive,
  ScheduleStateFailed,
  ScheduleState'
  #-}
