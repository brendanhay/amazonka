{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.EventType
  ( EventType
      ( EventType',
        EventTypeSystem,
        EventTypeCreateAction,
        EventTypeDeleteAction,
        EventTypeUpdateAction,
        EventTypeExecuteAction,
        fromEventType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EventType = EventType' {fromEventType :: Core.Text}
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

pattern EventTypeSystem :: EventType
pattern EventTypeSystem = EventType' "SYSTEM"

pattern EventTypeCreateAction :: EventType
pattern EventTypeCreateAction = EventType' "CREATE_ACTION"

pattern EventTypeDeleteAction :: EventType
pattern EventTypeDeleteAction = EventType' "DELETE_ACTION"

pattern EventTypeUpdateAction :: EventType
pattern EventTypeUpdateAction = EventType' "UPDATE_ACTION"

pattern EventTypeExecuteAction :: EventType
pattern EventTypeExecuteAction = EventType' "EXECUTE_ACTION"

{-# COMPLETE
  EventTypeSystem,
  EventTypeCreateAction,
  EventTypeDeleteAction,
  EventTypeUpdateAction,
  EventTypeExecuteAction,
  EventType'
  #-}
