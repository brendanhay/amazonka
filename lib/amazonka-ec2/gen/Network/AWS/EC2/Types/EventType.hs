{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EventType
  ( EventType
      ( EventType',
        EventTypeInstanceChange,
        EventTypeFleetRequestChange,
        EventTypeError,
        EventTypeInformation,
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

pattern EventTypeInstanceChange :: EventType
pattern EventTypeInstanceChange = EventType' "instanceChange"

pattern EventTypeFleetRequestChange :: EventType
pattern EventTypeFleetRequestChange = EventType' "fleetRequestChange"

pattern EventTypeError :: EventType
pattern EventTypeError = EventType' "error"

pattern EventTypeInformation :: EventType
pattern EventTypeInformation = EventType' "information"

{-# COMPLETE
  EventTypeInstanceChange,
  EventTypeFleetRequestChange,
  EventTypeError,
  EventTypeInformation,
  EventType'
  #-}
