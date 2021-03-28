{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CalendarState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CalendarState
  ( CalendarState
    ( CalendarState'
    , CalendarStateOpen
    , CalendarStateClosed
    , fromCalendarState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CalendarState = CalendarState'{fromCalendarState ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern CalendarStateOpen :: CalendarState
pattern CalendarStateOpen = CalendarState' "OPEN"

pattern CalendarStateClosed :: CalendarState
pattern CalendarStateClosed = CalendarState' "CLOSED"

{-# COMPLETE 
  CalendarStateOpen,

  CalendarStateClosed,
  CalendarState'
  #-}
