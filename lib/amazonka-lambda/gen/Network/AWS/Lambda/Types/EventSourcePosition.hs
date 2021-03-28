{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.EventSourcePosition
  ( EventSourcePosition
    ( EventSourcePosition'
    , EventSourcePositionTrimHorizon
    , EventSourcePositionLatest
    , EventSourcePositionAtTimestamp
    , fromEventSourcePosition
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventSourcePosition = EventSourcePosition'{fromEventSourcePosition
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern EventSourcePositionTrimHorizon :: EventSourcePosition
pattern EventSourcePositionTrimHorizon = EventSourcePosition' "TRIM_HORIZON"

pattern EventSourcePositionLatest :: EventSourcePosition
pattern EventSourcePositionLatest = EventSourcePosition' "LATEST"

pattern EventSourcePositionAtTimestamp :: EventSourcePosition
pattern EventSourcePositionAtTimestamp = EventSourcePosition' "AT_TIMESTAMP"

{-# COMPLETE 
  EventSourcePositionTrimHorizon,

  EventSourcePositionLatest,

  EventSourcePositionAtTimestamp,
  EventSourcePosition'
  #-}
