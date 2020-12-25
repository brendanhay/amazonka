{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EventType
  ( EventType
      ( EventType',
        EventTypeViewerRequest,
        EventTypeViewerResponse,
        EventTypeOriginRequest,
        EventTypeOriginResponse,
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

pattern EventTypeViewerRequest :: EventType
pattern EventTypeViewerRequest = EventType' "viewer-request"

pattern EventTypeViewerResponse :: EventType
pattern EventTypeViewerResponse = EventType' "viewer-response"

pattern EventTypeOriginRequest :: EventType
pattern EventTypeOriginRequest = EventType' "origin-request"

pattern EventTypeOriginResponse :: EventType
pattern EventTypeOriginResponse = EventType' "origin-response"

{-# COMPLETE
  EventTypeViewerRequest,
  EventTypeViewerResponse,
  EventTypeOriginRequest,
  EventTypeOriginResponse,
  EventType'
  #-}
