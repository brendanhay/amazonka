{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.LookupAttributeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttributeKey
  ( LookupAttributeKey
      ( LookupAttributeKey',
        LookupAttributeKeyEventId,
        LookupAttributeKeyEventName,
        LookupAttributeKeyReadOnly,
        LookupAttributeKeyUsername,
        LookupAttributeKeyResourceType,
        LookupAttributeKeyResourceName,
        LookupAttributeKeyEventSource,
        LookupAttributeKeyAccessKeyId,
        fromLookupAttributeKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LookupAttributeKey = LookupAttributeKey'
  { fromLookupAttributeKey ::
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

pattern LookupAttributeKeyEventId :: LookupAttributeKey
pattern LookupAttributeKeyEventId = LookupAttributeKey' "EventId"

pattern LookupAttributeKeyEventName :: LookupAttributeKey
pattern LookupAttributeKeyEventName = LookupAttributeKey' "EventName"

pattern LookupAttributeKeyReadOnly :: LookupAttributeKey
pattern LookupAttributeKeyReadOnly = LookupAttributeKey' "ReadOnly"

pattern LookupAttributeKeyUsername :: LookupAttributeKey
pattern LookupAttributeKeyUsername = LookupAttributeKey' "Username"

pattern LookupAttributeKeyResourceType :: LookupAttributeKey
pattern LookupAttributeKeyResourceType = LookupAttributeKey' "ResourceType"

pattern LookupAttributeKeyResourceName :: LookupAttributeKey
pattern LookupAttributeKeyResourceName = LookupAttributeKey' "ResourceName"

pattern LookupAttributeKeyEventSource :: LookupAttributeKey
pattern LookupAttributeKeyEventSource = LookupAttributeKey' "EventSource"

pattern LookupAttributeKeyAccessKeyId :: LookupAttributeKey
pattern LookupAttributeKeyAccessKeyId = LookupAttributeKey' "AccessKeyId"

{-# COMPLETE
  LookupAttributeKeyEventId,
  LookupAttributeKeyEventName,
  LookupAttributeKeyReadOnly,
  LookupAttributeKeyUsername,
  LookupAttributeKeyResourceType,
  LookupAttributeKeyResourceName,
  LookupAttributeKeyEventSource,
  LookupAttributeKeyAccessKeyId,
  LookupAttributeKey'
  #-}
