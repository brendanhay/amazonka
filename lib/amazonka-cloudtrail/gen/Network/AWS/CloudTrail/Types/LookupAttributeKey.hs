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
        AccessKeyId,
        EventId,
        EventName,
        EventSource,
        ReadOnly,
        ResourceName,
        ResourceType,
        Username
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LookupAttributeKey = LookupAttributeKey' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AccessKeyId :: LookupAttributeKey
pattern AccessKeyId = LookupAttributeKey' "AccessKeyId"

pattern EventId :: LookupAttributeKey
pattern EventId = LookupAttributeKey' "EventId"

pattern EventName :: LookupAttributeKey
pattern EventName = LookupAttributeKey' "EventName"

pattern EventSource :: LookupAttributeKey
pattern EventSource = LookupAttributeKey' "EventSource"

pattern ReadOnly :: LookupAttributeKey
pattern ReadOnly = LookupAttributeKey' "ReadOnly"

pattern ResourceName :: LookupAttributeKey
pattern ResourceName = LookupAttributeKey' "ResourceName"

pattern ResourceType :: LookupAttributeKey
pattern ResourceType = LookupAttributeKey' "ResourceType"

pattern Username :: LookupAttributeKey
pattern Username = LookupAttributeKey' "Username"

{-# COMPLETE
  AccessKeyId,
  EventId,
  EventName,
  EventSource,
  ReadOnly,
  ResourceName,
  ResourceType,
  Username,
  LookupAttributeKey'
  #-}
