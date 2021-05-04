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
-- Module      : Network.AWS.CloudTrail.Types.LookupAttributeKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttributeKey
  ( LookupAttributeKey
      ( ..,
        LookupAttributeKey_AccessKeyId,
        LookupAttributeKey_EventId,
        LookupAttributeKey_EventName,
        LookupAttributeKey_EventSource,
        LookupAttributeKey_ReadOnly,
        LookupAttributeKey_ResourceName,
        LookupAttributeKey_ResourceType,
        LookupAttributeKey_Username
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LookupAttributeKey = LookupAttributeKey'
  { fromLookupAttributeKey ::
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

pattern LookupAttributeKey_AccessKeyId :: LookupAttributeKey
pattern LookupAttributeKey_AccessKeyId = LookupAttributeKey' "AccessKeyId"

pattern LookupAttributeKey_EventId :: LookupAttributeKey
pattern LookupAttributeKey_EventId = LookupAttributeKey' "EventId"

pattern LookupAttributeKey_EventName :: LookupAttributeKey
pattern LookupAttributeKey_EventName = LookupAttributeKey' "EventName"

pattern LookupAttributeKey_EventSource :: LookupAttributeKey
pattern LookupAttributeKey_EventSource = LookupAttributeKey' "EventSource"

pattern LookupAttributeKey_ReadOnly :: LookupAttributeKey
pattern LookupAttributeKey_ReadOnly = LookupAttributeKey' "ReadOnly"

pattern LookupAttributeKey_ResourceName :: LookupAttributeKey
pattern LookupAttributeKey_ResourceName = LookupAttributeKey' "ResourceName"

pattern LookupAttributeKey_ResourceType :: LookupAttributeKey
pattern LookupAttributeKey_ResourceType = LookupAttributeKey' "ResourceType"

pattern LookupAttributeKey_Username :: LookupAttributeKey
pattern LookupAttributeKey_Username = LookupAttributeKey' "Username"

{-# COMPLETE
  LookupAttributeKey_AccessKeyId,
  LookupAttributeKey_EventId,
  LookupAttributeKey_EventName,
  LookupAttributeKey_EventSource,
  LookupAttributeKey_ReadOnly,
  LookupAttributeKey_ResourceName,
  LookupAttributeKey_ResourceType,
  LookupAttributeKey_Username,
  LookupAttributeKey'
  #-}
