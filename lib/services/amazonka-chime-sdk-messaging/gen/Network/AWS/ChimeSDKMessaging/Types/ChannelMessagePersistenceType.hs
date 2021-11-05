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
-- Module      : Network.AWS.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ChimeSDKMessaging.Types.ChannelMessagePersistenceType
  ( ChannelMessagePersistenceType
      ( ..,
        ChannelMessagePersistenceType_NON_PERSISTENT,
        ChannelMessagePersistenceType_PERSISTENT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ChannelMessagePersistenceType = ChannelMessagePersistenceType'
  { fromChannelMessagePersistenceType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern ChannelMessagePersistenceType_NON_PERSISTENT :: ChannelMessagePersistenceType
pattern ChannelMessagePersistenceType_NON_PERSISTENT = ChannelMessagePersistenceType' "NON_PERSISTENT"

pattern ChannelMessagePersistenceType_PERSISTENT :: ChannelMessagePersistenceType
pattern ChannelMessagePersistenceType_PERSISTENT = ChannelMessagePersistenceType' "PERSISTENT"

{-# COMPLETE
  ChannelMessagePersistenceType_NON_PERSISTENT,
  ChannelMessagePersistenceType_PERSISTENT,
  ChannelMessagePersistenceType'
  #-}
