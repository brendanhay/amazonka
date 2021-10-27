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
-- Module      : Network.AWS.ChimeSDKMessaging.Types.ChannelMessageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ChimeSDKMessaging.Types.ChannelMessageStatus
  ( ChannelMessageStatus
      ( ..,
        ChannelMessageStatus_DENIED,
        ChannelMessageStatus_FAILED,
        ChannelMessageStatus_PENDING,
        ChannelMessageStatus_SENT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ChannelMessageStatus = ChannelMessageStatus'
  { fromChannelMessageStatus ::
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

pattern ChannelMessageStatus_DENIED :: ChannelMessageStatus
pattern ChannelMessageStatus_DENIED = ChannelMessageStatus' "DENIED"

pattern ChannelMessageStatus_FAILED :: ChannelMessageStatus
pattern ChannelMessageStatus_FAILED = ChannelMessageStatus' "FAILED"

pattern ChannelMessageStatus_PENDING :: ChannelMessageStatus
pattern ChannelMessageStatus_PENDING = ChannelMessageStatus' "PENDING"

pattern ChannelMessageStatus_SENT :: ChannelMessageStatus
pattern ChannelMessageStatus_SENT = ChannelMessageStatus' "SENT"

{-# COMPLETE
  ChannelMessageStatus_DENIED,
  ChannelMessageStatus_FAILED,
  ChannelMessageStatus_PENDING,
  ChannelMessageStatus_SENT,
  ChannelMessageStatus'
  #-}
