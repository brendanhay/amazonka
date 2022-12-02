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
-- Module      : Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.ChannelMessageStatus
  ( ChannelMessageStatus
      ( ..,
        ChannelMessageStatus_DENIED,
        ChannelMessageStatus_FAILED,
        ChannelMessageStatus_PENDING,
        ChannelMessageStatus_SENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChannelMessageStatus = ChannelMessageStatus'
  { fromChannelMessageStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
