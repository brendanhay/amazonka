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
-- Module      : Network.AWS.LexModels.Types.ChannelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ChannelStatus
  ( ChannelStatus
      ( ..,
        ChannelStatus_CREATED,
        ChannelStatus_FAILED,
        ChannelStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ChannelStatus = ChannelStatus'
  { fromChannelStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ChannelStatus_CREATED :: ChannelStatus
pattern ChannelStatus_CREATED = ChannelStatus' "CREATED"

pattern ChannelStatus_FAILED :: ChannelStatus
pattern ChannelStatus_FAILED = ChannelStatus' "FAILED"

pattern ChannelStatus_IN_PROGRESS :: ChannelStatus
pattern ChannelStatus_IN_PROGRESS = ChannelStatus' "IN_PROGRESS"

{-# COMPLETE
  ChannelStatus_CREATED,
  ChannelStatus_FAILED,
  ChannelStatus_IN_PROGRESS,
  ChannelStatus'
  #-}
