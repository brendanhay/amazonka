{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ChannelStatus
  ( ChannelStatus
      ( ChannelStatus',
        ChannelStatusInProgress,
        ChannelStatusCreated,
        ChannelStatusFailed,
        fromChannelStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ChannelStatus = ChannelStatus'
  { fromChannelStatus ::
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

pattern ChannelStatusInProgress :: ChannelStatus
pattern ChannelStatusInProgress = ChannelStatus' "IN_PROGRESS"

pattern ChannelStatusCreated :: ChannelStatus
pattern ChannelStatusCreated = ChannelStatus' "CREATED"

pattern ChannelStatusFailed :: ChannelStatus
pattern ChannelStatusFailed = ChannelStatus' "FAILED"

{-# COMPLETE
  ChannelStatusInProgress,
  ChannelStatusCreated,
  ChannelStatusFailed,
  ChannelStatus'
  #-}
