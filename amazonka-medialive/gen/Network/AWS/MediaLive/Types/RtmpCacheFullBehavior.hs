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
-- Module      : Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
  ( RtmpCacheFullBehavior
      ( ..,
        RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY,
        RtmpCacheFullBehavior_WAIT_FOR_SERVER
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Rtmp Cache Full Behavior
newtype RtmpCacheFullBehavior = RtmpCacheFullBehavior'
  { fromRtmpCacheFullBehavior ::
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

pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY = RtmpCacheFullBehavior' "DISCONNECT_IMMEDIATELY"

pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER = RtmpCacheFullBehavior' "WAIT_FOR_SERVER"

{-# COMPLETE
  RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY,
  RtmpCacheFullBehavior_WAIT_FOR_SERVER,
  RtmpCacheFullBehavior'
  #-}
