-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
  ( RtmpCacheFullBehavior
      ( RtmpCacheFullBehavior',
        DisconnectImmediately,
        WaitForServer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Rtmp Cache Full Behavior
newtype RtmpCacheFullBehavior = RtmpCacheFullBehavior' Lude.Text
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

pattern DisconnectImmediately :: RtmpCacheFullBehavior
pattern DisconnectImmediately = RtmpCacheFullBehavior' "DISCONNECT_IMMEDIATELY"

pattern WaitForServer :: RtmpCacheFullBehavior
pattern WaitForServer = RtmpCacheFullBehavior' "WAIT_FOR_SERVER"

{-# COMPLETE
  DisconnectImmediately,
  WaitForServer,
  RtmpCacheFullBehavior'
  #-}
