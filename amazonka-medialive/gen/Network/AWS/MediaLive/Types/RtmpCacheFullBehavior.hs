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

import qualified Network.AWS.Prelude as Prelude

-- | Rtmp Cache Full Behavior
newtype RtmpCacheFullBehavior = RtmpCacheFullBehavior'
  { fromRtmpCacheFullBehavior ::
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

pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY = RtmpCacheFullBehavior' "DISCONNECT_IMMEDIATELY"

pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER = RtmpCacheFullBehavior' "WAIT_FOR_SERVER"

{-# COMPLETE
  RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY,
  RtmpCacheFullBehavior_WAIT_FOR_SERVER,
  RtmpCacheFullBehavior'
  #-}
