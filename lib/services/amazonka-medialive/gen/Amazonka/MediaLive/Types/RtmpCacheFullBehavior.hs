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
-- Module      : Amazonka.MediaLive.Types.RtmpCacheFullBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RtmpCacheFullBehavior
  ( RtmpCacheFullBehavior
      ( ..,
        RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY,
        RtmpCacheFullBehavior_WAIT_FOR_SERVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Rtmp Cache Full Behavior
newtype RtmpCacheFullBehavior = RtmpCacheFullBehavior'
  { fromRtmpCacheFullBehavior ::
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

pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY = RtmpCacheFullBehavior' "DISCONNECT_IMMEDIATELY"

pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER :: RtmpCacheFullBehavior
pattern RtmpCacheFullBehavior_WAIT_FOR_SERVER = RtmpCacheFullBehavior' "WAIT_FOR_SERVER"

{-# COMPLETE
  RtmpCacheFullBehavior_DISCONNECT_IMMEDIATELY,
  RtmpCacheFullBehavior_WAIT_FOR_SERVER,
  RtmpCacheFullBehavior'
  #-}
