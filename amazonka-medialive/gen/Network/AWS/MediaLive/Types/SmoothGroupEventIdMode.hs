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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
  ( SmoothGroupEventIdMode
      ( ..,
        SmoothGroupEventIdMode_NO_EVENT_ID,
        SmoothGroupEventIdMode_USE_CONFIGURED,
        SmoothGroupEventIdMode_USE_TIMESTAMP
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Smooth Group Event Id Mode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode'
  { fromSmoothGroupEventIdMode ::
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

pattern SmoothGroupEventIdMode_NO_EVENT_ID :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_NO_EVENT_ID = SmoothGroupEventIdMode' "NO_EVENT_ID"

pattern SmoothGroupEventIdMode_USE_CONFIGURED :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_USE_CONFIGURED = SmoothGroupEventIdMode' "USE_CONFIGURED"

pattern SmoothGroupEventIdMode_USE_TIMESTAMP :: SmoothGroupEventIdMode
pattern SmoothGroupEventIdMode_USE_TIMESTAMP = SmoothGroupEventIdMode' "USE_TIMESTAMP"

{-# COMPLETE
  SmoothGroupEventIdMode_NO_EVENT_ID,
  SmoothGroupEventIdMode_USE_CONFIGURED,
  SmoothGroupEventIdMode_USE_TIMESTAMP,
  SmoothGroupEventIdMode'
  #-}
