{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
  ( SmoothGroupEventIdMode
      ( SmoothGroupEventIdMode',
        NoEventId,
        UseConfigured,
        UseTimestamp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Smooth Group Event Id Mode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode' Lude.Text
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

pattern NoEventId :: SmoothGroupEventIdMode
pattern NoEventId = SmoothGroupEventIdMode' "NO_EVENT_ID"

pattern UseConfigured :: SmoothGroupEventIdMode
pattern UseConfigured = SmoothGroupEventIdMode' "USE_CONFIGURED"

pattern UseTimestamp :: SmoothGroupEventIdMode
pattern UseTimestamp = SmoothGroupEventIdMode' "USE_TIMESTAMP"

{-# COMPLETE
  NoEventId,
  UseConfigured,
  UseTimestamp,
  SmoothGroupEventIdMode'
  #-}
