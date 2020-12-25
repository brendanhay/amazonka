{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264LookAheadRateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264LookAheadRateControl
  ( H264LookAheadRateControl
      ( H264LookAheadRateControl',
        H264LookAheadRateControlHigh,
        H264LookAheadRateControlLow,
        H264LookAheadRateControlMedium,
        fromH264LookAheadRateControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Look Ahead Rate Control
newtype H264LookAheadRateControl = H264LookAheadRateControl'
  { fromH264LookAheadRateControl ::
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

pattern H264LookAheadRateControlHigh :: H264LookAheadRateControl
pattern H264LookAheadRateControlHigh = H264LookAheadRateControl' "HIGH"

pattern H264LookAheadRateControlLow :: H264LookAheadRateControl
pattern H264LookAheadRateControlLow = H264LookAheadRateControl' "LOW"

pattern H264LookAheadRateControlMedium :: H264LookAheadRateControl
pattern H264LookAheadRateControlMedium = H264LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  H264LookAheadRateControlHigh,
  H264LookAheadRateControlLow,
  H264LookAheadRateControlMedium,
  H264LookAheadRateControl'
  #-}
