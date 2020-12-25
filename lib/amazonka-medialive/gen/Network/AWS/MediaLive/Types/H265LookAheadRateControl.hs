{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265LookAheadRateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265LookAheadRateControl
  ( H265LookAheadRateControl
      ( H265LookAheadRateControl',
        H265LookAheadRateControlHigh,
        H265LookAheadRateControlLow,
        H265LookAheadRateControlMedium,
        fromH265LookAheadRateControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H265 Look Ahead Rate Control
newtype H265LookAheadRateControl = H265LookAheadRateControl'
  { fromH265LookAheadRateControl ::
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

pattern H265LookAheadRateControlHigh :: H265LookAheadRateControl
pattern H265LookAheadRateControlHigh = H265LookAheadRateControl' "HIGH"

pattern H265LookAheadRateControlLow :: H265LookAheadRateControl
pattern H265LookAheadRateControlLow = H265LookAheadRateControl' "LOW"

pattern H265LookAheadRateControlMedium :: H265LookAheadRateControl
pattern H265LookAheadRateControlMedium = H265LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  H265LookAheadRateControlHigh,
  H265LookAheadRateControlLow,
  H265LookAheadRateControlMedium,
  H265LookAheadRateControl'
  #-}
