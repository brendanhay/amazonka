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
        HLARCHigh,
        HLARCLow,
        HLARCMedium
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Look Ahead Rate Control
newtype H264LookAheadRateControl = H264LookAheadRateControl' Lude.Text
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

pattern HLARCHigh :: H264LookAheadRateControl
pattern HLARCHigh = H264LookAheadRateControl' "HIGH"

pattern HLARCLow :: H264LookAheadRateControl
pattern HLARCLow = H264LookAheadRateControl' "LOW"

pattern HLARCMedium :: H264LookAheadRateControl
pattern HLARCMedium = H264LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  HLARCHigh,
  HLARCLow,
  HLARCMedium,
  H264LookAheadRateControl'
  #-}
