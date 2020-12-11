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
        High,
        Low,
        Medium
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Look Ahead Rate Control
newtype H265LookAheadRateControl = H265LookAheadRateControl' Lude.Text
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

pattern High :: H265LookAheadRateControl
pattern High = H265LookAheadRateControl' "HIGH"

pattern Low :: H265LookAheadRateControl
pattern Low = H265LookAheadRateControl' "LOW"

pattern Medium :: H265LookAheadRateControl
pattern Medium = H265LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  High,
  Low,
  Medium,
  H265LookAheadRateControl'
  #-}
