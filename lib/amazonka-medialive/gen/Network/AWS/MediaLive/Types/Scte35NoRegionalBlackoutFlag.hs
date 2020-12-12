{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
  ( Scte35NoRegionalBlackoutFlag
      ( Scte35NoRegionalBlackoutFlag',
        NoRegionalBlackout,
        RegionalBlackout
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Corresponds to the no_regional_blackout_flag parameter. A value of REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35NoRegionalBlackoutFlag = Scte35NoRegionalBlackoutFlag' Lude.Text
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

pattern NoRegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern NoRegionalBlackout = Scte35NoRegionalBlackoutFlag' "NO_REGIONAL_BLACKOUT"

pattern RegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern RegionalBlackout = Scte35NoRegionalBlackoutFlag' "REGIONAL_BLACKOUT"

{-# COMPLETE
  NoRegionalBlackout,
  RegionalBlackout,
  Scte35NoRegionalBlackoutFlag'
  #-}
