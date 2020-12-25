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
        Scte35NoRegionalBlackoutFlagRegionalBlackout,
        Scte35NoRegionalBlackoutFlagNoRegionalBlackout,
        fromScte35NoRegionalBlackoutFlag
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Corresponds to the no_regional_blackout_flag parameter. A value of REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35NoRegionalBlackoutFlag = Scte35NoRegionalBlackoutFlag'
  { fromScte35NoRegionalBlackoutFlag ::
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

pattern Scte35NoRegionalBlackoutFlagRegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlagRegionalBlackout = Scte35NoRegionalBlackoutFlag' "REGIONAL_BLACKOUT"

pattern Scte35NoRegionalBlackoutFlagNoRegionalBlackout :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlagNoRegionalBlackout = Scte35NoRegionalBlackoutFlag' "NO_REGIONAL_BLACKOUT"

{-# COMPLETE
  Scte35NoRegionalBlackoutFlagRegionalBlackout,
  Scte35NoRegionalBlackoutFlagNoRegionalBlackout,
  Scte35NoRegionalBlackoutFlag'
  #-}
