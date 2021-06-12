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
-- Module      : Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
  ( Scte35NoRegionalBlackoutFlag
      ( ..,
        Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT,
        Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Corresponds to the no_regional_blackout_flag parameter. A value of
-- REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification.
-- If you include one of the \"restriction\" flags then you must include
-- all four of them.
newtype Scte35NoRegionalBlackoutFlag = Scte35NoRegionalBlackoutFlag'
  { fromScte35NoRegionalBlackoutFlag ::
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

pattern Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT = Scte35NoRegionalBlackoutFlag' "NO_REGIONAL_BLACKOUT"

pattern Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT = Scte35NoRegionalBlackoutFlag' "REGIONAL_BLACKOUT"

{-# COMPLETE
  Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT,
  Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT,
  Scte35NoRegionalBlackoutFlag'
  #-}
