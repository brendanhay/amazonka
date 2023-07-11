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
-- Module      : Amazonka.MediaLive.Types.Scte35NoRegionalBlackoutFlag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35NoRegionalBlackoutFlag
  ( Scte35NoRegionalBlackoutFlag
      ( ..,
        Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT,
        Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to the no_regional_blackout_flag parameter. A value of
-- REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification.
-- If you include one of the \"restriction\" flags then you must include
-- all four of them.
newtype Scte35NoRegionalBlackoutFlag = Scte35NoRegionalBlackoutFlag'
  { fromScte35NoRegionalBlackoutFlag ::
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

pattern Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT = Scte35NoRegionalBlackoutFlag' "NO_REGIONAL_BLACKOUT"

pattern Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT :: Scte35NoRegionalBlackoutFlag
pattern Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT = Scte35NoRegionalBlackoutFlag' "REGIONAL_BLACKOUT"

{-# COMPLETE
  Scte35NoRegionalBlackoutFlag_NO_REGIONAL_BLACKOUT,
  Scte35NoRegionalBlackoutFlag_REGIONAL_BLACKOUT,
  Scte35NoRegionalBlackoutFlag'
  #-}
