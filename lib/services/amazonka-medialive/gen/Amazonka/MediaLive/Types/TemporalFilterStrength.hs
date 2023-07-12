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
-- Module      : Amazonka.MediaLive.Types.TemporalFilterStrength
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TemporalFilterStrength
  ( TemporalFilterStrength
      ( ..,
        TemporalFilterStrength_AUTO,
        TemporalFilterStrength_STRENGTH_1,
        TemporalFilterStrength_STRENGTH_10,
        TemporalFilterStrength_STRENGTH_11,
        TemporalFilterStrength_STRENGTH_12,
        TemporalFilterStrength_STRENGTH_13,
        TemporalFilterStrength_STRENGTH_14,
        TemporalFilterStrength_STRENGTH_15,
        TemporalFilterStrength_STRENGTH_16,
        TemporalFilterStrength_STRENGTH_2,
        TemporalFilterStrength_STRENGTH_3,
        TemporalFilterStrength_STRENGTH_4,
        TemporalFilterStrength_STRENGTH_5,
        TemporalFilterStrength_STRENGTH_6,
        TemporalFilterStrength_STRENGTH_7,
        TemporalFilterStrength_STRENGTH_8,
        TemporalFilterStrength_STRENGTH_9
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Temporal Filter Strength
newtype TemporalFilterStrength = TemporalFilterStrength'
  { fromTemporalFilterStrength ::
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

pattern TemporalFilterStrength_AUTO :: TemporalFilterStrength
pattern TemporalFilterStrength_AUTO = TemporalFilterStrength' "AUTO"

pattern TemporalFilterStrength_STRENGTH_1 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_1 = TemporalFilterStrength' "STRENGTH_1"

pattern TemporalFilterStrength_STRENGTH_10 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_10 = TemporalFilterStrength' "STRENGTH_10"

pattern TemporalFilterStrength_STRENGTH_11 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_11 = TemporalFilterStrength' "STRENGTH_11"

pattern TemporalFilterStrength_STRENGTH_12 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_12 = TemporalFilterStrength' "STRENGTH_12"

pattern TemporalFilterStrength_STRENGTH_13 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_13 = TemporalFilterStrength' "STRENGTH_13"

pattern TemporalFilterStrength_STRENGTH_14 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_14 = TemporalFilterStrength' "STRENGTH_14"

pattern TemporalFilterStrength_STRENGTH_15 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_15 = TemporalFilterStrength' "STRENGTH_15"

pattern TemporalFilterStrength_STRENGTH_16 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_16 = TemporalFilterStrength' "STRENGTH_16"

pattern TemporalFilterStrength_STRENGTH_2 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_2 = TemporalFilterStrength' "STRENGTH_2"

pattern TemporalFilterStrength_STRENGTH_3 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_3 = TemporalFilterStrength' "STRENGTH_3"

pattern TemporalFilterStrength_STRENGTH_4 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_4 = TemporalFilterStrength' "STRENGTH_4"

pattern TemporalFilterStrength_STRENGTH_5 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_5 = TemporalFilterStrength' "STRENGTH_5"

pattern TemporalFilterStrength_STRENGTH_6 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_6 = TemporalFilterStrength' "STRENGTH_6"

pattern TemporalFilterStrength_STRENGTH_7 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_7 = TemporalFilterStrength' "STRENGTH_7"

pattern TemporalFilterStrength_STRENGTH_8 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_8 = TemporalFilterStrength' "STRENGTH_8"

pattern TemporalFilterStrength_STRENGTH_9 :: TemporalFilterStrength
pattern TemporalFilterStrength_STRENGTH_9 = TemporalFilterStrength' "STRENGTH_9"

{-# COMPLETE
  TemporalFilterStrength_AUTO,
  TemporalFilterStrength_STRENGTH_1,
  TemporalFilterStrength_STRENGTH_10,
  TemporalFilterStrength_STRENGTH_11,
  TemporalFilterStrength_STRENGTH_12,
  TemporalFilterStrength_STRENGTH_13,
  TemporalFilterStrength_STRENGTH_14,
  TemporalFilterStrength_STRENGTH_15,
  TemporalFilterStrength_STRENGTH_16,
  TemporalFilterStrength_STRENGTH_2,
  TemporalFilterStrength_STRENGTH_3,
  TemporalFilterStrength_STRENGTH_4,
  TemporalFilterStrength_STRENGTH_5,
  TemporalFilterStrength_STRENGTH_6,
  TemporalFilterStrength_STRENGTH_7,
  TemporalFilterStrength_STRENGTH_8,
  TemporalFilterStrength_STRENGTH_9,
  TemporalFilterStrength'
  #-}
