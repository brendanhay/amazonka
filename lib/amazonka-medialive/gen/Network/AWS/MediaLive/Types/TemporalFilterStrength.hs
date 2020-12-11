-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterStrength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterStrength
  ( TemporalFilterStrength
      ( TemporalFilterStrength',
        Auto,
        Strength1,
        Strength10,
        Strength11,
        Strength12,
        Strength13,
        Strength14,
        Strength15,
        Strength16,
        Strength2,
        Strength3,
        Strength4,
        Strength5,
        Strength6,
        Strength7,
        Strength8,
        Strength9
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Temporal Filter Strength
newtype TemporalFilterStrength = TemporalFilterStrength' Lude.Text
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

pattern Auto :: TemporalFilterStrength
pattern Auto = TemporalFilterStrength' "AUTO"

pattern Strength1 :: TemporalFilterStrength
pattern Strength1 = TemporalFilterStrength' "STRENGTH_1"

pattern Strength10 :: TemporalFilterStrength
pattern Strength10 = TemporalFilterStrength' "STRENGTH_10"

pattern Strength11 :: TemporalFilterStrength
pattern Strength11 = TemporalFilterStrength' "STRENGTH_11"

pattern Strength12 :: TemporalFilterStrength
pattern Strength12 = TemporalFilterStrength' "STRENGTH_12"

pattern Strength13 :: TemporalFilterStrength
pattern Strength13 = TemporalFilterStrength' "STRENGTH_13"

pattern Strength14 :: TemporalFilterStrength
pattern Strength14 = TemporalFilterStrength' "STRENGTH_14"

pattern Strength15 :: TemporalFilterStrength
pattern Strength15 = TemporalFilterStrength' "STRENGTH_15"

pattern Strength16 :: TemporalFilterStrength
pattern Strength16 = TemporalFilterStrength' "STRENGTH_16"

pattern Strength2 :: TemporalFilterStrength
pattern Strength2 = TemporalFilterStrength' "STRENGTH_2"

pattern Strength3 :: TemporalFilterStrength
pattern Strength3 = TemporalFilterStrength' "STRENGTH_3"

pattern Strength4 :: TemporalFilterStrength
pattern Strength4 = TemporalFilterStrength' "STRENGTH_4"

pattern Strength5 :: TemporalFilterStrength
pattern Strength5 = TemporalFilterStrength' "STRENGTH_5"

pattern Strength6 :: TemporalFilterStrength
pattern Strength6 = TemporalFilterStrength' "STRENGTH_6"

pattern Strength7 :: TemporalFilterStrength
pattern Strength7 = TemporalFilterStrength' "STRENGTH_7"

pattern Strength8 :: TemporalFilterStrength
pattern Strength8 = TemporalFilterStrength' "STRENGTH_8"

pattern Strength9 :: TemporalFilterStrength
pattern Strength9 = TemporalFilterStrength' "STRENGTH_9"

{-# COMPLETE
  Auto,
  Strength1,
  Strength10,
  Strength11,
  Strength12,
  Strength13,
  Strength14,
  Strength15,
  Strength16,
  Strength2,
  Strength3,
  Strength4,
  Strength5,
  Strength6,
  Strength7,
  Strength8,
  Strength9,
  TemporalFilterStrength'
  #-}
