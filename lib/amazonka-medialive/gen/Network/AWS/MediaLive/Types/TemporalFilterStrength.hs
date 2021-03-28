{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterStrength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TemporalFilterStrength
  ( TemporalFilterStrength
    ( TemporalFilterStrength'
    , TemporalFilterStrengthAuto
    , TemporalFilterStrengthStrength1
    , TemporalFilterStrengthStrength2
    , TemporalFilterStrengthStrength3
    , TemporalFilterStrengthStrength4
    , TemporalFilterStrengthStrength5
    , TemporalFilterStrengthStrength6
    , TemporalFilterStrengthStrength7
    , TemporalFilterStrengthStrength8
    , TemporalFilterStrengthStrength9
    , TemporalFilterStrengthStrength10
    , TemporalFilterStrengthStrength11
    , TemporalFilterStrengthStrength12
    , TemporalFilterStrengthStrength13
    , TemporalFilterStrengthStrength14
    , TemporalFilterStrengthStrength15
    , TemporalFilterStrengthStrength16
    , fromTemporalFilterStrength
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Temporal Filter Strength
newtype TemporalFilterStrength = TemporalFilterStrength'{fromTemporalFilterStrength
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern TemporalFilterStrengthAuto :: TemporalFilterStrength
pattern TemporalFilterStrengthAuto = TemporalFilterStrength' "AUTO"

pattern TemporalFilterStrengthStrength1 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength1 = TemporalFilterStrength' "STRENGTH_1"

pattern TemporalFilterStrengthStrength2 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength2 = TemporalFilterStrength' "STRENGTH_2"

pattern TemporalFilterStrengthStrength3 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength3 = TemporalFilterStrength' "STRENGTH_3"

pattern TemporalFilterStrengthStrength4 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength4 = TemporalFilterStrength' "STRENGTH_4"

pattern TemporalFilterStrengthStrength5 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength5 = TemporalFilterStrength' "STRENGTH_5"

pattern TemporalFilterStrengthStrength6 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength6 = TemporalFilterStrength' "STRENGTH_6"

pattern TemporalFilterStrengthStrength7 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength7 = TemporalFilterStrength' "STRENGTH_7"

pattern TemporalFilterStrengthStrength8 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength8 = TemporalFilterStrength' "STRENGTH_8"

pattern TemporalFilterStrengthStrength9 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength9 = TemporalFilterStrength' "STRENGTH_9"

pattern TemporalFilterStrengthStrength10 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength10 = TemporalFilterStrength' "STRENGTH_10"

pattern TemporalFilterStrengthStrength11 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength11 = TemporalFilterStrength' "STRENGTH_11"

pattern TemporalFilterStrengthStrength12 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength12 = TemporalFilterStrength' "STRENGTH_12"

pattern TemporalFilterStrengthStrength13 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength13 = TemporalFilterStrength' "STRENGTH_13"

pattern TemporalFilterStrengthStrength14 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength14 = TemporalFilterStrength' "STRENGTH_14"

pattern TemporalFilterStrengthStrength15 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength15 = TemporalFilterStrength' "STRENGTH_15"

pattern TemporalFilterStrengthStrength16 :: TemporalFilterStrength
pattern TemporalFilterStrengthStrength16 = TemporalFilterStrength' "STRENGTH_16"

{-# COMPLETE 
  TemporalFilterStrengthAuto,

  TemporalFilterStrengthStrength1,

  TemporalFilterStrengthStrength2,

  TemporalFilterStrengthStrength3,

  TemporalFilterStrengthStrength4,

  TemporalFilterStrengthStrength5,

  TemporalFilterStrengthStrength6,

  TemporalFilterStrengthStrength7,

  TemporalFilterStrengthStrength8,

  TemporalFilterStrengthStrength9,

  TemporalFilterStrengthStrength10,

  TemporalFilterStrengthStrength11,

  TemporalFilterStrengthStrength12,

  TemporalFilterStrengthStrength13,

  TemporalFilterStrengthStrength14,

  TemporalFilterStrengthStrength15,

  TemporalFilterStrengthStrength16,
  TemporalFilterStrength'
  #-}
